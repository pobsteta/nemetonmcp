test_that(".code_profil_corpus suit la correspondance de nemetonshiny", {
  expect_equal(.code_profil_corpus("profil_naturaliste"), "naturaliste")
  expect_equal(.code_profil_corpus("owner"), "proprietaire_prive")
  expect_equal(.code_profil_corpus("elu"), "elu")
  expect_null(.code_profil_corpus(NULL))
  expect_null(.code_profil_corpus(""))
})

passages_fictifs <- data.frame(
  chunk_id = 1:4, document_id = c("d1", "d2", "d1", "d3"),
  title = c("Guide chênaie", "Hêtraie et climat", "Guide chênaie", NA),
  author = c("ONF", "INRAE", "ONF", NA), pub_date = c("2020-01-01", NA, "2020-01-01", NA),
  source_url = c("https://exemple.org/g", NA, "https://exemple.org/g", NA), lang = "fr",
  chunk_index = 1:4, page_number = c(3L, NA, 7L, NA),
  text = c("Le chêne sessile résiste.", strrep("x", 800), "Éclaircir tôt.", "Autre"),
  similarity = c(0.9, 0.8, 0.85, 0.6), stringsAsFactors = FALSE)

test_that(".documents_corpus regroupe par document, garde l'ordre et tronque", {
  d <- .documents_corpus(passages_fictifs, n = 2)
  expect_length(d, 2)
  expect_equal(vapply(d, `[[`, "", "document_id"), c("d1", "d2"))
  expect_equal(d[[1]]$ref, 1L)
  expect_equal(d[[1]]$titre, "Guide chênaie")
  expect_equal(d[[1]]$similarite, 0.9)
  expect_equal(vapply(d[[1]]$extraits, `[[`, "", "texte"), c("Le chêne sessile résiste.", "Éclaircir tôt."))
  expect_equal(d[[1]]$extraits[[2]]$page, 7L)
  expect_null(d[[2]]$date)
  expect_null(d[[2]]$url)
  expect_equal(nchar(d[[2]]$extraits[[1]]$texte), .EXTRAIT_MAX + 1L)
  expect_length(.documents_corpus(passages_fictifs[0, ]), 0)
})

test_that("chercher_connaissances dit quand le corpus est indisponible", {
  withr::local_envvar(NEMETON_KNOWLEDGE_DB_URL = NA, NEMETON_DB_URL = NA)
  r <- chercher_connaissances("chênaie et sécheresse")
  expect_true(isTRUE(attr(r, "mcp_erreur")))
  x <- jsonlite::fromJSON(r)
  expect_equal(x$code, "connaissances_indisponibles")
  expect_match(x$erreur, "NEMETON_KNOWLEDGE_DB_URL")
  expect_equal(jsonlite::fromJSON(chercher_connaissances("  "))$code, "question_vide")
})

test_that("chercher_connaissances transmet filtres et seuil, puis met en forme", {
  recu <- NULL
  local_mocked_bindings(
    .connexion_corpus = function() structure(list(), class = "faux_con"),
    .recuperer_connaissances = function(con, ...) { recu <<- list(...); passages_fictifs })
  local_mocked_bindings(db_disconnect = function(con) invisible(TRUE), .package = "nemeton")
  x <- jsonlite::fromJSON(chercher_connaissances("Quelle essence ?", familles = "b, C1", profil = "profil_naturaliste", n = 2),
                          simplifyVector = FALSE)
  expect_true(x$ok)
  expect_equal(recu$top_k, 6L)
  expect_equal(recu$family_codes, c("B", "C1"))
  expect_equal(recu$profile_codes, "naturaliste")
  expect_equal(recu$min_similarity, .SEUIL_SIMILARITE)
  expect_equal(x$n_documents, 2)
  expect_match(x$citations, "Guide ch")
  expect_match(x$consigne, "^Citer")

  local_mocked_bindings(.recuperer_connaissances = function(con, ...) stop("NEMETON_MISTRAL_API_KEY absente"))
  e <- jsonlite::fromJSON(chercher_connaissances("Quelle essence ?"))
  expect_equal(e$code, "connaissances_indisponibles")
  expect_match(e$erreur, "MISTRAL")
})

test_that("chercher_connaissances interroge un vrai corpus SQLite du coeur", {
  skip_if_not_installed("RSQLite")
  base <- withr::local_tempfile(fileext = ".sqlite")
  # Embedding simule (pas d'API) : un vecteur par mot-cle, meme modele que
  # le corpus (mistral-embed), pour exercer la vraie recherche du coeur.
  vecteur <- function(t) {
    t <- tolower(t)
    v <- c(grepl("chêne|chene", t), grepl("hêtre|hetre", t), grepl("pin", t), 0.05)
    as.numeric(v) / sqrt(sum(as.numeric(v)^2))
  }
  local_mocked_bindings(.mistral_embed = function(texts, api_key) do.call(rbind, lapply(texts, vecteur)), .package = "nemeton")
  withr::local_envvar(NEMETON_MISTRAL_API_KEY = "factice", NEMETON_KNOWLEDGE_DB_URL = paste0("sqlite:///", base))
  con <- nemeton::db_connect(paste0("sqlite:///", base))
  nemeton::enable_rag(con)
  for (d in list(list(t = "Le chêne sessile supporte la sécheresse estivale.", titre = "Guide du chêne", f = "B"),
                 list(t = "Le hêtre souffre des canicules répétées.", titre = "Hêtraie", f = "R"))) {
    f <- withr::local_tempfile(fileext = ".md")
    writeLines(d$t, f)
    nemeton::ingest_knowledge_document(con, f, metadata = list(title = d$titre, lang = "fr", doc_type = "guide", family_codes = d$f))
  }
  nemeton::db_disconnect(con)
  x <- jsonlite::fromJSON(chercher_connaissances("Le chêne face à la sécheresse ?"), simplifyVector = FALSE)
  expect_true(x$ok)
  expect_equal(x$documents[[1]]$titre, "Guide du chêne")
  expect_match(x$documents[[1]]$extraits[[1]]$texte, "sessile")
  expect_true(nzchar(x$citations))
  vide <- jsonlite::fromJSON(chercher_connaissances("Le chêne ?", familles = "R"), simplifyVector = FALSE)
  expect_equal(vide$n_documents, 0)
  expect_match(vide$consigne, "^Aucun passage")
})
