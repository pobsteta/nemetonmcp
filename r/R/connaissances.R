# Outil `chercher_connaissances(question)` : passages du corpus documentaire
# de nemeton (RAG), pour que Claude cite ses sources
# (specs/ARCHITECTURE-nemeton-llm.md, « Connaissances (RAG) : Conversation »).
#
# Recherche du coeur (`nemeton::retrieve_knowledge()` : embedding de la
# question, similarite cosinus pgvector ou SQLite), sur la base du corpus
# que lit nemetonshiny : NEMETON_KNOWLEDGE_DB_URL, a defaut NEMETON_DB_URL.
# L'embedding de la question passe par le fournisseur choisi a l'ingestion
# (NEMETON_MCP_RAG_EMBED, `mistral` par defaut) et sa cle
# (NEMETON_MISTRAL_API_KEY...). Sans base ou sans cle, l'outil le dit
# (`connaissances_indisponibles`) au lieu de repondre sans sources.

.SEUIL_SIMILARITE <- 0.55
.EXTRAIT_MAX <- 700L

# Profils de l'application (`profil_<code>`) vers les codes du corpus : meme
# correspondance que nemetonshiny (rag_profile_code). Pure, testee.
.ALIAS_PROFILS_CORPUS <- c(owner = "proprietaire_prive")
.code_profil_corpus <- function(profil) {
  if (is.null(profil) || !length(profil)) return(NULL)
  k <- sub("^profil_", "", trimws(as.character(profil[[1]])))
  if (is.na(k) || !nzchar(k)) return(NULL)
  if (k %in% names(.ALIAS_PROFILS_CORPUS)) unname(.ALIAS_PROFILS_CORPUS[k]) else k
}

.indisponible <- function(message) {
  .abort(c("Corpus documentaire indisponible.", i = message), "nemetonmcp_connaissances_indisponibles")
}

# Connexion en lecture a la base du corpus, ou erreur.
.connexion_corpus <- function() {
  url <- Sys.getenv("NEMETON_KNOWLEDGE_DB_URL", "")
  if (!nzchar(url)) url <- Sys.getenv("NEMETON_DB_URL", "")
  if (!nzchar(url)) .indisponible("Ni NEMETON_KNOWLEDGE_DB_URL ni NEMETON_DB_URL ne sont d\u00e9finies sur ce serveur.")
  tryCatch(nemeton::db_connect(url, read_only = TRUE),
           error = function(e) .indisponible(paste("Connexion refus\u00e9e :", conditionMessage(e))))
}

# Passages regroupes par document, dans l'ordre de pertinence (le meilleur
# passage de chaque document fixe son rang), `n` documents au plus. Pure,
# testee.
.documents_corpus <- function(passages, n = 5L) {
  if (is.null(passages) || !nrow(passages)) return(list())
  passages <- passages[order(-passages$similarity), , drop = FALSE]
  docs <- unique(passages$document_id)[seq_len(min(n, length(unique(passages$document_id))))]
  champ <- function(df, cc, i) if (cc %in% names(df)) { v <- df[[cc]][i]; if (length(v) && !is.na(v) && nzchar(as.character(v))) v }
  lapply(seq_along(docs), function(d) {
    p <- passages[passages$document_id == docs[d], , drop = FALSE]
    Filter(Negate(is.null), list(
      ref = d, document_id = docs[d], titre = champ(p, "title", 1), auteur = champ(p, "author", 1),
      date = if (!is.null(champ(p, "pub_date", 1))) as.character(champ(p, "pub_date", 1)),
      url = champ(p, "source_url", 1), langue = champ(p, "lang", 1),
      similarite = round(max(p$similarity), 3),
      extraits = lapply(seq_len(nrow(p)), function(i) Filter(Negate(is.null), list(
        page = champ(p, "page_number", i),
        texte = { x <- as.character(p$text[i]); if (nchar(x) > .EXTRAIT_MAX) paste0(substr(x, 1, .EXTRAIT_MAX), "\u2026") else x })))
    ))
  })
}

#' Search the nemeton knowledge corpus
#'
#' Passages of the documentary corpus (RAG) closest to a question, grouped
#' by document, with the citation block to quote, so that answers cite their
#' sources. Retrieval is the core's (`nemeton::retrieve_knowledge()`), on the
#' corpus database nemetonshiny reads (`NEMETON_KNOWLEDGE_DB_URL`, else
#' `NEMETON_DB_URL`); the question is embedded with the provider used at
#' ingestion (`NEMETON_MCP_RAG_EMBED`, default `"mistral"`) and its key.
#' Without a corpus or a key: error `connaissances_indisponibles`.
#'
#' @param question Natural-language question.
#' @param familles Optional family or indicator codes (`"B"`, `"C1"`...):
#'   only documents tagged with one of them.
#' @param profil Optional expert profile (`profils_experts`), e.g.
#'   `"naturaliste"`: only documents tagged for it.
#' @param n Documents to return (1 to 10, default 5).
#' @param langue `"fr"` or `"en"` (citation block; documents of every
#'   language are searched).
#' @return JSON: `n_documents`, `documents` (ref, title, author, date, url,
#'   similarity, excerpts with page), `citations` (markdown block, numbered
#'   like `ref`), `seuil`.
#' @export
chercher_connaissances <- function(question, familles = NULL, profil = NULL, n = 5, langue = "fr") {
  .mcp_call(langue = langue, {
    langue <- if (identical(langue, "en")) "en" else "fr"
    question <- trimws(as.character(question %||% ""))
    if (!nzchar(question)) .abort("Question vide.", "nemetonmcp_question_vide")
    n <- max(1L, min(10L, as.integer(n %||% 5L)))
    familles <- unique(toupper(trimws(unlist(strsplit(as.character(unlist(familles %||% character())), "[,;[:space:]]+")))))
    familles <- familles[nzchar(familles)]
    passages <- .interroger_corpus(question, top_k = 3L * n, familles = familles,
                                   profil = .code_profil_corpus(profil))
    docs <- .documents_corpus(passages, n)
    meilleurs <- if (length(docs)) {
      p <- passages[order(-passages$similarity), , drop = FALSE]
      p[!duplicated(p$document_id), , drop = FALSE][seq_along(docs), , drop = FALSE]
    }
    citations <- if (!is.null(meilleurs)) tryCatch(
      nemeton::format_citations(meilleurs, format = "markdown", lang = langue), error = function(e) NULL)
    list(question = question, familles = if (length(familles)) familles,
         profil = .code_profil_corpus(profil), n_documents = length(docs),
         documents = docs, citations = citations, seuil = .SEUIL_SIMILARITE,
         consigne = if (length(docs)) "Citer les documents par leur ref [^n] ; ne rien affirmer que les extraits ne disent pas." else
           "Aucun passage au-dessus du seuil : r\u00e9pondre sans citer, en le disant, ou reformuler la question.")
  })
}

# Ouvre la base du corpus, cherche, referme. Une erreur de la recherche
# (cle d'embedding absente, schema RAG non active...) devient
# `connaissances_indisponibles`.
.interroger_corpus <- function(question, top_k, familles = character(), profil = NULL) {
  con <- .connexion_corpus()
  on.exit(try(nemeton::db_disconnect(con), silent = TRUE), add = TRUE)
  tryCatch(
    .recuperer_connaissances(con, query = question, top_k = top_k,
                             family_codes = if (length(familles)) familles,
                             profile_codes = profil,
                             min_similarity = .SEUIL_SIMILARITE, lang = NULL,
                             embed_provider = .env_mcp("RAG_EMBED", "mistral")),
    error = function(e) .indisponible(conditionMessage(e)))
}

# Point d'acces unique a la recherche du coeur (remplace dans les tests).
.recuperer_connaissances <- function(...) nemeton::retrieve_knowledge(...)
