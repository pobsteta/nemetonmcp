test_that(".chemin_journal suit l'option, l'environnement, et se coupe", {
  withr::local_options(nemetonmcp.journal = NULL)
  withr::local_envvar(NEMETON_MCP_JOURNAL = "/tmp/j.jsonl")
  expect_equal(.chemin_journal(), "/tmp/j.jsonl")
  withr::local_envvar(NEMETON_MCP_JOURNAL = "non")
  expect_null(.chemin_journal())
  withr::local_envvar(NEMETON_MCP_JOURNAL = NA)
  expect_match(.chemin_journal(), "outils\\.jsonl$")
  withr::local_options(nemetonmcp.journal = FALSE)
  expect_null(.chemin_journal())
})

test_that(".issue_outil distingue succes et erreurs marquees", {
  expect_true(.issue_outil('{"ok":true}')$ok)
  marquee <- structure('{"ok":false,"code":"projet_introuvable"}', mcp_erreur = TRUE)
  expect_equal(.issue_outil(marquee), list(ok = FALSE, code = "projet_introuvable"))
  res <- ellmer::ContentToolResult(error = '{"ok":false,"code":"droits"}')
  expect_equal(.issue_outil(res)$code, "droits")
  expect_true(.issue_outil(ellmer::ContentToolResult(value = "x"))$ok)
  expect_equal(.issue_outil(ellmer::ContentToolResult(error = "pas du json"))$code, "erreur")
})

test_that(".journaliser garde l'outil et ecrit une ligne par appel", {
  journal <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_options(nemetonmcp.journal = journal, nemetonmcp.utilisateur = "pascal")
  f <- function(projet, langue = "fr") {
    if (identical(projet, "absent")) return(structure('{"ok":false,"code":"projet_introuvable"}', mcp_erreur = TRUE))
    if (identical(projet, "boum")) stop("boum")
    '{"ok":true}'
  }
  o <- ellmer::tool(f, name = "essai", description = "Essai.",
                    arguments = list(projet = ellmer::type_string("P."),
                                     langue = ellmer::type_string("L.", required = FALSE)),
                    annotations = ellmer::tool_annotations(read_only_hint = TRUE))
  j <- .journaliser(o)
  expect_equal(j@name, "essai")
  expect_equal(j@description, "Essai.")
  expect_true(j@annotations$read_only_hint)
  expect_equal(names(formals(j)), c("projet", "langue"))
  expect_equal(j("Couchey"), '{"ok":true}')
  expect_true(isTRUE(attr(j("absent"), "mcp_erreur")))
  expect_error(j("boum"), "boum")
  lignes <- lapply(readLines(journal), jsonlite::fromJSON)
  expect_length(lignes, 3)
  expect_equal(vapply(lignes, `[[`, "", "outil"), rep("essai", 3))
  expect_equal(lignes[[1]]$projet, "Couchey")
  expect_equal(lignes[[1]]$utilisateur, "pascal")
  expect_equal(lignes[[1]]$transport, "stdio")
  expect_true(lignes[[1]]$ok)
  expect_null(lignes[[1]]$code)
  expect_false(lignes[[2]]$ok)
  expect_equal(lignes[[2]]$code, "projet_introuvable")
  expect_equal(lignes[[3]]$code, "exception")
  expect_true(is.numeric(lignes[[1]]$duree_ms))
})

test_that("un journal coupe ou impossible n'empeche pas l'outil", {
  o <- ellmer::tool(function() "x", name = "sans_arg", description = "D.", arguments = list())
  withr::local_options(nemetonmcp.journal = "non")
  expect_equal(.journaliser(o)(), "x")
  withr::local_options(nemetonmcp.journal = "/proc/interdit/outils.jsonl")
  expect_equal(.journaliser(o)(), "x")
})

test_that("le journal tourne au-dela de sa taille maximale", {
  journal <- withr::local_tempfile(fileext = ".jsonl")
  writeLines(strrep("x", 100), journal)
  local_mocked_bindings(.JOURNAL_MAX_OCTETS = 10)
  .ecrire_journal(list(outil = "a", duree_ms = 1, ok = TRUE), journal)
  expect_true(file.exists(paste0(journal, ".1")))
  expect_length(readLines(journal), 1)
})

test_that("journal_outils resume par outil, filtre par date et utilisateur", {
  journal <- withr::local_tempfile(fileext = ".jsonl")
  l <- function(ts, outil, d, ok = TRUE, u = "a") .json(list(ts = ts, outil = outil, duree_ms = d, ok = ok, utilisateur = u))
  writeLines(c(l("2026-11-14T10:00:00+0100", "vue_atlas", 3000),
               l("2026-11-15T10:00:00+0100", "vue_atlas", 1000, FALSE),
               l("2026-11-15T10:01:00+0100", "vue_atlas", 2000),
               l("2026-11-15T10:02:00+0100", "lister_projets", 50, u = "b"),
               "pas du json", ""), journal)
  r <- journal_outils(journal)
  expect_equal(r$outil, c("vue_atlas", "lister_projets"))
  expect_equal(r$appels, c(3L, 1L))
  expect_equal(r$erreurs, c(1L, 0L))
  expect_equal(r$mediane_ms, c(2000, 50))
  expect_equal(r$max_ms, c(3000, 50))
  expect_equal(journal_outils(journal, depuis = "2026-11-15")$appels, c(2L, 1L))
  expect_equal(journal_outils(journal, utilisateur = "b")$outil, "lister_projets")
  expect_equal(nrow(journal_outils(tempfile())), 0)
})

test_that("outils_mcp journalise tous les outils", {
  journal <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_options(nemetonmcp.journal = journal)
  o <- outils_mcp()
  i <- which(.noms_outils(o) == "profils_experts")
  o[[i]]()
  expect_equal(jsonlite::fromJSON(readLines(journal)[1])$outil, "profils_experts")
})
