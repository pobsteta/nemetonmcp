# Outils MCP : forme des erreurs, etat_calcul, noms et annotations, contrat
# des objets internes de nemetonshiny (briefs nemetonshiny des 2026-11-10 et
# 2026-11-13).

test_that(".mcp_call rend la forme d'erreur de nemetonshiny, marquee mcp_erreur", {
  x <- .mcp_call(.abort("Commune {.val X} introuvable.", "nemetonmcp_commune_introuvable"))
  expect_true(isTRUE(attr(x, "mcp_erreur")))
  e <- jsonlite::fromJSON(x)
  expect_false(e$ok)
  expect_equal(e$classe, "nemetonmcp_commune_introuvable")
  expect_equal(e$code, "commune_introuvable")
  expect_equal(e$langue, "fr")
  expect_true(nzchar(e$action))
  # Code aligne sur nemetonshiny, phrase propre a nemetonmcp, en anglais.
  y <- jsonlite::fromJSON(.mcp_call(langue = "en", rlang::abort("Occupe.",
    class = c("nemetonshiny_calcul_en_cours", "nemetonshiny_erreur"))))
  expect_equal(y$code, "calcul_en_cours")
  expect_equal(y$langue, "en")
  expect_match(y$action, "etat_calcul")
  # Erreur R sans classe connue.
  z <- jsonlite::fromJSON(.mcp_call(stop("boum")))
  expect_equal(z$code, "erreur_inconnue")
  expect_equal(z$erreur, "boum")
  # Un succes n'est pas marque.
  expect_null(attr(.mcp_call(list(a = 1)), "mcp_erreur"))
})

test_that("une confirmation porte projet, jeton, apercu et echeance", {
  x <- jsonlite::fromJSON(.mcp_call(rlang::abort("Confirmation requise.",
    class = c("nemetonshiny_confirmation_requise", "nemetonshiny_erreur"),
    projet = "p1", projet_nom = "Couchey", jeton = "abc", apercu = list(n = 1),
    expire_a = "2026-11-13T10:00:00Z")), simplifyVector = FALSE)
  expect_equal(x$code, "confirmation_requise")
  expect_equal(x$projet, "p1")
  expect_equal(x$projet_nom, "Couchey")
  expect_equal(x$jeton, "abc")
  expect_equal(x$apercu$n, 1)
  expect_equal(x$expire_a, "2026-11-13T10:00:00Z")
})

test_that("etat_calcul : un job en echec garde ses durees, une erreur d'outil reste marquee", {
  t0 <- as.POSIXct("2026-11-13 10:00:00")
  echec <- .json(list(ok = FALSE, statut = "echec", type = "rapport", code = "erreur",
                      lance_a = "2026-11-13T09:58:00", fin_a = "2026-11-13T09:59:30"))
  r <- .etat_avec_durees(echec, maintenant = t0)
  expect_null(attr(r, "mcp_erreur"))
  r <- jsonlite::fromJSON(r)
  expect_equal(r$statut, "echec")
  expect_equal(r$ecoule_s, 90)
  outil <- structure(.json(list(ok = FALSE, code = "projet_introuvable")), mcp_erreur = TRUE)
  expect_identical(.etat_avec_durees(outil), outil)
  en_cours <- jsonlite::fromJSON(.etat_avec_durees(.json(list(ok = TRUE, statut = "en_cours", type = "calcul",
    lance_a = "2026-11-13T09:59:00")), maintenant = t0))
  expect_equal(en_cours$ecoule_s, 60)
})

test_that(".marquer garde la signature et marque les erreurs isError", {
  f <- function(projet, langue = "fr") .mcp_call(.abort("Non.", "nemetonmcp_droits"))
  g <- .marquer(f)
  expect_identical(formals(g), formals(f))
  local_mocked_bindings(.vers_outil = function(x) if (isTRUE(attr(x, "mcp_erreur"))) "MARQUE" else x)
  expect_equal(g("p"), "MARQUE")
  ok <- .marquer(function(projet) .mcp_call(list(a = 1)))
  expect_equal(jsonlite::fromJSON(ok("p"))$a, 1)
})

test_that("les outils ont des noms uniques ; les notres remplacent les homonymes", {
  outil <- function(nom, lecture = TRUE) {
    ellmer::tool(function() "", name = nom, description = nom, arguments = list(),
                 annotations = ellmer::tool_annotations(read_only_hint = lecture))
  }
  fusion <- .fusionner_outils(list(outil("a"), outil("b"), outil("c")), list(outil("b", FALSE), outil("d")))
  expect_equal(sort(.noms_outils(fusion)), c("a", "b", "c", "d"))
  expect_false(fusion[[which(.noms_outils(fusion) == "b")]]@annotations$read_only_hint)

  skip_if_not(exists(".mcp_vers_outil", asNamespace("nemetonshiny"), inherits = FALSE),
              "nemetonshiny sans .mcp_vers_outil (avant 2026.11.1)")
  o <- outils_mcp()
  noms <- .noms_outils(o)
  expect_false(anyDuplicated(noms) > 0L)
  expect_true(all(c("etat_calcul", "exporter_gpkg", "vue_rasters", "ajouter_action") %in% noms))
  ann <- function(n) o[[which(noms == n)]]@annotations
  # Annotations de nemetonshiny gardees entieres.
  expect_true(ann("resume_projet")$read_only_hint)
  expect_false(ann("resume_projet")$destructive_hint)
  expect_true(ann("resume_projet")$idempotent_hint)
  # Les notres, completes.
  expect_true(ann("etat_calcul")$read_only_hint)
  expect_true(ann("ajouter_action")$destructive_hint)
  expect_false(ann("ajouter_action")$idempotent_hint)
  expect_false(ann("exporter_gpkg")$destructive_hint)
  # Les ecritures du plan prennent un jeton.
  args_outil <- function(n) names(o[[which(noms == n)]]@arguments@properties)
  for (n in c("ajouter_action", "modifier_action", "supprimer_action")) expect_true("jeton" %in% args_outil(n))
})

test_that(".environnement_serveur pose l'heure de Paris et une locale UTF-8", {
  withr::local_envvar(TZ = "UTC")
  withr::local_locale(c(LC_CTYPE = Sys.getlocale("LC_CTYPE")))
  .environnement_serveur()
  expect_equal(Sys.getenv("TZ"), "Europe/Paris")
  expect_true(isTRUE(l10n_info()[["UTF-8"]]))
})

# Contrat (CONTRAT.md de nemetonshiny, section 6 bis) : les objets internes
# que nemetonmcp peut lire par .ns(). Un appel hors de cette liste n'est pas
# garanti : le test le refuse. La liste change avec un brief de nemetonshiny.
CONTRAT_NEMETONSHINY <- c(
  "load_action_plan", "save_action_plan", "add_action_to_plan", "update_action_in_plan",
  "delete_action_from_plan", "get_action_by_id", "find_action_index", "action_plan_annee_base",
  "load_ug_comments", "ACTION_PLAN_TYPES", "ACTION_PLAN_STATUTS", "ACTION_PLAN_PRIORITES",
  "ACTION_PLAN_FAMILY_CODES", "ACTION_PLAN_TRANSITIONS",
  "load_project", "load_parcels", "get_project_path", "get_projects_root", "ug_build_sf", "lock_status",
  "get_commune_geometry", "save_commune_geometry", "get_departments",
  "mcp_tools", "mcp_etat_calcul", "mcp_exporter_gpkg", ".mcp_resolve_project", ".mcp_vers_outil",
  ".mcp_garde_ecriture", ".mcp_jeton_emettre", ".mcp_jeton_consommer",
  "get_expert_profiles", "marculus_export_bundle", "MARCULUS_CONTEXT_ACTION_TYPES", "INDICATOR_FAMILIES")

test_that("nemetonmcp n'appelle aucun objet interne hors contrat", {
  expect_length(CONTRAT_NEMETONSHINY, 35L)
  dossier <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(dossier), "sources R absentes (paquet installe)")
  code <- unlist(lapply(list.files(dossier, pattern = "\\.R$", full.names = TRUE), readLines, warn = FALSE))
  appels <- unique(unlist(regmatches(code, gregexpr('\\.ns\\("[^"]+"\\)', code))))
  appels <- sub('^\\.ns\\("(.*)"\\)$', "\\1", appels)
  expect_true(length(appels) > 0)
  expect_equal(setdiff(appels, CONTRAT_NEMETONSHINY), character())
})
