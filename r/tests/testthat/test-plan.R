# Plan d'actions : l'API de nemetonshiny ecrit un vrai action_plan.json dans
# un projet temporaire ; seules l'unite de gestion et la resolution du projet
# sont simulees (pas de cadastre ni de calcul).

ID <- "20261006_101500_test"

# Le registre de jetons de nemetonshiny est reel (dossier temporaire) ; sa
# garde d'ecriture (calcul en cours, verrou, projet ancien) suppose un projet
# complet : elle est remplacee par `garde`, qui ne refuse rien par defaut.
projet_test <- function(env = parent.frame(), garde = function(id) invisible(NULL)) {
  racine <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(racine, ID, "data"), recursive = TRUE)
  withr::local_options(nemeton.app_options = list(project_dir = racine),
                       nemetonmcp.utilisateur = "gest", nemetonmcp.peut_ecrire = TRUE,
                       .local_envir = env)
  withr::local_envvar(NEMETON_MCP_JETONS_DIR = file.path(racine, "jetons"), .local_envir = env)
  vrai_ns <- .ns
  testthat::local_mocked_bindings(
    .resoudre_projet = function(projet) ID,
    .ug_ids_projet = function(id) list(ids = c("u1", "u2"), ug = NULL,
                                       projet = list(metadata = list(name = "For\u00eat test"))),
    .ns = function(nom) {
      if (identical(nom, ".mcp_garde_ecriture")) return(function(id, call = NULL) garde(id))
      if (identical(nom, ".mcp_jeton_consommer")) {
        return(function(jeton, outil, projet, lien) {
          vrai_ns(".mcp_jeton_consommer")(jeton, outil, projet, lien, garde = garde)
        })
      }
      vrai_ns(nom)
    },
    .env = env)
  racine
}

lire <- function(json) jsonlite::fromJSON(json, simplifyVector = FALSE)

# Ecriture en deux temps : l'apercu (confirmation_requise, rien d'ecrit),
# puis le meme appel avec le jeton.
deux_temps <- function(f, ...) {
  r <- lire(f(...))
  testthat::expect_equal(r$code, "confirmation_requise")
  testthat::expect_true(nzchar(r$jeton))
  lire(f(..., jeton = r$jeton))
}

test_that(".normaliser_action garde les champs connus et convertit l'annee civile", {
  a <- .normaliser_action(list(ug_id = "u1", type = "eclaircie", annee = 2031, id = "pirate",
                               cree_par = "x", objectifs_lies = list("B", "C"), duree = "3"), 2026)
  expect_equal(a$annee_cible, 5L)
  expect_null(a$id)
  expect_null(a$cree_par)
  expect_equal(a$objectifs_lies, c("B", "C"))
  expect_identical(a$duree, 3L)
  expect_equal(.normaliser_action(list(annee_cible = 4), 2026)$annee_cible, 4L)
  expect_equal(.normaliser_action('{"type":"autre","type_libre":"Mare"}', 2026)$type_libre, "Mare")
})

test_that(".action_pour_vue ajoute l'annee civile, le libelle et l'origine Claude", {
  v <- .action_pour_vue(list(ug_id = "u1", annee_cible = 3L, source = list(origine = "claude")),
                        2026, c(u1 = "UG 1"))
  expect_equal(v$annee, 2029L)
  expect_equal(v$ug_label, "UG 1")
  expect_true(v$propose_par_claude)
  expect_null(.action_pour_vue(list(ug_id = "u9"), 2026, c(u1 = "UG 1"))$annee)
  # Une seule famille reste un tableau JSON.
  expect_match(.json(.action_pour_vue(list(objectifs_lies = "W"), 2026)), '"objectifs_lies":\\["W"\\]')
  expect_match(.json(.action_pour_vue(list(), 2026)), '"objectifs_lies":\\[\\]')
})

test_that("ajouter, lire, modifier et supprimer une action ecrivent le plan et son historique", {
  projet_test()
  nouvelle <- list(ug_id = "u1", type = "eclaircie", annee = 2030, priorite = "haute",
                   objectifs_lies = list("B"), source = list(origine = "claude"))
  # Premier temps : apercu, rien n'est ecrit.
  ap <- lire(ajouter_action("x", nouvelle))
  expect_false(ap$ok)
  expect_equal(ap$code, "confirmation_requise")
  expect_equal(ap$apercu$operation, "ajout")
  expect_equal(ap$apercu$annee, 2030)
  expect_length(lire(plan_actions("x", geojson = FALSE))$actions, 0)
  r <- lire(ajouter_action("x", nouvelle, jeton = ap$jeton))
  expect_true(r$ok)
  # Le jeton est a usage unique.
  expect_equal(lire(ajouter_action("x", nouvelle, jeton = ap$jeton))$code, "jeton_invalide")
  a <- r$action
  expect_equal(a$statut, "proposee")
  expect_equal(a$cree_par, "gest")
  expect_true(a$propose_par_claude)
  expect_equal(a$annee, 2030)

  p <- lire(plan_actions("x", geojson = FALSE))
  expect_length(p$actions, 1)
  # La version rendue a l'ajout est celle que relit plan_actions().
  expect_equal(p$actions[[1]]$version, a$version)
  expect_equal(p$nom, "For\u00eat test")
  expect_true(p$peut_ecrire)
  expect_true("eclaircie" %in% unlist(p$catalogue$types))
  expect_equal(p$historique[[1]]$op, "create")

  m <- deux_temps(modifier_action, "x", a$id, list(statut = "validee", annee = 2031), attendu = a$version)
  expect_true(m$ok)
  expect_false(identical(m$action$version, a$version))
  expect_equal(m$action$statut, "validee")
  expect_equal(m$action$annee, 2031)
  expect_equal(m$action$modifie_par, "gest")

  # Conflit : la vue avait lu une version plus ancienne.
  k <- lire(modifier_action("x", a$id, list(priorite = "basse"), attendu = a$version))
  expect_false(k$ok)
  expect_equal(k$classe, "nemetonmcp_conflit")
  expect_equal(k$code, "conflit")
  expect_true(nzchar(k$action))
  expect_equal(k$candidats[[1]]$statut, "validee")

  champs <- vapply(lire(plan_actions("x", geojson = FALSE))$historique, function(e) e$champ %||% "", "")
  expect_true(all(c("statut", "annee_cible") %in% champs))

  s <- deux_temps(supprimer_action, "x", m$action$id)
  expect_true(s$ok)
  p <- lire(plan_actions("x", geojson = FALSE))
  expect_length(p$actions, 0)
  expect_equal(p$historique[[1]]$op, "delete")
})

test_that("une action invalide ou un compte sans ecriture sont refuses sans rien ecrire", {
  projet_test()
  r <- lire(ajouter_action("x", list(ug_id = "u9", type = "eclaircie", annee = 2030)))
  expect_false(r$ok)
  expect_equal(r$classe, "nemetonmcp_action_invalide")
  expect_match(r$erreur, "u9")
  r <- lire(ajouter_action("x", list(ug_id = "u1", type = "coupe_magique", annee = 2030)))
  expect_equal(r$classe, "nemetonmcp_action_invalide")
  expect_equal(lire(modifier_action("x", "act_absente", list(statut = "validee")))$classe,
               "nemetonmcp_action_introuvable")
  withr::local_options(nemetonmcp.peut_ecrire = FALSE)
  expect_equal(lire(ajouter_action("x", list(ug_id = "u1", type = "eclaircie", annee = 2030)))$classe,
               "nemetonmcp_droits")
  withr::local_options(nemetonmcp.peut_ecrire = TRUE)
  expect_length(lire(plan_actions("x", geojson = FALSE))$actions, 0)
})

test_that("un projet occupe n'est pas modifie ; le refus a la confirmation garde le jeton", {
  occupe <- FALSE
  projet_test(garde = function(id) {
    if (occupe) rlang::abort("Projet en cours d'\u00e9dition dans l'application (Alice).",
                             class = c("nemetonshiny_projet_verrouille", "nemetonshiny_erreur"))
  })
  a <- list(ug_id = "u1", type = "eclaircie", annee = 2030)
  occupe <- TRUE
  r <- lire(ajouter_action("x", a))
  expect_equal(r$code, "projet_verrouille")
  expect_match(r$erreur, "Alice")
  # Apercu obtenu projet libre, confirmation refusee projet occupe : le
  # jeton reste valable et sert une fois le projet libere.
  occupe <- FALSE
  ap <- lire(ajouter_action("x", a))
  occupe <- TRUE
  expect_equal(lire(ajouter_action("x", a, jeton = ap$jeton))$code, "projet_verrouille")
  occupe <- FALSE
  expect_true(lire(ajouter_action("x", a, jeton = ap$jeton))$ok)
  expect_length(lire(plan_actions("x", geojson = FALSE))$actions, 1)
})

test_that("un jeton ne sert pas si l'action a change ou pour une autre ecriture", {
  projet_test()
  a <- deux_temps(ajouter_action, "x", list(ug_id = "u1", type = "eclaircie", annee = 2030))$action
  ap <- lire(modifier_action("x", a$id, list(priorite = "basse")))
  # Quelqu'un modifie l'action entre l'apercu et la confirmation.
  deux_temps(modifier_action, "x", a$id, list(statut = "validee"))
  expect_equal(lire(modifier_action("x", a$id, list(priorite = "basse"), jeton = ap$jeton))$code,
               "jeton_invalide")
  # Un jeton de suppression ne vaut pas pour une modification.
  sup <- lire(supprimer_action("x", a$id))
  expect_equal(lire(modifier_action("x", a$id, list(priorite = "basse"), jeton = sup$jeton))$code,
               "jeton_invalide")
  expect_length(lire(plan_actions("x", geojson = FALSE))$actions, 1)
})

test_that("profils_experts sert les profils YAML de nemetonshiny", {
  r <- lire(profils_experts("fr"))
  expect_true(r$ok)
  cles <- vapply(r$profils, `[[`, "", "cle")
  expect_true("generalist" %in% cles)
  p <- r$profils[[which(cles == "generalist")]]
  expect_true(nzchar(p$libelle))
  expect_true(nzchar(p$consigne))
})
