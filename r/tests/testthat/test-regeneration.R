test_that(".choisir_ug trouve une UGF par id, nom exact ou partiel, sinon des candidats", {
  ids <- c("ug_1", "ug_2", "ug_3")
  labels <- c("UGF 1", "Forêt de Couchey — parcelle 50", "Forêt de Couchey — parcelle 5")
  expect_equal(.choisir_ug(NULL, ids, labels), 1:3)
  expect_equal(.choisir_ug("ug_2", ids, labels), 2L)
  expect_equal(.choisir_ug("ugf 1", ids, labels), 1L)
  expect_equal(.choisir_ug("foret de couchey — parcelle 5", ids, labels), 3L)
  expect_equal(.choisir_ug("parcelle 50", ids, labels), 2L)
  e <- tryCatch(.choisir_ug("couchey", ids, labels), error = function(e) e)
  expect_s3_class(e, "nemetonmcp_ug_introuvable")
  expect_length(e$candidats, 2)
  e <- tryCatch(.choisir_ug("inconnue", ids, labels), error = function(e) e)
  expect_length(e$candidats, 3)
})

rang_fictif <- data.frame(
  ug_id = c("a", "a", "b", "b", "c"), rank = c(1L, 2L, 1L, 2L, NA),
  species_code = c("qp", "fs", "qp", "ps", NA), label = c("Chêne sessile", "Hêtre", "Chêne sessile", "Pin sylvestre", NA),
  type = c("feuillu", "feuillu", "feuillu", "conifere", NA), suitability = c(81.24, 70, 77, 60, NA),
  limiting_factor = c("secheresse", "gel", "secheresse", "chaleur", NA), confidence = c("eleve", "eleve", "moyen", "moyen", NA),
  invasif = FALSE, stringsAsFactors = FALSE)
ugs_fictives <- data.frame(ug_id = c("a", "b", "c"), label = c("UGF 1", "UGF 2", "UGF 3"),
                           priorite = c(TRUE, FALSE, NA), tmax_moyenne = c(30, 31, NA), d_tmax = c(1.5, -0.5, NA),
                           rew_min = c(0.1, 0.3, NA), stringsAsFactors = FALSE)

test_that(".synthese_essences resume par UGF et pour le projet", {
  s <- .synthese_essences(rang_fictif, ugs_fictives, "Dabo")
  expect_length(s$ugf, 3)
  a <- s$ugf[[1]]
  expect_true(a$priorite)
  expect_equal(a$tmax_ete, 31.5)
  expect_equal(vapply(a$essences, `[[`, "", "nom"), c("Chêne sessile", "Hêtre"))
  expect_equal(a$essences[[1]]$aptitude, 81.2)
  expect_length(s$ugf[[3]]$essences, 0)
  expect_equal(s$tete[[1]], list(nom = "Chêne sessile", ugf = 2L))
  expect_equal(s$facteurs$secheresse, 2L)
  expect_equal(s$texte, "Dabo : essences les mieux adaptées sur 3 UGF : Chêne sessile (en tête dans 2 UGF). Facteur limitant le plus fréquent : la sécheresse.")
  une <- .synthese_essences(rang_fictif[rang_fictif$ug_id == "a", ], ugs_fictives[1, ], "Dabo")
  expect_equal(une$texte, "Dabo, UGF 1 : Chêne sessile (81/100, limité par la sécheresse), Hêtre (70/100, limité par le gel tardif).")
  en <- .synthese_essences(rang_fictif, ugs_fictives, "Dabo", "en")
  expect_match(en$texte, "^Dabo: best-suited species over 3 units: Chêne sessile \\(first in 2 units\\)\\. Most frequent limiting factor: drought\\.$")
})

test_that(".unites_regen joint les sorties du cache par ug_id", {
  d <- withr::local_tempdir()
  expect_null(.unites_regen(d))
  carre <- function(x) sf::st_polygon(list(rbind(c(x, 6685000), c(x + 100, 6685000), c(x + 100, 6685100),
                                                 c(x, 6685100), c(x, 6685000))))
  g <- sf::st_sfc(carre(843000), carre(843100), crs = 2154)
  sf::st_write(sf::st_sf(ug_id = c("a", "b"), tmax_moyenne = c(30, 31), d_tmax = c(1, 2), vpd_canicule = c(2, 3),
                         autre = 1, geometry = g), file.path(d, "sensibilite.gpkg"), quiet = TRUE)
  sf::st_write(sf::st_sf(ug_id = c("b", "a"), rew_min = c(0.3, 0.1), geometry = g), file.path(d, "biljou.gpkg"), quiet = TRUE)
  u <- .unites_regen(d)
  expect_equal(u$ug_id, c("a", "b"))
  expect_equal(u$rew_min, c(0.1, 0.3))
  expect_false("autre" %in% names(u))
  expect_false("r7_gel_days" %in% names(u))
  rk <- nemeton::regen_rank_species(u, top_n = 2, region = "BFC")
  expect_true(all(c("ug_id", "rank", "label", "suitability", "limiting_factor") %in% names(rk)))
  expect_false(anyNA(rk$rank))
})

traits_fictifs <- data.frame(code = c("pa", "qi", "fs", "qp"), frost_winter_min_c = c(-12, -15, -30, -30),
                             thermophily = c(9, 8, 4, 5), stringsAsFactors = FALSE)
rang_long <- data.frame(
  ug_id = c("a", "a", "a", "a", "b"), rank = c(1L, 2L, 3L, 4L, NA),
  species_code = c("pa", "qi", "fs", "qp", NA), label = c("Pin d'Alep", "Chêne vert", "Hêtre", "Chêne sessile", NA),
  type = NA, suitability = c(97, 49, 38, 37, NA), limiting_factor = c("secheresse", "chaleur", "chaleur", "chaleur", NA),
  confidence = "eleve", invasif = FALSE, stringsAsFactors = FALSE)

test_that(".filtrer_froid ecarte les essences gelives, reclasse et garde la meilleure non mediterraneenne", {
  f <- .filtrer_froid(rang_long, traits_fictifs, -15, 2)
  a <- f$rang[f$rang$ug_id == "a", ]
  expect_equal(a$species_code, c("qi", "fs"))
  expect_equal(a$rank, 1:2)
  expect_equal(f$ecartees$species_code, "pa")
  expect_equal(f$ecartees$tenue_froid_c, -12)
  expect_equal(f$locale$species_code, "fs")
  b <- f$rang[f$rang$ug_id == "b", ]
  expect_equal(nrow(b), 1L)
  expect_true(is.na(b$rank))
  # Seuil plus froid observe : le chene vert (-15) tombe aussi.
  f2 <- .filtrer_froid(rang_long, traits_fictifs, -18, 2)
  expect_equal(f2$rang$species_code[f2$rang$ug_id == "a"], c("fs", "qp"))
  expect_equal(f2$ecartees$species_code, c("pa", "qi"))
})

test_that(".synthese_essences dit le froid, la base climatique et l'essence non mediterraneenne", {
  f <- .filtrer_froid(rang_long, traits_fictifs, -15, 1)
  ugs <- data.frame(ug_id = "a", label = "UGF 1", stringsAsFactors = FALSE)
  climat <- list(annees_ete = c(2022L, 2023L), annee_canicule = 2022L)
  s <- .synthese_essences(f$rang[f$rang$ug_id == "a", ], ugs, "Couchey", "fr", f$ecartees, f$locale, climat, -15)
  expect_equal(s$ugf[[1]]$ecartees_froid[[1]]$nom, "Pin d'Alep")
  expect_equal(s$ugf[[1]]$non_mediterraneenne$nom, "Hêtre")
  expect_equal(s$texte, paste0("Couchey, UGF 1 : Chêne vert (49/100, limité par la chaleur).",
    " Écartées pour le froid d'hiver (minimum décennal -15 °C) : Pin d'Alep (tient -12 °C).",
    " Meilleure essence non méditerranéenne : Hêtre (38/100).",
    " Station : climat observé (étés 2022-2023, Tmax sous couvert de l'été 2022, retenu comme caniculaire), pas une projection."))
  inconnu <- .synthese_essences(f$rang[f$rang$ug_id == "a", ], ugs, "Couchey", climat = list(annees_ete = integer()))
  expect_match(inconnu$texte, "climat observé récent, pas une projection\\.$")
  expect_match(s$methode, "pas une projection climatique")
})

test_that(".climat_regen lit les annees d'ete et du gel tardif dans le cache", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "microclimf")); dir.create(file.path(d, "meteoland"))
  r <- terra::rast(nrows = 2, ncols = 2, vals = 1)
  terra::writeRaster(r * 43, file.path(d, "microclimf", "cache_2023_abc_tmax.tif"))
  terra::writeRaster(r * 38, file.path(d, "microclimf", "cache_2022_abc_tmax.tif"))
  file.create(file.path(d, "meteoland", "tmin_1_2_3_4__y2022-2023__doy60-180.tif"))
  u <- data.frame(tmax_moyenne = c(43, 44), d_tmax = c(-5, -6))
  c1 <- .climat_regen(d, u)
  expect_equal(c1$annees_ete, c(2022L, 2023L))
  expect_equal(c1$annee_canicule, 2022L)
  expect_equal(c1$annees_gel_tardif, c(2022L, 2023L))
  expect_null(c1$tmin_hiver_c)
  expect_false(c1$projection)
})

test_that(".unites_regen met NA, pas 0, au gel tardif non calcule", {
  d <- withr::local_tempdir()
  g <- sf::st_sfc(sf::st_point(c(843000, 6685000)), sf::st_point(c(843100, 6685000)), crs = 2154)
  sf::st_write(sf::st_sf(ug_id = c("a", "b"), tmax_moyenne = 30, geometry = g), file.path(d, "sensibilite.gpkg"), quiet = TRUE)
  sf::st_write(sf::st_sf(ug_id = c("a", "b"), r7_gel_days = c(0, 2), r7_status = c("skipped_no_tmin", "calculated"),
                         geometry = g), file.path(d, "r7.gpkg"), quiet = TRUE)
  u <- .unites_regen(d)
  expect_true(is.na(u$r7_gel_days[1]))
  expect_equal(u$r7_gel_days[2], 2)
})

test_that(".statut_mfr suit l'annexe 1.1 de l'arrete MFR de Bourgogne-Franche-Comte", {
  expect_equal(.statut_mfr(c("quercus_pubescens", "pinus_halepensis", "pinus_canariensis", "quercus_ilex",
                             "cedrus_atlantica", "pinus_nigra", "pinus_pinea")),
               c("objectif", "accompagnement", "hors_liste", "hors_liste", "objectif", "objectif", "accompagnement"))
  expect_length(intersect(.MFR_BFC$objectif, .MFR_BFC$accompagnement), 0)
  # Tous les codes existent dans la table des essences du coeur.
  eu <- nemeton::european_species_tolerances()
  expect_true(all(c(.MFR_BFC$objectif, .MFR_BFC$accompagnement) %in% c(eu$code, "abies_nordmanniana")))
})

test_that(".filtrer_froid avec aides ne classe que les essences objectif et liste les autres", {
  rl <- rang_long
  rl$species_code <- c("pinus_halepensis", "quercus_ilex", "fagus_sylvatica", "quercus_petraea", NA)
  tr <- traits_fictifs
  tr$code <- c("pinus_halepensis", "quercus_ilex", "fagus_sylvatica", "quercus_petraea")
  f <- .filtrer_froid(rl, tr, -15, 2, aides = TRUE)
  expect_equal(f$rang$species_code[f$rang$ug_id == "a"], c("fagus_sylvatica", "quercus_petraea"))
  expect_equal(f$ecartees$species_code, "pinus_halepensis")
  expect_equal(f$hors_aide$species_code, "quercus_ilex")
  expect_equal(f$hors_aide$aides_bfc, "hors_liste")
  sans <- .filtrer_froid(rl, tr, -15, 2)
  expect_equal(sans$rang$species_code[sans$rang$ug_id == "a"], c("quercus_ilex", "fagus_sylvatica"))
  expect_equal(nrow(sans$hors_aide), 0L)
  s <- .synthese_essences(f$rang[f$rang$ug_id == "a", ], data.frame(ug_id = "a", label = "UGF 1"), "Couchey",
                          hors_aide = f$hors_aide)
  expect_equal(s$ugf[[1]]$essences[[1]]$aides_bfc, "objectif")
  expect_match(s$texte, "non éligibles en essence objectif aux aides de l'État en Bourgogne-Franche-Comté : Chêne vert \\(hors liste\\)\\.$")
})

test_that(".filtrer_froid ecarte les paires exclues par le coeur et garde leur note", {
  # Classement complet (sans filtre) ; le coeur exclut le Pin d'Alep de l'UGF a.
  excl <- data.frame(ug_id = "a", species_code = "pa", stringsAsFactors = FALSE)
  f <- .filtrer_froid(rang_long, traits_fictifs, NULL, 2, exclus = excl)
  expect_equal(f$rang$species_code[f$rang$ug_id == "a"], c("qi", "fs"))
  expect_equal(f$ecartees$species_code, "pa")
  expect_equal(f$ecartees$suitability, 97) # note conservee (champ aptitude de ecartees_froid)
  expect_equal(f$ecartees$tenue_froid_c, -12)
  s <- .synthese_essences(f$rang[f$rang$ug_id == "a", ], data.frame(ug_id = "a", label = "UGF 1"), "Couchey",
                          "fr", f$ecartees, f$locale, NULL, -13.2)
  expect_equal(s$ugf[[1]]$ecartees_froid[[1]], list(code = "pa", nom = "Pin d'Alep", aptitude = 97, tenue_froid_c = -12))
  expect_match(s$texte, "minimum décennal -13 °C\\) : Pin d'Alep \\(tient -12 °C\\)")
})

test_that("une UGF dont toutes les essences sont exclues par le froid le dit (froid_hiver)", {
  excl <- data.frame(ug_id = "a", species_code = c("pa", "qi", "fs", "qp"), stringsAsFactors = FALSE)
  f <- .filtrer_froid(rang_long, traits_fictifs, NULL, 2, exclus = excl)
  a <- f$rang[f$rang$ug_id == "a", ]
  expect_true(is.na(a$rank))
  expect_equal(a$limiting_factor, "froid_hiver")
  s <- .synthese_essences(f$rang, data.frame(ug_id = "a", label = "UGF 1"), "Couchey", "fr", f$ecartees, f$locale, NULL, -20)
  expect_match(s$texte, "aucune essence ne tient le froid d'hiver de l'UGF")
  expect_match(s$texte, "Pin d'Alep \\(tient -12 °C\\)")
})

test_that(".froid_exiger rend le cache, ou une erreur claire : absent, en cours, en echec", {
  d <- withr::local_tempdir()
  ids <- c("a", "b")
  classe <- function(expr) tryCatch({ expr; "aucune" }, error = function(e) class(e)[1])
  expect_equal(classe(.froid_exiger(d, ids)), "nemetonmcp_froid_absent")
  expect_match(tryCatch(.froid_exiger(d, ids), error = conditionMessage), "classement non rendu")
  # En cours : pid vivant (le notre).
  .froid_ecrire(d, list(job_id = "j1", statut = "en_cours", pid = Sys.getpid(), lance_a = .froid_maintenant()))
  expect_equal(classe(.froid_exiger(d, ids)), "nemetonmcp_froid_en_cours")
  # Echec : E-OBS indisponible.
  .froid_ecrire(d, list(job_id = "j1", statut = "echec", erreur = "E-OBS tn unavailable (knmi)"))
  e <- tryCatch(.froid_exiger(d, ids), error = function(e) e)
  expect_s3_class(e, "nemetonmcp_froid_indisponible")
  expect_match(conditionMessage(e), "Froid d'hiver indisponible, classement non rendu")
  # Cache partiel (une UGF manque) : pas rendu.
  saveRDS(data.frame(ug_id = "a", tmin_decennale = -13), file.path(d, "tmin_decennale.rds"))
  expect_equal(classe(.froid_exiger(d, ids)), "nemetonmcp_froid_indisponible")
  saveRDS(data.frame(ug_id = c("a", "b", "c"), tmin_decennale = c(-13, -11, -9)), file.path(d, "tmin_decennale.rds"))
  expect_equal(.froid_exiger(d, ids)$tmin_decennale[1:2], c(-13, -11))
})

test_that(".tache_froid ecrit le cache complet seulement, et tient l'etat du job", {
  d <- withr::local_tempdir()
  carre <- function(x) sf::st_polygon(list(rbind(c(x, 6685000), c(x + 100, 6685000), c(x + 100, 6685100),
                                                 c(x, 6685100), c(x, 6685000))))
  sf::st_write(sf::st_sf(ug_id = c("a", "b"), tmax_moyenne = c(30, 31), d_tmax = c(1, 2),
                         geometry = sf::st_sfc(carre(843000), carre(843100), crs = 2154)),
               file.path(d, "sensibilite.gpkg"), quiet = TRUE)
  .froid_ecrire(d, list(job_id = "j1", statut = "en_cours", lance_a = .froid_maintenant()))
  ok <- function(x) { x$tmin_decennale <- c(-13.2, -11); x$tmin_decennale_n_annees <- 30L; x }
  .tache_froid(d, "j1", ok)
  expect_equal(.froid_job(d)$statut, "termine")
  expect_equal(.froid_job(d)$pid, Sys.getpid())
  expect_equal(readRDS(file.path(d, "tmin_decennale.rds"))$tmin_decennale, c(-13.2, -11))
  # E-OBS indisponible : le coeur avertit et rend NA -> echec, pas de cache.
  d2 <- withr::local_tempdir()
  file.copy(file.path(d, "sensibilite.gpkg"), d2)
  .froid_ecrire(d2, list(job_id = "j2", statut = "en_cours"))
  .tache_froid(d2, "j2", function(x) { warning("E-OBS tn unavailable (knmi)"); x$tmin_decennale <- NA_real_; x })
  expect_equal(.froid_job(d2)$statut, "echec")
  expect_match(.froid_job(d2)$erreur, "E-OBS tn unavailable")
  expect_false(file.exists(file.path(d2, "tmin_decennale.rds")))
  # Une erreur du coeur est aussi un echec.
  .froid_ecrire(d2, list(job_id = "j3", statut = "en_cours"))
  .tache_froid(d2, "j3", function(x) stop("pas de reseau"))
  expect_match(.froid_job(d2)$erreur, "pas de reseau")
  # Un job remplace entre-temps (autre job_id) n'est pas ecrase.
  .froid_ecrire(d2, list(job_id = "j4", statut = "en_cours"))
  .tache_froid(d2, "j3", ok)
  expect_equal(.froid_job(d2)$job_id, "j4")
})

test_that(".froid_lancer lance une fois, dit s'il est fait ou en cours", {
  d <- withr::local_tempdir()
  appels <- 0L
  spawn <- function(dossier, job_id, log) { appels <<- appels + 1L; invisible(TRUE) }
  r1 <- .froid_lancer("p", d, c("a", "b"), spawn)
  expect_equal(r1$statut, "en_cours")
  expect_equal(appels, 1L)
  expect_equal(.froid_job(d)$job_id, r1$job_id)
  # Sans pid depuis moins de 2 minutes : encore en lancement, pas de second job.
  r2 <- .froid_lancer("p", d, c("a", "b"), spawn)
  expect_equal(r2$job_id, r1$job_id)
  expect_equal(appels, 1L)
  saveRDS(data.frame(ug_id = c("a", "b"), tmin_decennale = c(-13, -11)), file.path(d, "tmin_decennale.rds"))
  expect_equal(.froid_lancer("p", d, c("a", "b"), spawn)$statut, "termine")
  expect_equal(appels, 1L)
  # Echec du lancement : etat en echec, erreur propagee.
  d2 <- withr::local_tempdir()
  expect_error(.froid_lancer("p", d2, "a", function(...) stop("setsid absent")), "setsid absent")
  expect_equal(.froid_job(d2)$statut, "echec")
  # Un job en echec se relance.
  .froid_ecrire(d2, list(job_id = "x", statut = "echec", erreur = "E-OBS"))
  expect_equal(.froid_lancer("p", d2, "a", spawn)$statut, "en_cours")
})

# Projet de bout en bout : analyse en cache (sensibilite.gpkg), identite des
# UGF et nom du projet simules ; le coeur nemeton est le vrai.
projet_regen <- function(froid = NULL) {
  racine <- withr::local_tempdir(.local_envir = parent.frame())
  dossier <- file.path(racine, "cache", "regeneration")
  dir.create(dossier, recursive = TRUE)
  carre <- function(x) sf::st_polygon(list(rbind(c(x, 6685000), c(x + 100, 6685000), c(x + 100, 6685100),
                                                 c(x, 6685100), c(x, 6685000))))
  sf::st_write(sf::st_sf(ug_id = c("ug_1", "ug_2"), tmax_moyenne = c(30, 31), d_tmax = c(1, -0.5),
                         vpd_canicule = c(2, 3), geometry = sf::st_sfc(carre(843000), carre(843100), crs = 2154)),
               file.path(dossier, "sensibilite.gpkg"), quiet = TRUE)
  sf::st_write(sf::st_sf(ug_id = c("ug_1", "ug_2"), rew_min = c(0.1, 0.3),
                         geometry = sf::st_sfc(carre(843000), carre(843100), crs = 2154)),
               file.path(dossier, "biljou.gpkg"), quiet = TRUE)
  if (!is.null(froid)) saveRDS(data.frame(ug_id = c("ug_1", "ug_2"), tmin_decennale = froid), file.path(dossier, "tmin_decennale.rds"))
  list(racine = racine, dossier = dossier)
}
simuler_projet <- function(racine, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .ns = function(nom) switch(nom, get_project_path = function(id) racine, function(x) x),
    .package = "nemetonmcp", .env = env)
}
lire <- function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)

test_that("regen_essences : seuil decennal applique, essences ecartees, JSON sans rupture", {
  p <- projet_regen(froid = c(-13.2, -13.2))
  simuler_projet(p$racine)
  r <- lire(regen_essences("p", top_n = 3, aides = FALSE))
  expect_true(r$ok)
  expect_equal(r$climat$seuil_decennal_c, -13.2)
  expect_equal(unlist(r$climat$tmin_decennale_c), c(-13.2, -13.2))
  # Champs historiques conserves (depreciés) : seuil_froid_c garde le sens min(-15, ERA5).
  expect_equal(r$climat$seuil_froid_c, -15)
  tol <- nemeton::european_species_tolerances()
  e <- r$ugf[[1]]$ecartees_froid
  expect_gt(length(e), 0L)
  expect_true(all(c("code", "nom", "aptitude", "tenue_froid_c") %in% names(e[[1]])))
  # Aucune essence classee n'est moins rustique que le minimum decennal.
  codes <- vapply(r$ugf[[1]]$essences, `[[`, "", "code")
  expect_true(all(tol$frost_winter_min_c[match(codes, tol$code)] <= -13.2, na.rm = TRUE))
  expect_true(all(vapply(e, `[[`, 0, "tenue_froid_c") > -13.2))
  expect_match(r$texte, "Écartées pour le froid d'hiver \\(minimum décennal -13 °C\\)")
  expect_true(length(r$avertissements) >= 1L)
  # Aucune ecriture dans le projet (lecture seule).
  expect_setequal(list.files(p$dossier), c("sensibilite.gpkg", "biljou.gpkg", "tmin_decennale.rds"))
})

test_that("regen_essences : un appel avec ug ne touche pas au cache et classe l'UGF demandee", {
  p <- projet_regen(froid = c(-13.2, -11))
  simuler_projet(p$racine)
  avant <- readRDS(file.path(p$dossier, "tmin_decennale.rds"))
  r <- lire(regen_essences("p", ug = "ug_2", aides = FALSE))
  expect_equal(r$n_ugf, 1L)
  expect_equal(r$climat$seuil_decennal_c, -11)
  expect_identical(readRDS(file.path(p$dossier, "tmin_decennale.rds")), avant)
})

test_that("regen_essences : sans cache du minimum decennal, erreur claire et rien d'ecrit ni de calcule", {
  p <- projet_regen()
  simuler_projet(p$racine)
  testthat::local_mocked_bindings(regen_tmin_decennale = function(...) stop("ne doit pas etre appele"), .package = "nemeton")
  r <- lire(regen_essences("p"))
  expect_false(r$ok)
  expect_equal(r$code, "froid_absent")
  expect_match(r$erreur, "classement non rendu")
  expect_null(r$ugf)
  expect_setequal(list.files(p$dossier), c("sensibilite.gpkg", "biljou.gpkg"))
  # Calcul en echec (E-OBS indisponible) : erreur claire, jamais de classement.
  .froid_ecrire(p$dossier, list(job_id = "j", statut = "echec", erreur = "E-OBS tn unavailable (knmi)"))
  r2 <- lire(regen_essences("p"))
  expect_false(r2$ok)
  expect_equal(r2$code, "froid_indisponible")
  expect_match(r2$erreur, "Froid d'hiver indisponible, classement non rendu")
  expect_null(r2$ugf)
  # Calcul en cours.
  .froid_ecrire(p$dossier, list(job_id = "j", statut = "en_cours", pid = Sys.getpid(), lance_a = .froid_maintenant()))
  expect_equal(lire(regen_essences("p"))$code, "froid_en_cours")
})

test_that("lancer_froid_hiver : refuse sans analyse, lance en tache de fond avec l'analyse", {
  p <- projet_regen()
  simuler_projet(p$racine)
  lances <- character()
  testthat::local_mocked_bindings(.froid_spawn = function(dossier, job_id, log) { lances <<- c(lances, job_id); invisible(TRUE) })
  r <- lire(lancer_froid_hiver("p"))
  expect_true(r$ok)
  expect_equal(r$statut, "en_cours")
  expect_length(lances, 1L)
  expect_equal(lire(lancer_froid_hiver("p"))$job_id, r$job_id)
  expect_length(lances, 1L)
  vide <- withr::local_tempdir()
  simuler_projet(vide)
  expect_equal(lire(lancer_froid_hiver("p"))$code, "regeneration_absente")
})

test_that(".avertissements_regen signale d_tmax negatif, gel sur deux printemps et le sens de confiance", {
  u <- data.frame(d_tmax = c(1.5, -0.5, -2), r7_gel_days = c(0, 2, NA))
  a <- .avertissements_regen(u, list(annees_gel_tardif = c(2022L, 2023L)))
  expect_length(a, 3L)
  expect_match(a[1], "^d_tmax est négatif dans 2 UGF")
  expect_match(a[2], "Gel tardif \\(r7_gel_days\\) fondé sur 2 printemps")
  expect_match(a[3], "traits de l'essence")
  # Pas de d_tmax negatif, gel sur plus de deux printemps : seule la confiance.
  b <- .avertissements_regen(data.frame(d_tmax = 1, r7_gel_days = 1), list(annees_gel_tardif = 2019:2023), "en")
  expect_length(b, 1L)
  expect_match(b, "species traits")
})
