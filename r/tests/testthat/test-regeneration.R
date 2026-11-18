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
  expect_null(c1$tmin_hiver_c) # le froid vient du minimum decennal du coeur, plus d'ERA5 local
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

test_that(".filtrer_froid sans seuil rend les essences exclues par le coeur comme ecartees", {
  # Le coeur a deja retire le Pin d'Alep du classement (attr exclus_froid).
  rl <- rang_long[rang_long$species_code %in% c("qi", "fs", "qp") | is.na(rang_long$species_code), ]
  rl$rank[rl$ug_id == "a"] <- 1:3
  excl <- data.frame(ug_id = "a", species_code = "pa", stringsAsFactors = FALSE)
  tr <- traits_fictifs
  tr$label <- c("Pin d'Alep", "Chêne vert", "Hêtre", "Chêne sessile")
  f <- .filtrer_froid(rl, tr, NULL, 2, exclus = excl)
  expect_equal(f$rang$species_code[f$rang$ug_id == "a"], c("qi", "fs"))
  expect_equal(f$ecartees$species_code, "pa")
  expect_equal(f$ecartees$label, "Pin d'Alep")
  expect_equal(f$ecartees$tenue_froid_c, -12)
  s <- .synthese_essences(f$rang[f$rang$ug_id == "a", ], data.frame(ug_id = "a", label = "UGF 1"), "Couchey",
                          "fr", f$ecartees, f$locale, NULL, -13.2)
  expect_equal(s$ugf[[1]]$ecartees_froid[[1]], list(code = "pa", nom = "Pin d'Alep", tenue_froid_c = -12))
  expect_match(s$texte, "minimum décennal -13 °C\\) : Pin d'Alep \\(tient -12 °C\\)")
})

test_that("une UGF dont toutes les essences sont exclues par le froid le dit", {
  rl <- data.frame(ug_id = "a", rank = NA_integer_, species_code = NA_character_, label = NA_character_,
                   type = NA, suitability = NA_real_, limiting_factor = "froid_hiver", confidence = NA_character_,
                   invasif = NA, stringsAsFactors = FALSE)
  excl <- data.frame(ug_id = "a", species_code = c("pa", "qi"), stringsAsFactors = FALSE)
  tr <- traits_fictifs
  tr$label <- c("Pin d'Alep", "Chêne vert", "Hêtre", "Chêne sessile")
  f <- .filtrer_froid(rl, tr, NULL, 2, exclus = excl)
  expect_true(is.na(f$rang$rank))
  s <- .synthese_essences(f$rang, data.frame(ug_id = "a", label = "UGF 1"), "Couchey", "fr", f$ecartees, f$locale, NULL, -20)
  expect_match(s$texte, "aucune essence ne tient le froid d'hiver de l'UGF")
  expect_match(s$texte, "Pin d'Alep \\(tient -12 °C\\)")
})

test_that(".tmin_decennale_projet calcule une fois, garde le resultat, et rend NULL sans E-OBS", {
  d <- withr::local_tempdir()
  u <- data.frame(ug_id = c("a", "b"))
  appels <- 0L
  calcul <- function(x) { appels <<- appels + 1L; x$tmin_decennale <- c(-13.2, -11); x$tmin_decennale_n_annees <- 30L; x }
  r1 <- .tmin_decennale_projet(u, d, calcul)
  expect_equal(r1$tmin_decennale, c(-13.2, -11))
  expect_true(file.exists(file.path(d, "tmin_decennale.rds")))
  r2 <- .tmin_decennale_projet(u, d, calcul)
  expect_equal(r2$tmin_decennale, c(-13.2, -11))
  expect_equal(appels, 1L)
  # Une UGF de plus : recalcul.
  .tmin_decennale_projet(data.frame(ug_id = c("a", "b", "c")), d, function(x) { appels <<- appels + 1L; x$tmin_decennale <- c(1, 2, 3); x })
  expect_equal(appels, 2L)
  # E-OBS indisponible : le coeur rend NA, ou echoue.
  d2 <- withr::local_tempdir()
  expect_null(.tmin_decennale_projet(u, d2, function(x) { x$tmin_decennale <- NA_real_; x }))
  expect_null(.tmin_decennale_projet(u, d2, function(x) stop("pas de reseau")))
  expect_false(file.exists(file.path(d2, "tmin_decennale.rds")))
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
