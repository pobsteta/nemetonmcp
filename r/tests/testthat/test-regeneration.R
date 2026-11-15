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
    " Écartées pour le froid d'hiver (seuil -15 °C) : Pin d'Alep (tient -12 °C).",
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
