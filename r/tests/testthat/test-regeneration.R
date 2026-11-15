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
