raster_l93 <- function(valeurs, nrow = 20, ncol = 30) {
  terra::rast(nrows = nrow, ncols = ncol, xmin = 843000, xmax = 843000 + ncol * 10,
              ymin = 6685000, ymax = 6685000 + nrow * 10, crs = "EPSG:2154", vals = valeurs)
}
ugs_deux <- function() sf::st_sf(
  ug_id = c("ug_1", "ug_2"), label = c("UGF 1", "UGF 2"), surface_ha = c(3, 3),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(843000, 6685000), c(843150, 6685000), c(843150, 6685200), c(843000, 6685200), c(843000, 6685000)))),
    sf::st_polygon(list(rbind(c(843150, 6685000), c(843300, 6685000), c(843300, 6685200), c(843150, 6685200), c(843150, 6685000)))),
    crs = 2154))

test_that(".part_classes somme les parts des classes demandees", {
  parts <- list(`0` = 0.5, `1` = 0.2, `3` = 0.1, `4` = 0.2)
  expect_equal(.part_classes(parts, 1:3), 0.3)
  expect_equal(.part_classes(parts, 2), 0)
  expect_equal(.part_classes(NULL, 1:3), 0)
})

test_that(".niveau_alerte : FORDEAD, FAST, RECONFORT et coupes rases", {
  fd <- function(part, ha, forte = 0, coupe = 0) list(fordead = list(part_touchee = part, surface_touchee_ha = ha,
                                                                    part_forte = forte, coupe_rase_ha = coupe,
                                                                    premiere_anomalie = "2024-07-12"))
  expect_equal(.niveau_alerte(list())$niveau, "aucune")
  n <- .niveau_alerte(fd(0.15, 0.5))
  expect_equal(n$niveau, "forte")
  expect_match(n$motifs[1], "FORDEAD : 0,5 ha dépérissants \\(15 %\\), depuis 2024-07")
  expect_equal(.niveau_alerte(fd(0.01, 1.2))$niveau, "forte")
  expect_equal(.niveau_alerte(fd(0.03, 0.1))$niveau, "moderee")
  coupe <- .niveau_alerte(fd(0, 0, coupe = 2.4))
  expect_equal(coupe$niveau, "aucune")
  expect_match(coupe$motifs, "coupe rase\\) : 2,4 ha")
  fast <- list(fast = list(`NDVI (count)` = list(part_forte = 0.9, part_tres_forte = 0.35)))
  expect_equal(.niveau_alerte(fast)$niveau, "moderee")
  fast$fast$`NDVI (count)`$part_tres_forte <- 0.2
  expect_equal(.niveau_alerte(fast)$niveau, "aucune")
  rec <- list(reconfort = list(part_deperissant = 0.5, part_tres_deperissant = 0.3))
  expect_equal(.niveau_alerte(rec)$niveau, "forte")
  rec$reconfort$part_tres_deperissant <- 0.1
  expect_equal(.niveau_alerte(rec)$niveau, "moderee")
  expect_match(.niveau_alerte(rec, "en")$motifs, "RECONFORT: 50 % declining")
})

test_that(".synthese_alertes classe les UGF et resume en une phrase", {
  ugs <- data.frame(ug_id = c("a", "b", "c"), label = c("UGF 1", "UGF 2", "UGF 3"), surface_ha = c(1, 2, 3))
  par_ug <- list(a = list(reconfort = list(part_deperissant = 0.3, part_tres_deperissant = 0)),
                 b = list(fordead = list(part_touchee = 0.4, surface_touchee_ha = 0.8, part_forte = 0)),
                 c = list())
  src <- list(list(id = "fordead_severite", libelle = "FORDEAD · sévérité"),
              list(id = "reconfort_classes", libelle = "RECONFORT · classes"))
  s <- .synthese_alertes(par_ug, ugs, "Dabo", src)
  expect_equal(c(s$n_ugf, s$n_forte, s$n_moderee, s$ugf_saines), c(3, 1, 1, 1))
  expect_equal(vapply(s$alertes, `[[`, "", "ug_id"), c("b", "a"))
  expect_equal(s$alertes[[1]]$fordead$surface_touchee_ha, 0.8)
  expect_match(s$texte, "^Dabo : 1 UGF en alerte forte, 1 en alerte modérée, sur 3 \\(FORDEAD, RECONFORT\\)\\. La plus touchée : UGF 2")
  saine <- .synthese_alertes(list(), ugs, "Dabo", src)
  expect_match(saine$texte, "aucune alerte sanitaire sur 3 UGF")
  expect_match(.synthese_alertes(list(), ugs, "Dabo", list())$texte, "Aucune couche")
})

test_that(".mesures_sante extrait FORDEAD, FAST et RECONFORT par UGF", {
  ugs <- ugs_deux()
  # FORDEAD : ouest sain, est en severite 2 (moitie) et sol nu (moitie).
  sev <- raster_l93(rep(c(rep(0, 15), rep(c(2, 4), length.out = 15)), 20))
  j <- as.numeric(as.Date("2024-07-12"))
  fa <- raster_l93(rep(c(rep(NA, 15), rep(j, 15)), 20))
  leg <- .legende_classes(0:4, letters[1:5], rep("#000000", 5), transparentes = 0)
  fast <- raster_l93(rep(4, 600))
  rec <- raster_l93(rep(c(rep(1, 15), rep(3, 15)), 20))
  cat_r <- list(couches = list(
    fordead_severite = list(id = "fordead_severite", theme = "sante", libelle = "FORDEAD · s",
                            lire = function() sev, legende = leg, premiere = fa),
    fast_ndvi_count = list(id = "fast_ndvi_count", theme = "sante", libelle = "FAST · a", date = "2026-10-19",
                           lire = function() fast, legende = leg),
    reconfort_classes = list(id = "reconfort_classes", theme = "sante", libelle = "RECONFORT · c",
                             lire = function() rec, legende = .legende_classes(1:3, letters[1:3], rep("#000000", 3))),
    acc_skidder = list(id = "acc_skidder", theme = "accessibilite")))
  m <- .mesures_sante(cat_r, ugs)
  expect_equal(vapply(m$sources, `[[`, "", "id"), c("fordead_severite", "fast_ndvi_count", "reconfort_classes"))
  expect_equal(m$sources[[1]]$date, "2024-07-12")
  e <- m$par_ug$ug_2
  expect_equal(e$fordead$part_touchee, round(8 / 15, 4))
  expect_equal(e$fordead$coupe_rase_ha, 1.4)
  expect_equal(e$fordead$premiere_anomalie, "2024-07-12")
  expect_equal(m$par_ug$ug_1$fordead$part_touchee, 0)
  expect_null(m$par_ug$ug_1$fordead$premiere_anomalie)
  expect_equal(e$fast$`NDVI (count)`$part_tres_forte, 1)
  expect_equal(e$reconfort$part_tres_deperissant, 1)
  expect_equal(m$par_ug$ug_1$reconfort$part_deperissant, 0)
  s <- .synthese_alertes(m$par_ug, sf::st_drop_geometry(ugs), "Essai", m$sources)
  expect_equal(s$alertes[[1]]$ug_id, "ug_2")
  expect_equal(s$alertes[[1]]$niveau, "forte")
})
