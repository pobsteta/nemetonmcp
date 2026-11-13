# Lot 4 : rendu des rasters en PNG georeferences, sur des rasters fictifs.

raster_lambert <- function(valeurs, nrow = 20, ncol = 30, xmin = 843000, ymin = 6685000, res = 10) {
  r <- terra::rast(nrows = nrow, ncols = ncol, xmin = xmin, xmax = xmin + ncol * res,
                   ymin = ymin, ymax = ymin + nrow * res, crs = "EPSG:2154")
  terra::values(r) <- valeurs
  r
}

test_that(".colorer peint les classes, laisse transparentes celles sans couleur", {
  leg <- .legende_classes(0:2, c("sain", "faible", "fort"), c("#00ff00", "#ffff00", "#ff0000"), transparentes = 0)
  expect_null(leg$classes[[1]]$couleur)
  expect_equal(.colorer(c(0, 1, 2, NA, 7), leg), c(NA, "#ffff00", "#ff0000", NA, NA))
})

test_that(".colorer interpole une legende continue et borne aux extremes", {
  leg <- list(type = "continu", min = 0, max = 10, couleurs = list("#000000", "#FFFFFF"))
  expect_equal(.colorer(c(-5, 0, 5, 10, 20, NA), leg),
               c("#000000", "#000000", "#808080", "#FFFFFF", "#FFFFFF", NA))
})

test_that(".dates_frise : fins de mois sur deux ans au plus, fins d'annee au-dela, plafond", {
  j <- as.numeric(as.Date(c("2026-06-10", "2026-09-25")))
  expect_equal(.dates_frise(j), c("2026-06-30", "2026-07-31", "2026-08-31", "2026-09-25"))
  j <- as.numeric(as.Date(c("2019-01-03", "2026-09-25")))
  d <- .dates_frise(j)
  expect_equal(d[1], "2019-12-31")
  expect_equal(utils::tail(d, 1), "2026-09-25")
  expect_length(d, 8)
  j <- as.numeric(as.Date(c("1990-01-01", "2026-09-25")))
  d <- .dates_frise(j, max_n = 5)
  expect_lte(length(d), 6)
  expect_equal(utils::tail(d, 1), "2026-09-25")
  expect_equal(.dates_frise(c(NA_real_, NA_real_)), character())
})

test_that(".legende_niveaux traduit les classes, reprend les couleurs, rend hors foret transparent", {
  r <- raster_lambert(rep(1:4, length.out = 600))
  levels(r) <- data.frame(value = 1:4, classe = c("parcourable", "accessible", "non_accessible", "hors_foret"))
  leg <- .legende_niveaux(r, .textes_rasters("fr"))
  expect_equal(vapply(leg$classes, `[[`, "", "libelle"),
               c("Parcourable (engin)", "Accessible", "Non accessible", "Hors for\u00eat"))
  expect_equal(leg$classes[[1]]$couleur, "#2E7D32")
  expect_null(leg$classes[[4]]$couleur)
  r2 <- raster_lambert(rep(1:2, length.out = 600))
  levels(r2) <- data.frame(value = 1:2, classe_debardage = c("0-250", "> 2000"))
  leg2 <- .legende_niveaux(r2, .textes_rasters("en"))
  expect_equal(vapply(leg2$classes, `[[`, "", "libelle"), c("0\u2013250 m", "> 2000 m"))
})

test_that(".rendre_png ecrit une image RGBA et son emprise WGS84", {
  skip_if_not_installed("png")
  v <- rep(c(0, 1, 2), length.out = 600)
  v[1:30] <- NA
  r <- raster_lambert(v)
  leg <- .legende_classes(0:2, c("a", "b", "c"), c("#00ff00", "#ffff00", "#ff0000"), transparentes = 0)
  f <- withr::local_tempfile(fileext = ".png")
  rd <- .rendre_png(r, leg, f)
  expect_true(file.exists(f))
  img <- png::readPNG(f)
  expect_equal(dim(img)[3], 4L)
  expect_equal(c(rd$hauteur, rd$largeur), dim(img)[1:2])
  # La premiere ligne (NA) est transparente, des pixels peints existent.
  expect_true(all(img[1, , 4] == 0))
  expect_true(any(img[, , 4] == 1))
  # Emprise [[sud, ouest], [nord, est]] autour du raster, en degres.
  e <- rd$emprise
  expect_lt(e[[1]][1], e[[2]][1])
  expect_lt(e[[1]][2], e[[2]][2])
  centre <- sf::st_transform(sf::st_sfc(sf::st_point(c(843150, 6685100)), crs = 2154), 4326)
  xy <- sf::st_coordinates(centre)
  expect_true(xy[2] > e[[1]][1] && xy[2] < e[[2]][1] && xy[1] > e[[1]][2] && xy[1] < e[[2]][2])
})

test_that(".rendre_png agrege un raster plus grand que max_px", {
  skip_if_not_installed("png")
  r <- raster_lambert(seq_len(200 * 300), nrow = 200, ncol = 300, res = 1)
  leg <- .legende_continue(1, 60000, "viridis")
  f <- withr::local_tempfile(fileext = ".png")
  rd <- .rendre_png(r, leg, f, max_px = 100)
  expect_lte(max(rd$largeur, rd$hauteur), 110)
})

test_that(".rendre_png grise le contexte hors des unites, garde les couleurs dedans", {
  skip_if_not_installed("png")
  # Raster 300 x 200 m entierement rouge ; une seule unite sur le tiers ouest.
  r <- raster_lambert(rep(1, 600))
  leg <- .legende_classes(1, "fort", "#ff0000")
  ug <- sf::st_sf(ug_id = "ug_1", geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(843000, 6685000), c(843100, 6685000), c(843100, 6685200), c(843000, 6685200), c(843000, 6685000)))),
    crs = 2154))
  f <- withr::local_tempfile(fileext = ".png")
  .rendre_png(r, leg, f, ugs = ug, marge_m = 0)
  img <- png::readPNG(f)
  milieu <- round(dim(img)[1] / 2)
  ouest <- img[milieu, 2, ]
  est <- img[milieu, dim(img)[2] - 1, ]
  # Dedans : rouge plein, opaque.
  expect_equal(unname(ouest), c(1, 0, 0, 1), tolerance = 0.01)
  # Dehors : gris (luminance du rouge), semi-transparent.
  expect_equal(est[1], est[2], tolerance = 0.01)
  expect_equal(est[1], 0.299, tolerance = 0.01)
  expect_equal(est[4], .OPACITE_CONTEXTE, tolerance = 0.01)
  # Sans unites : rendu d'origine, tout en rouge opaque.
  f2 <- withr::local_tempfile(fileext = ".png")
  .rendre_png(r, leg, f2)
  img2 <- png::readPNG(f2)
  expect_equal(unname(img2[milieu, dim(img2)[2] - 1, ]), c(1, 0, 0, 1), tolerance = 0.01)
})

test_that(".masque_ugs etend les unites de la marge, en metres vrais", {
  r <- raster_lambert(rep(1, 600))
  m <- terra::project(r, "EPSG:3857", method = "near")
  ug <- sf::st_sf(ug_id = "ug_1", geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(843000, 6685000), c(843100, 6685000), c(843100, 6685200), c(843000, 6685200), c(843000, 6685000)))),
    crs = 2154))
  sans <- sum(.masque_ugs(m, ug, 0))
  avec <- sum(.masque_ugs(m, ug, 50))
  expect_gt(sans, 0)
  expect_lt(sans, length(terra::values(m[[1]], mat = FALSE)))
  expect_gt(avec, sans)
  expect_null(.masque_ugs(m, NULL))
})

test_that(".stats_ug et .serie_frise_ug resument une couche par unite", {
  # Deux unites : la moitie ouest et la moitie est du raster.
  ugs <- sf::st_sf(ug_id = c("ug_1", "ug_2"), geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(843000, 6685000), c(843150, 6685000), c(843150, 6685200), c(843000, 6685200), c(843000, 6685000)))),
    sf::st_polygon(list(rbind(c(843150, 6685000), c(843300, 6685000), c(843300, 6685200), c(843150, 6685200), c(843150, 6685000)))),
    crs = 2154))
  sev <- raster_lambert(rep(c(rep(0, 15), rep(2, 15)), 20))  # 0 a l'ouest, 2 a l'est
  leg <- .legende_classes(0:4, letters[1:5], rep("#000000", 5), transparentes = 0)
  s <- .stats_ug(sev, leg, ugs)
  expect_equal(s$ug_1$parts$`0`, 1)
  expect_equal(s$ug_2$parts$`2`, 1)
  expect_equal(s$ug_2$surface_ha, 3)
  cont <- .stats_ug(raster_lambert(rep(5, 600)), .legende_continue(0, 10, "viridis"), ugs)
  expect_equal(cont$ug_1$moyenne, 5)
  # Premiere anomalie : la moitie nord detectee en 2025, la moitie sud en 2026.
  j25 <- as.numeric(as.Date("2025-07-01")); j26 <- as.numeric(as.Date("2026-07-01"))
  fa <- raster_lambert(c(rep(j25, 300), rep(j26, 300)))
  serie <- .serie_frise_ug(sev, fa, c("2025-12-31", "2026-12-31"), ugs)
  expect_equal(serie$ug_1, c(0, 0))
  expect_equal(serie$ug_2, c(1.5, 3))
})

test_that(".catalogue_rasters liste FAST et accessibilite, signale le reste", {
  skip_if_not_installed("png")
  d <- withr::local_tempdir()
  dir.create(file.path(d, "cache", "layers", "fast_alert_mask", "zone_7"), recursive = TRUE)
  dir.create(file.path(d, "cache", "accessibility"), recursive = TRUE)
  jsonlite::write_json(list(monitoring_zone_id = 7), file.path(d, "metadata.json"), auto_unbox = TRUE)
  terra::writeRaster(raster_lambert(rep(0:4, length.out = 600)),
                     file.path(d, "cache", "layers", "fast_alert_mask", "zone_7", "fast_alert_NDVI_count_abc.tif"))
  r <- raster_lambert(rep(1:4, length.out = 600))
  levels(r) <- data.frame(value = 1:4, classe = c("parcourable", "accessible", "non_accessible", "hors_foret"))
  terra::writeRaster(r, file.path(d, "cache", "accessibility", "acc_skidder.tif"))

  cat_r <- .catalogue_rasters(d, "fr")
  expect_setequal(names(cat_r$couches), c("fast_ndvi_count", "acc_skidder"))
  expect_equal(cat_r$zone, 7L)
  expect_setequal(names(cat_r$absentes), c("fordead", "reconfort", "regeneration"))
  expect_equal(cat_r$couches$fast_ndvi_count$theme, "sante")
  expect_equal(cat_r$couches$acc_skidder$theme, "accessibilite")
  expect_s4_class(cat_r$couches$acc_skidder$lire(), "SpatRaster")
  expect_true(nzchar(.cle_fichiers(cat_r$couches$acc_skidder$sources)))
  expect_equal(.cle_fichiers(file.path(d, "absent.tif")), "0")
})
