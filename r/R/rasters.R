# Outil `vue_rasters(projet, couche, date)` : lot 4 du brief. Les rasters du
# projet (sante des forets, accessibilite, climat de la regeneration) sont
# pre-rendus en PNG georeferences, puisqu'un artefact ne peut ni lire un
# GeoTIFF ni charger des tuiles. Chaque PNG est reprojete en Web Mercator
# (EPSG:3857), la projection de Leaflet : pose par `L.imageOverlay` sur son
# emprise WGS84, il tombe juste, sans deformation.
#
# Aucune semantique metier ici : les rasters sont ceux que nemetonshiny a
# deja calcules et mis en cache dans le projet, lus par les lecteurs du coeur
# (`read_fordead_dieback_mask()`, `reconfort_cache_manifest()`...) quand ils
# existent ; palettes et classes sont celles de l'application. Rien n'est
# recalcule : une couche absente est signalee, avec l'onglet de nemetonshiny
# qui la produit.

.TEXTES_RASTERS <- list(
  fr = list(
    sante = "Sant\u00e9 des for\u00eats", accessibilite = "Accessibilit\u00e9",
    regeneration = "R\u00e9g\u00e9n\u00e9ration",
    fordead_severite = "FORDEAD \u00b7 s\u00e9v\u00e9rit\u00e9 du d\u00e9p\u00e9rissement",
    fordead_premiere = "FORDEAD \u00b7 date de premi\u00e8re anomalie",
    fast = "FAST \u00b7 alerte %s (%s)",
    reconfort_score = "RECONFORT \u00b7 score de d\u00e9p\u00e9rissement",
    reconfort_classes = "RECONFORT \u00b7 classes de sant\u00e9",
    reconfort_proba = "RECONFORT \u00b7 probabilit\u00e9 d'atteinte",
    acc_skidder = "D\u00e9busqueur (skidder)", acc_porteur = "Porteur",
    acc_camion_dfci = "D\u00e9fendabilit\u00e9 DFCI (camion)",
    acc_classes_debardage = "Classes de distance de d\u00e9bardage",
    climat = "Climat %s \u00b7 \u0394T\u00b0max \u00d7 \u0394pr\u00e9cipitations",
    sev = c("sain", "faible", "moyenne", "forte", "sol nu"),
    fast_cl = c("faible (moins fiable)", "moyenne (moins fiable)", "forte", "tr\u00e8s forte"),
    rec_cl = c("sain", "d\u00e9p\u00e9rissant", "tr\u00e8s d\u00e9p\u00e9rissant"),
    acc = c(parcourable = "Parcourable (engin)", accessible = "Accessible",
            non_accessible = "Non accessible", inaccessible = "Inaccessible",
            inexploitable = "Inexploitable", accessible_cable = "Accessible au c\u00e2ble",
            non_defendable_pente = "Non d\u00e9fendable (pente)",
            defendable_c1 = "D\u00e9fendable, bande 1 (la plus proche)",
            defendable_c2 = "D\u00e9fendable, bande 2", defendable_c3 = "D\u00e9fendable, bande 3",
            hors_foret = "Hors for\u00eat"),
    absente = list(
      fordead = "Lancer FORDEAD dans nemetonshiny (Suivi sanitaire).",
      fast = "Calculer une alerte FAST dans nemetonshiny (Suivi sanitaire).",
      reconfort = "Lancer RECONFORT dans nemetonshiny (Suivi sanitaire).",
      accessibilite = "Lancer l'analyse dans nemetonshiny (Accessibilit\u00e9).",
      regeneration = "Lancer la r\u00e9g\u00e9n\u00e9ration dans nemetonshiny (R\u00e9g\u00e9n\u00e9ration)."),
    regen_var = c(priorite = "Indice de priorit\u00e9", sensibilite = "Sensibilit\u00e9 microclimatique",
                  d_tmax = "\u0394T\u00b0max sous couvert (\u00b0C)", d_vpd = "\u0394VPD sous couvert (kPa)")
  ),
  en = list(
    sante = "Forest health", accessibilite = "Accessibility", regeneration = "Regeneration",
    fordead_severite = "FORDEAD \u00b7 dieback severity",
    fordead_premiere = "FORDEAD \u00b7 date of first anomaly",
    fast = "FAST \u00b7 %s alert (%s)",
    reconfort_score = "RECONFORT \u00b7 dieback score",
    reconfort_classes = "RECONFORT \u00b7 health classes",
    reconfort_proba = "RECONFORT \u00b7 probability of dieback",
    acc_skidder = "Skidder", acc_porteur = "Forwarder",
    acc_camion_dfci = "DFCI defendability (truck)",
    acc_classes_debardage = "Skidding distance classes",
    climat = "%s climate \u00b7 \u0394T\u00b0max \u00d7 \u0394precipitation",
    sev = c("healthy", "low", "medium", "high", "bare soil"),
    fast_cl = c("low (less reliable)", "medium (less reliable)", "high", "very high"),
    rec_cl = c("healthy", "declining", "severely declining"),
    acc = c(parcourable = "Traversable (machine)", accessible = "Accessible",
            non_accessible = "Not accessible", inaccessible = "Inaccessible",
            inexploitable = "Not workable", accessible_cable = "Cable-accessible",
            non_defendable_pente = "Not defendable (slope)",
            defendable_c1 = "Defendable, band 1 (closest)",
            defendable_c2 = "Defendable, band 2", defendable_c3 = "Defendable, band 3",
            hors_foret = "Outside forest"),
    absente = list(
      fordead = "Run FORDEAD in nemetonshiny (Health monitoring).",
      fast = "Compute a FAST alert in nemetonshiny (Health monitoring).",
      reconfort = "Run RECONFORT in nemetonshiny (Health monitoring).",
      accessibilite = "Run the analysis in nemetonshiny (Accessibility).",
      regeneration = "Run regeneration in nemetonshiny (Regeneration)."),
    regen_var = c(priorite = "Priority index", sensibilite = "Microclimatic sensitivity",
                  d_tmax = "Sub-canopy \u0394T\u00b0max (\u00b0C)", d_vpd = "Sub-canopy \u0394VPD (kPa)")
  )
)

# Couleurs des classes, reprises de nemetonshiny (mod_monitoring_*,
# mod_accessibility) pour que la vue et l'application se lisent pareil.
.COULEURS_SEVERITE <- c("#2CA02C", "#FFD27F", "#FF9933", "#D62728", "#222222")
.COULEURS_FAST <- c("#fee08b", "#fdae61", "#f46d43", "#d73027")
.COULEURS_RECONFORT <- c("#1A9850", "#FF9933", "#D62728")
.COULEURS_ACC <- c(
  parcourable = "#2E7D32", accessible = "#9CCC65", non_accessible = "#C62828",
  accessible_cable = "#2E7D32", inaccessible = "#9E9E9E", inexploitable = "#455A64",
  non_defendable_pente = "#78909C", defendable_c1 = "#2E7D32",
  defendable_c2 = "#FDD835", defendable_c3 = "#FB8C00")
.COULEURS_REPLI <- c("#2E7D32", "#9CCC65", "#FDD835", "#FB8C00", "#C62828",
                     "#6D4C41", "#9E9E9E", "#455A64")

# Plus grand cote d'une image rendue (pixels) : au-dela, le raster est
# agrege avant d'etre colore.
.RASTER_MAX_PX <- 1600L
# Format des entrees du manifeste : a augmenter quand il change, pour que le
# cache des couches (cle `cle`) ne resserve pas d'anciennes entrees.
.FORMAT_RASTERS <- 2L
# Nombre maximal d'images d'une frise temporelle.
.FRISE_MAX <- 24L

.textes_rasters <- function(langue) .TEXTES_RASTERS[[if (identical(langue, "en")) "en" else "fr"]]

# Couleur d'une palette continue nommee (noms de grDevices::hcl.colors, ceux
# de RColorBrewer qu'utilise leaflet dans nemetonshiny).
.rampe <- function(palette, n = 9L, inverser = FALSE) {
  p <- tryCatch(grDevices::hcl.colors(n, palette), error = function(e) grDevices::hcl.colors(n, "viridis"))
  p <- substr(p, 1, 7)
  if (inverser) rev(p) else p
}

#' Colour a vector of raster values (pure function, tested)
#'
#' @param v Numeric values.
#' @param legende A `classes` legend (`classes`: list of `valeur`, `couleur`;
#'   a class without colour is transparent) or a `continu` legend (`min`,
#'   `max`, `couleurs`).
#' @return Character vector of `#RRGGBB` colours, `NA` where transparent.
#' @noRd
.colorer <- function(v, legende) {
  v <- as.numeric(v)
  out <- rep(NA_character_, length(v))
  if (identical(legende$type, "continu")) {
    ok <- is.finite(v)
    if (!any(ok)) return(out)
    t <- (v[ok] - legende$min) / max(legende$max - legende$min, .Machine$double.eps)
    t <- pmin(1, pmax(0, t))
    rampe <- grDevices::colorRamp(unlist(legende$couleurs))
    m <- round(rampe(t))
    out[ok] <- grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
    return(out)
  }
  vals <- vapply(legende$classes, function(k) as.numeric(k$valeur), 0)
  cols <- vapply(legende$classes, function(k) k$couleur %||% NA_character_, "")
  idx <- match(v, vals)
  out[!is.na(idx)] <- cols[idx[!is.na(idx)]]
  out
}

# Legende a classes ; `transparentes` : valeurs non peintes (sain, hors foret),
# gardees dans la legende sans couleur pour que la vue les explique.
.legende_classes <- function(valeurs, libelles, couleurs, transparentes = numeric()) {
  list(type = "classes", classes = lapply(seq_along(valeurs), function(i) {
    list(valeur = valeurs[i], libelle = libelles[i],
         couleur = if (valeurs[i] %in% transparentes) NULL else couleurs[i])
  }))
}

.legende_continue <- function(min, max, palette, inverser = FALSE, format = "nombre", unite = NULL) {
  list(type = "continu", min = min, max = max, couleurs = .rampe(palette, 9L, inverser),
       format = format, unite = unite)
}

# Rasters FORDEAD, FAST et RECONFORT d'un projet : zone de suivi tiree de
# metadata.json (`monitoring_zone_id`), comme dans l'application.
.zone_suivi <- function(chemin) {
  meta <- tryCatch(jsonlite::read_json(file.path(chemin, "metadata.json")), error = function(e) list())
  z <- suppressWarnings(as.integer(meta$monitoring_zone_id %||% NA))
  if (!is.na(z)) return(z)
  # Repli : la zone dont un dossier de cache existe.
  d <- list.files(file.path(chemin, "cache", "layers", c("fordead", "fast_alert_mask", "reconfort")),
                  pattern = "^zone_[0-9]+$")
  if (length(d)) as.integer(sub("^zone_", "", d[1])) else NA_integer_
}

#' Catalogue of the rasters cached in a project
#'
#' @param chemin Project folder.
#' @param langue `"fr"` or `"en"`.
#' @return list: `couches`, one entry per available layer (`id`, `theme`,
#'   `libelle`, `source`, `lire` (function returning a SpatRaster),
#'   `legende`, `date`, `frise` (function of a date, or NULL)), and
#'   `absentes`, the missing layer groups with the way to produce them.
#' @noRd
.catalogue_rasters <- function(chemin, langue = "fr") {
  tx <- .textes_rasters(langue)
  couches <- list()
  absentes <- list()
  ajouter <- function(x) couches[[x$id]] <<- x
  lay <- file.path(chemin, "cache", "layers")
  zone <- .zone_suivi(chemin)

  # FORDEAD : masque de severite 0-4 (classe 0 transparente) et date de
  # premiere anomalie ; la frise montre la severite des pixels detectes
  # avant chaque date (meme filtrage cumulatif que l'application).
  cd <- file.path(lay, "fordead")
  sev <- if (!is.na(zone) && dir.exists(cd)) tryCatch(
    nemeton::read_fordead_dieback_mask(NULL, zone_id = zone, cache_dir = cd), error = function(e) NULL)
  if (!is.null(sev)) {
    fa <- tryCatch(nemeton::read_fordead_layer(NULL, zone_id = zone, layer = "first_anomaly", cache_dir = cd),
                   error = function(e) NULL)
    leg <- .legende_classes(0:4, tx$sev, .COULEURS_SEVERITE, transparentes = 0)
    src_fd <- list.files(file.path(cd, paste0("zone_", zone)), recursive = TRUE, full.names = TRUE)
    ajouter(list(id = "fordead_severite", theme = "sante", libelle = tx$fordead_severite,
                 source = "FORDEAD (nemeton, Sentinel-2)", lire = function() sev, legende = leg,
                 sources = src_fd,
                 premiere = fa,
                 frise = if (!is.null(fa)) function(date) {
                   jours <- as.numeric(as.Date(date))
                   terra::ifel(fa <= jours, sev, NA)
                 }))
    if (!is.null(fa)) {
      rng <- range(terra::values(fa, mat = FALSE), na.rm = TRUE)
      if (all(is.finite(rng))) {
        ajouter(list(id = "fordead_premiere_anomalie", theme = "sante", libelle = tx$fordead_premiere,
                     source = "FORDEAD (nemeton, Sentinel-2)", lire = function() fa, sources = src_fd,
                     legende = .legende_continue(rng[1], rng[2], "RdYlBu", inverser = TRUE, format = "date")))
      }
    }
  } else absentes$fordead <- tx$absente$fordead

  # FAST : masques d'alerte en quartiles 1-4, un par indice et par mode ; le
  # plus recent de chaque.
  fast <- if (!is.na(zone)) list.files(file.path(lay, "fast_alert_mask", paste0("zone_", zone)),
                                       pattern = "^fast_alert_[A-Z]+_[a-z]+_.*\\.tif$", full.names = TRUE)
  if (length(fast)) {
    cle <- sub("^fast_alert_([A-Z]+)_([a-z]+)_.*$", "\\1|\\2", basename(fast))
    fast <- fast[order(file.mtime(fast), decreasing = TRUE)]
    cle <- sub("^fast_alert_([A-Z]+)_([a-z]+)_.*$", "\\1|\\2", basename(fast))
    for (i in which(!duplicated(cle))) {
      p <- strsplit(cle[i], "|", fixed = TRUE)[[1]]
      f <- fast[i]
      ajouter(list(id = tolower(paste("fast", p[1], p[2], sep = "_")), theme = "sante",
                   libelle = sprintf(tx$fast, p[1], p[2]), source = "FAST (nemeton, Sentinel-2)",
                   lire = local({ f <- f; function() terra::rast(f) }), sources = f,
                   legende = .legende_classes(0:4, c(tx$sev[1], tx$fast_cl), c(NA, .COULEURS_FAST), transparentes = 0),
                   date = format(file.mtime(f), "%Y-%m-%d")))
    }
  } else absentes$fast <- tx$absente$fast

  # RECONFORT : couches et palettes du manifeste du coeur.
  rc <- file.path(lay, "reconfort")
  man <- if (!is.na(zone) && dir.exists(rc)) tryCatch(
    nemeton::reconfort_cache_manifest(rc, zone_id = zone), error = function(e) NULL)
  n_rec <- 0L
  if (is.data.frame(man) && nrow(man)) {
    for (i in seq_len(nrow(man))) {
      m <- man[i, ]
      f <- Filter(file.exists, c(file.path(lay, m$path), file.path(rc, m$path), m$path))[1]
      if (is.na(f) || !nzchar(f %||% "")) next
      annee <- regmatches(m$path, regexpr("(19|20)[0-9]{2}(?=[^0-9]*\\.tif$)", m$path, perl = TRUE))
      leg <- if (isTRUE(m$categorical)) {
        .legende_classes(1:3, tx$rec_cl, .COULEURS_RECONFORT)
      } else {
        .legende_continue(m$vmin, m$vmax, if (is.na(m$palette)) "viridis" else m$palette,
                          inverser = isTRUE(m$reverse))
      }
      id <- switch(m$role, score = "reconfort_score", classification = "reconfort_classes",
                   probability = "reconfort_proba", paste0("reconfort_", m$id))
      ajouter(list(id = id, theme = "sante",
                   libelle = tx[[id]] %||% id, source = "RECONFORT (nemeton, DSF)",
                   lire = local({ f <- f; function() terra::rast(f)[[1]] }), sources = f,
                   legende = leg, date = if (length(annee)) annee else NULL))
      n_rec <- n_rec + 1L
    }
  }
  if (!n_rec) absentes$reconfort <- tx$absente$reconfort

  # Accessibilite : rasters categoriels a niveaux (terra), une couche par
  # moteur, plus les classes de distance de debardage.
  acc <- file.path(chemin, "cache", "accessibility")
  n_acc <- 0L
  for (id in c("acc_skidder", "acc_porteur", "acc_camion_dfci", "acc_classes_debardage")) {
    f <- file.path(acc, paste0(id, ".tif"))
    if (!file.exists(f)) next
    r <- tryCatch(terra::rast(f), error = function(e) NULL)
    if (is.null(r)) next
    leg <- .legende_niveaux(r, tx)
    ajouter(list(id = id, theme = "accessibilite", libelle = tx[[id]],
                 source = "foretaccess (nemetonshiny)",
                 lire = local({ f <- f; function() terra::rast(f) }), legende = leg, sources = f,
                 date = format(file.mtime(f), "%Y-%m-%d")))
    n_acc <- n_acc + 1L
  }
  if (!n_acc) absentes$accessibilite <- tx$absente$accessibilite

  # Regeneration : carte climatique bivariee (classe 1-100, palette 10 x 10
  # et bornes donnees par le .meta.json du coeur), une par source.
  cl <- list.files(file.path(chemin, "cache", "regeneration", "climat"),
                   pattern = "^carte_[a-z0-9]+_.*[^s]\\.tif$", full.names = TRUE)
  cl <- cl[!grepl("_tendances\\.tif$", cl)]
  for (f in cl) {
    meta <- tryCatch(jsonlite::read_json(sub("\\.tif$", ".meta.json", f), simplifyVector = TRUE),
                     error = function(e) NULL)
    if (is.null(meta$palette$colors) || !identical(meta$status %||% "ok", "ok")) next
    src <- toupper(sub("^carte_([a-z0-9]+)_.*$", "\\1", basename(f)))
    n <- as.integer(round(sqrt(length(meta$palette$colors))))
    ajouter(list(
      id = tolower(paste0("climat_", src)), theme = "regeneration",
      libelle = sprintf(tx$climat, src), source = paste(src, "(nemeton, r\u00e9g\u00e9n\u00e9ration)"),
      lire = local({ f <- f; function() terra::rast(f)[[1]] }),
      sources = c(f, sub("\\.tif$", ".meta.json", f)),
      legende = list(type = "bivarie", n = n,
                     classes = lapply(seq_along(meta$palette$colors), function(i) {
                       list(valeur = meta$palette$classes[i], couleur = meta$palette$colors[i])
                     }),
                     # classe = (rang Tmax - 1) * n + rang precipitations
                     # (nemeton, .climat_classer) : Tmax en ordonnee.
                     axe_x = list(nom = "\u0394P", unite = "mm", bornes = meta$bornes$precip),
                     axe_y = list(nom = "\u0394T\u00b0max", unite = "\u00b0C", bornes = meta$bornes$tmax)),
      date = if (length(meta$periode)) paste(meta$periode, collapse = "\u2013")))
  }
  regen <- file.path(chemin, "cache", "regeneration", "sensibilite.gpkg")
  if (!length(cl) && !file.exists(regen)) absentes$regeneration <- tx$absente$regeneration

  list(couches = couches, absentes = absentes, zone = zone)
}

# Legende d'un raster categoriel a niveaux (accessibilite) : libelles connus
# traduits, couleurs de l'application, « hors foret » transparent.
.legende_niveaux <- function(r, tx) {
  lv <- tryCatch(terra::levels(r)[[1]], error = function(e) NULL)
  if (!is.data.frame(lv) || !nrow(lv)) {
    v <- sort(unique(stats::na.omit(terra::values(r, mat = FALSE))))
    lv <- data.frame(value = v, classe = as.character(v))
  }
  codes <- as.numeric(lv[[1]])
  noms <- as.character(lv[[2]])
  ct <- tryCatch(terra::coltab(r)[[1]], error = function(e) NULL)
  cols <- rep(NA_character_, length(codes))
  if (is.data.frame(ct) && nrow(ct)) {
    j <- match(codes, ct[[1]])
    hex <- ifelse(is.na(j), NA, grDevices::rgb(ct$red[j], ct$green[j], ct$blue[j], maxColorValue = 255))
    if (length(unique(stats::na.omit(hex))) > 1L) cols <- hex
  }
  connues <- is.na(cols) & noms %in% names(.COULEURS_ACC)
  cols[connues] <- .COULEURS_ACC[noms[connues]]
  manque <- which(is.na(cols))
  cols[manque] <- .COULEURS_REPLI[((seq_along(manque) - 1L) %% length(.COULEURS_REPLI)) + 1L]
  libelles <- ifelse(noms %in% names(tx$acc), tx$acc[noms],
                     ifelse(grepl("^[0-9]+-[0-9]+$", noms), paste(sub("-", "\u2013", noms), "m"),
                            ifelse(grepl("^> ?[0-9]+$", noms), paste(noms, "m"), noms)))
  .legende_classes(codes, unname(libelles), cols, transparentes = codes[noms == "hors_foret"])
}

#' Render a raster as a georeferenced PNG for Leaflet
#'
#' @param r SpatRaster (first layer used).
#' @param legende Legend of the layer (`.colorer()`).
#' @param fichier PNG to write.
#' @param max_px Largest side of the image, in pixels.
#' @return list: `emprise` `[[sud, ouest], [nord, est]]` (WGS84, the
#'   `L.imageOverlay` bounds), `largeur`, `hauteur`, `taille_octets`.
#' @noRd
.rendre_png <- function(r, legende, fichier, max_px = .RASTER_MAX_PX) {
  r <- r[[1]]
  if (terra::is.factor(r)) r <- terra::as.int(r)
  continu <- identical(legende$type, "continu")
  facteur <- ceiling(max(dim(r)[1:2]) / max_px)
  if (facteur > 1) r <- terra::aggregate(r, facteur, fun = if (continu) "mean" else "modal", na.rm = TRUE)
  m <- terra::project(r, "EPSG:3857", method = if (continu) "bilinear" else "near")
  coul <- .colorer(terra::values(m, mat = FALSE), legende)
  nr <- terra::nrow(m)
  nc <- terra::ncol(m)
  rgb <- grDevices::col2rgb(ifelse(is.na(coul), "#000000", coul)) / 255
  img <- array(0, dim = c(nr, nc, 4))
  # terra rend les valeurs ligne par ligne, du nord au sud : l'ordre d'une image.
  for (k in 1:3) img[, , k] <- matrix(rgb[k, ], nrow = nr, ncol = nc, byrow = TRUE)
  img[, , 4] <- matrix(ifelse(is.na(coul), 0, 1), nrow = nr, ncol = nc, byrow = TRUE)
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  png::writePNG(img, fichier)
  e <- as.vector(terra::ext(m))
  coins <- sf::st_transform(sf::st_sfc(sf::st_point(c(e[["xmin"]], e[["ymin"]])),
                                       sf::st_point(c(e[["xmax"]], e[["ymax"]])), crs = 3857), 4326)
  xy <- round(sf::st_coordinates(coins), 6)
  list(emprise = list(c(xy[1, 2], xy[1, 1]), c(xy[2, 2], xy[2, 1])),
       largeur = nc, hauteur = nr, taille_octets = .taille(fichier))
}

#' Dates of a timeline (pure function, tested)
#'
#' @param jours Range of dates, in days since 1970.
#' @return Character vector of ISO dates: month ends over two years or less,
#'   year ends beyond, always ending on the last date; at most `.FRISE_MAX`.
#' @noRd
.dates_frise <- function(jours, max_n = .FRISE_MAX) {
  jours <- jours[is.finite(jours)]
  if (!length(jours)) return(character())
  d <- as.Date(range(jours), origin = "1970-01-01")
  mensuel <- as.numeric(d[2] - d[1]) <= 731
  debut <- as.Date(format(d[1], if (mensuel) "%Y-%m-01" else "%Y-01-01"))
  pas <- seq(debut, d[2], by = if (mensuel) "month" else "year")
  fins <- vapply(pas, function(x) {
    s <- seq(x, by = if (mensuel) "month" else "year", length.out = 2)[2] - 1
    as.character(min(s, d[2]))
  }, "")
  fins <- unique(c(fins, as.character(d[2])))
  if (length(fins) > max_n) fins <- unique(c(fins[round(seq(1, length(fins), length.out = max_n))], as.character(d[2])))
  fins
}

# Statistiques d'une couche par unite de gestion : part de surface par
# classe (couches a classes), ou moyenne, min et max (continues).
.stats_ug <- function(r, legende, ugs) {
  if (is.null(ugs) || !nrow(ugs)) return(NULL)
  r <- r[[1]]
  if (terra::is.factor(r)) r <- terra::as.int(r)
  v <- tryCatch(terra::extract(r, terra::vect(sf::st_transform(ugs, terra::crs(r))), ID = TRUE),
                error = function(e) NULL)
  if (is.null(v) || !nrow(v)) return(NULL)
  surf_px <- prod(terra::res(r)) / 1e4
  ids <- as.character(ugs$ug_id)
  out <- lapply(split(v[[2]], factor(v$ID, levels = seq_along(ids))), function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NULL)
    if (identical(legende$type, "continu")) {
      return(list(moyenne = signif(mean(x), 4), min = signif(min(x), 4), max = signif(max(x), 4),
                  n_px = length(x)))
    }
    t <- table(x)
    list(parts = as.list(stats::setNames(round(as.numeric(t) / length(x), 4), names(t))),
         surface_ha = round(length(x) * surf_px, 2))
  })
  stats::setNames(out, ids)[!vapply(out, is.null, TRUE)]
}

# Serie de la frise par unite : surface (ha) touchee (classe >= 1) a chaque
# date, a partir de la severite et de la date de premiere anomalie.
.serie_frise_ug <- function(sev, fa, dates, ugs) {
  if (is.null(ugs) || !nrow(ugs) || !length(dates)) return(NULL)
  v <- tryCatch(terra::extract(c(sev[[1]], fa[[1]]), terra::vect(sf::st_transform(ugs, terra::crs(sev))), ID = TRUE),
                error = function(e) NULL)
  if (is.null(v)) return(NULL)
  surf_px <- prod(terra::res(sev)) / 1e4
  jours <- as.numeric(as.Date(dates))
  ids <- as.character(ugs$ug_id)
  touches <- !is.na(v[[2]]) & v[[2]] >= 1 & !is.na(v[[3]])
  out <- lapply(seq_along(ids), function(i) {
    x <- v[[3]][v$ID == i & touches]
    round(vapply(jours, function(j) sum(x <= j) * surf_px, 0), 2)
  })
  stats::setNames(out, ids)
}

# Unites de gestion du projet, en WGS84 simplifie, avec les variables de la
# regeneration (sensibilite.gpkg de nemetonshiny) quand elles existent.
.ugs_rasters <- function(id, chemin, langue) {
  lu <- tryCatch(.lire_projet(id, langue), error = function(e) NULL)
  g <- lu$familles %||% lu$indicateurs
  if (is.null(g) || !inherits(g, "sf")) {
    g <- tryCatch(.ns("load_parcels")(id), error = function(e) NULL)
    if (is.null(g)) return(NULL)
    if (!"ug_id" %in% names(g)) g$ug_id <- as.character(g$id %||% seq_len(nrow(g)))
  }
  refs <- if ("cadastral_refs" %in% names(g)) g$cadastral_refs
  idt <- .identites_ug(as.character(g$ug_id), g$label %||% rep(NA_character_, nrow(g)), refs)
  x <- sf::st_sf(ug_id = as.character(g$ug_id), label = idt$label, parcelles = idt$parcelles,
                 surface_ha = round(as.numeric(sf::st_area(sf::st_transform(g, 2154))) / 1e4, 2),
                 geometry = sf::st_geometry(g))
  f <- file.path(chemin, "cache", "regeneration", "sensibilite.gpkg")
  if (file.exists(f)) {
    s <- tryCatch(sf::st_drop_geometry(sf::st_read(f, quiet = TRUE)), error = function(e) NULL)
    vars <- intersect(names(.TEXTES_RASTERS$fr$regen_var), names(s))
    if (length(vars) && "ug_id" %in% names(s)) {
      j <- match(x$ug_id, as.character(s$ug_id))
      for (v in vars) x[[paste0("regen_", v)]] <- signif(suppressWarnings(as.numeric(s[[v]][j])), 4)
    }
  }
  x
}

# Empreinte d'un ensemble de fichiers (date de modification la plus recente
# et nombre) : change des que l'un d'eux est recalcule.
.cle_fichiers <- function(fichiers) {
  fichiers <- fichiers[file.exists(fichiers)]
  if (!length(fichiers)) return("0")
  paste(format(max(as.numeric(file.mtime(fichiers))), digits = 15), length(fichiers), sep = ":")
}

# Entree du manifeste d'une couche, sans les fonctions de lecture.
.entree_couche <- function(cc, rendu, image, extra = list()) {
  c(list(id = cc$id, theme = cc$theme, libelle = cc$libelle, source = cc$source,
         date = cc$date, legende = cc$legende, image = image,
         emprise = rendu$emprise, largeur = rendu$largeur, hauteur = rendu$hauteur), extra)
}

#' Data of the raster views (forest health, accessibility, regeneration)
#'
#' Renders the rasters nemetonshiny cached in a project as georeferenced PNG
#' images (Web Mercator, with their WGS84 bounds for `L.imageOverlay`):
#' FORDEAD, FAST and RECONFORT (forest health), harvesting accessibility, and
#' the regeneration climate map. Nothing is recomputed.
#'
#' @param projet Project id or name.
#' @param couche `NULL` for the whole bundle (manifest `rasters.json`, unit
#'   outlines `ugs.geojson` and every image, to join to the artifact), or a
#'   layer id from that manifest to render only this layer.
#' @param date With `couche = "fordead_severite"`: show only the pixels first
#'   detected on or before this date (`"YYYY-MM-DD"`).
#' @param langue `"fr"` or `"en"`.
#' @param inclure_png Also return the image inline, base64 encoded
#'   (`png_base64`), for a view calling the connector (one layer only).
#' @param max_px Largest side of an image, in pixels.
#' @param theme Theme the view opens on: `"sante"`, `"accessibilite"` or
#'   `"regeneration"` (bundle only; default: the first with a layer).
#' @return JSON. Bundle: `fichiers` (`manifeste`, `ugs`, `images`, all to
#'   join to the artifact under `rasters/` for images), `couches` (id, theme,
#'   label), `absentes` (missing groups, with how to produce them). One
#'   layer: its manifest entry, `fichier`, and `png_base64` on demand.
#' @export
vue_rasters <- function(projet, couche = NULL, date = NULL, langue = "fr",
                        inclure_png = FALSE, max_px = .RASTER_MAX_PX, theme = NULL) {
  .mcp_call({
    if (!requireNamespace("png", quietly = TRUE)) {
      .abort("Le paquet {.pkg png} est requis pour rendre les rasters.", "nemetonmcp_dependance_manquante")
    }
    id <- .resoudre_projet(projet)
    langue <- if (identical(langue, "en")) "en" else "fr"
    chemin <- .ns("get_project_path")(id)
    cat_r <- .catalogue_rasters(chemin, langue)
    dossier <- .dossier_vues("projets", id)
    dossier_img <- .dossier_vues("projets", id, "rasters")
    max_px <- as.integer(max_px %||% .RASTER_MAX_PX)

    if (!is.null(couche) && nzchar(couche)) {
      cc <- cat_r$couches[[couche]]
      if (is.null(cc)) {
        .abort(c("Couche {.val {couche}} absente du projet {.val {id}}.",
                 i = "Couches disponibles : {.val {names(cat_r$couches)}}."),
               "nemetonmcp_couche_absente", candidats = names(cat_r$couches))
      }
      r <- cc$lire()
      nom <- cc$id
      if (!is.null(date) && nzchar(date)) {
        if (is.null(cc$frise)) .abort("La couche {.val {couche}} n'a pas de frise temporelle.", "nemetonmcp_couche_absente")
        r <- cc$frise(date)
        nom <- paste0(cc$id, "_", gsub("-", "", date))
      }
      fichier <- file.path(dossier_img, paste0(nom, ".png"))
      rendu <- .rendre_png(r, cc$legende, fichier, max_px)
      entree <- .entree_couche(cc, rendu, paste0("rasters/", basename(fichier)), list(date_frise = date))
      c(list(projet = id), entree, list(
        fichier = normalizePath(fichier), taille_octets = rendu$taille_octets,
        png_base64 = if (isTRUE(inclure_png))
          openssl::base64_encode(readBin(fichier, "raw", rendu$taille_octets), linebreaks = FALSE)))
    } else {
      theme <- if (!is.null(theme) && theme %in% c("sante", "accessibilite", "regeneration")) theme
      .paquet_rasters(id, chemin, cat_r, dossier, dossier_img, langue, max_px, theme)
    }
  })
}

# Paquet complet de vue_rasters (voir sa documentation).
.paquet_rasters <- function(id, chemin, cat_r, dossier, dossier_img, langue, max_px, theme = NULL) {
  # Paquet complet : manifeste, unites de gestion et toutes les images. Une
  # couche dont les sources, les unites et les reglages n'ont pas change
  # reprend son entree et ses images du manifeste precedent (cle `cle`).
  fichier_man <- file.path(dossier, "rasters.json")
  avant <- tryCatch(jsonlite::read_json(fichier_man), error = function(e) NULL)
  avant <- stats::setNames(avant$couches, vapply(avant$couches, function(e) e$id %||% "", ""))
  cle_ug <- .cle_fichiers(file.path(chemin, "data", c("parcels.gpkg", "ugs.json", "indicators.parquet")))
  ugs <- NULL
  lire_ugs <- function() { if (is.null(ugs)) ugs <<- .ugs_rasters(id, chemin, langue); ugs }
  insee <- tryCatch(as.character(.ns("load_parcels")(id)$code_insee[1]), error = function(e) NA)
  images <- character()
  entrees <- lapply(cat_r$couches, function(cc) {
    cle <- paste(.FORMAT_RASTERS, .cle_fichiers(cc$sources), cle_ug, max_px, langue,
                 as.character(utils::packageVersion("nemetonmcp")), sep = "|")
    e <- avant[[cc$id]]
    imgs <- file.path(dossier, c(e$image, vapply(e$frise %||% list(), function(x) x$image %||% "", "")))
    if (!is.null(e) && identical(e$cle, cle) && length(e$image) && all(file.exists(imgs))) {
      images <<- c(images, imgs)
      return(e)
    }
    r <- tryCatch(cc$lire(), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    fichier <- file.path(dossier_img, paste0(cc$id, ".png"))
    rendu <- tryCatch(.rendre_png(r, cc$legende, fichier, max_px), error = function(e) NULL)
    if (is.null(rendu)) return(NULL)
    images <<- c(images, fichier)
    extra <- list(par_ug = .stats_ug(r, cc$legende, lire_ugs()))
    if (!is.null(cc$frise)) {
      dates <- .dates_frise(range(terra::values(cc$premiere, mat = FALSE), na.rm = TRUE))
      frise <- lapply(dates, function(d) {
        f <- file.path(dossier_img, paste0(cc$id, "_", gsub("-", "", d), ".png"))
        rd <- tryCatch(.rendre_png(cc$frise(d), cc$legende, f, max_px), error = function(e) NULL)
        if (is.null(rd)) return(NULL)
        images <<- c(images, f)
        list(date = d, image = paste0("rasters/", basename(f)))
      })
      extra$frise <- Filter(Negate(is.null), frise)
      extra$serie_ug <- list(dates = dates, surfaces_ha = .serie_frise_ug(r, cc$premiere, dates, lire_ugs()))
    }
    extra$cle <- cle
    .entree_couche(cc, rendu, paste0("rasters/", basename(fichier)), extra)
  })
  entrees <- unname(Filter(Negate(is.null), entrees))

  tx <- .textes_rasters(langue)
  ugs <- lire_ugs()
  vars_regen <- if (!is.null(ugs)) sub("^regen_", "", grep("^regen_", names(ugs), value = TRUE)) else character()
  lu <- tryCatch(.lire_projet(id, langue), error = function(e) NULL)
  manifeste <- list(
    project_id = id, name = lu$synthese$name %||% id, langue = langue, genere_a = .maintenant(),
    theme_initial = theme,
    themes = lapply(c("sante", "accessibilite", "regeneration"), function(t) list(id = t, libelle = tx[[t]])),
    couches = entrees,
    variables_ug = lapply(vars_regen, function(v) list(id = paste0("regen_", v), theme = "regeneration",
                                                        libelle = unname(tx$regen_var[v]))),
    absentes = cat_r$absentes
  )
  writeLines(.json(manifeste), fichier_man, useBytes = TRUE)
  # Images d'une couche disparue ou d'une ancienne frise : retirees.
  vieilles <- setdiff(normalizePath(list.files(dossier_img, pattern = "\\.png$", full.names = TRUE)),
                      normalizePath(images))
  unlink(vieilles)
  fichier_ugs <- NULL
  if (!is.null(ugs)) {
    crs_m <- .crs_metrique(if (is.na(insee)) "" else insee)
    fichier_ugs <- .ecrire_geojson(.simplifier_wgs84(ugs, 1, crs_m), file.path(dossier, "ugs.geojson"))
  }
  list(projet = id, dossier = dossier,
       fichiers = list(manifeste = normalizePath(fichier_man), ugs = fichier_ugs,
                       images = unname(normalizePath(images))),
       taille_octets = sum(.taille(c(fichier_man, fichier_ugs, images))),
       couches = lapply(entrees, function(e) list(id = e$id, theme = e$theme, libelle = e$libelle,
                                                  date = e$date, n_frise = length(e$frise))),
       variables_ug = vapply(manifeste$variables_ug, `[[`, "", "id"),
       absentes = cat_r$absentes)
}
