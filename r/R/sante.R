# Outil `alertes_sanitaires(projet)` : les alertes de sante des forets par
# unite de gestion, pour la conversation (specs/ARCHITECTURE-nemeton-llm.md,
# « Suivi sanitaire : Conversation (alertes), Atelier (cartes pixel) »).
#
# Meme source que la vue Cartes (vue_rasters) : les couches que nemetonshiny
# a deja calculees et mises en cache (FORDEAD, FAST, RECONFORT), lues par
# .catalogue_rasters(). Rien n'est recalcule et aucune image n'est rendue :
# seules les parts de surface par classe sont extraites, par UGF.

.TEXTES_SANTE <- list(
  fr = list(
    forte = "forte", moderee = "modérée",
    fd_touchee = "FORDEAD : %s ha dépérissants (%s %%)",
    fd_forte = "FORDEAD : sévérité forte sur %s %%",
    fd_depuis = "depuis %s",
    fast = "FAST %s : alerte tr\u00e8s forte sur %s %%",
    rec = "RECONFORT : %s %% dépérissant",
    rec_tres = "RECONFORT : %s %% très dépérissant",
    coupe = "sol nu (coupe rase) : %s ha",
    aucune = "%s : aucune alerte sanitaire sur %d UGF (%s).",
    resume = "%s : %d UGF en alerte forte, %d en alerte modérée, sur %d (%s).",
    pire = " La plus touchée : %s (%s).",
    sans_couche = "Aucune couche de santé des forêts calculée pour ce projet."),
  en = list(
    forte = "high", moderee = "moderate",
    fd_touchee = "FORDEAD: %s ha declining (%s %%)",
    fd_forte = "FORDEAD: high severity on %s %%",
    fd_depuis = "since %s",
    fast = "FAST %s: very high alert on %s %%",
    rec = "RECONFORT: %s %% declining",
    rec_tres = "RECONFORT: %s %% severely declining",
    coupe = "bare soil (clear-cut): %s ha",
    aucune = "%s: no forest health alert on %d units (%s).",
    resume = "%s: %d units on high alert, %d on moderate alert, out of %d (%s).",
    pire = " Most affected: %s (%s).",
    sans_couche = "No forest health layer computed for this project.")
)

# Seuils d'alerte, en part de la surface observee de l'UGF (ou en hectares).
# Forte : deperissement FORDEAD etendu ou severe, ou RECONFORT tres
# deperissant sur une bonne part ; moderee : signal FORDEAD naissant, alerte
# FAST tres forte etendue ou RECONFORT deperissant. Les masques FAST sont en
# quartiles (classes relatives : la moitie des pixels alertes est toujours en
# classe 3 ou 4) : seule la classe 4 compte, et jamais seule pour une alerte
# forte. Le sol nu (coupe rase) n'est jamais une alerte.
.SEUILS_ALERTE <- list(
  forte = list(fd_part = 0.10, fd_ha = 1, fd_forte = 0.05, rec_tres = 0.25),
  moderee = list(fd_part = 0.02, fast_tres_forte = 0.30, rec = 0.25)
)

.pct <- function(x, langue) .nombre(100 * x, 0, langue)

# Somme des parts des classes `classes` (noms de .stats_ug : "0", "1"...).
.part_classes <- function(parts, classes) {
  if (is.null(parts)) return(0)
  v <- unlist(parts[intersect(as.character(classes), names(parts))])
  if (length(v)) sum(as.numeric(v)) else 0
}

# Date de premiere anomalie FORDEAD par UGF : la plus ancienne des pixels
# deperissants (severite 1 a 3, le sol nu exclu).
.premiere_anomalie_ug <- function(sev, fa, ugs) {
  if (is.null(fa) || is.null(ugs) || !nrow(ugs)) return(NULL)
  v <- tryCatch(terra::extract(c(sev[[1]], fa[[1]]), terra::vect(sf::st_transform(ugs, terra::crs(sev))), ID = TRUE),
                error = function(e) NULL)
  if (is.null(v)) return(NULL)
  ids <- as.character(ugs$ug_id)
  ok <- !is.na(v[[2]]) & v[[2]] >= 1 & v[[2]] <= 3 & !is.na(v[[3]])
  out <- lapply(seq_along(ids), function(i) {
    x <- v[[3]][v$ID == i & ok]
    if (length(x)) format(as.Date(min(x), origin = "1970-01-01"), "%Y-%m-%d")
  })
  stats::setNames(out, ids)
}

# Mesures de sante par UGF, a partir du catalogue des rasters (couches du
# theme sante). Rend list(par_ug = <ug_id -> list(fordead, fast, reconfort)>,
# sources = <couches lues>).
.mesures_sante <- function(cat_r, ugs) {
  couches <- Filter(function(cc) identical(cc$theme, "sante"), cat_r$couches)
  par_ug <- stats::setNames(lapply(as.character(ugs$ug_id), function(i) list()), as.character(ugs$ug_id))
  sources <- list()
  noter <- function(cc, date = cc$date) {
    sources[[length(sources) + 1L]] <<- Filter(Negate(is.null), list(id = cc$id, libelle = cc$libelle, date = date))
  }

  fd <- couches$fordead_severite
  if (!is.null(fd)) {
    sev <- fd$lire()
    s <- .stats_ug(sev, fd$legende, ugs)
    pa <- .premiere_anomalie_ug(sev, fd$premiere, ugs)
    derniere <- if (!is.null(fd$premiere)) {
      m <- suppressWarnings(max(terra::values(fd$premiere, mat = FALSE), na.rm = TRUE))
      if (is.finite(m)) format(as.Date(m, origin = "1970-01-01"), "%Y-%m-%d")
    }
    noter(fd, derniere)
    for (i in names(s)) {
      x <- s[[i]]
      surf <- as.numeric(x$surface_ha %||% 0)
      part <- .part_classes(x$parts, 1:3)
      par_ug[[i]]$fordead <- list(
        part_touchee = round(part, 4), surface_touchee_ha = round(part * surf, 2),
        part_forte = round(.part_classes(x$parts, 3), 4),
        coupe_rase_ha = round(.part_classes(x$parts, 4) * surf, 2),
        surface_observee_ha = surf,
        premiere_anomalie = pa[[i]])
    }
  }

  for (cc in couches[grepl("^fast_", names(couches))]) {
    s <- .stats_ug(cc$lire(), cc$legende, ugs)
    noter(cc)
    p <- strsplit(sub("^fast_", "", cc$id), "_", fixed = TRUE)[[1]]
    nom <- sprintf("%s (%s)", toupper(p[1]), paste(p[-1], collapse = "_"))
    for (i in names(s)) {
      par_ug[[i]]$fast[[nom]] <- list(part_forte = round(.part_classes(s[[i]]$parts, 3:4), 4),
                                      part_tres_forte = round(.part_classes(s[[i]]$parts, 4), 4))
    }
  }

  rc <- couches$reconfort_classes
  if (!is.null(rc)) {
    s <- .stats_ug(rc$lire(), rc$legende, ugs)
    noter(rc)
    for (i in names(s)) {
      par_ug[[i]]$reconfort <- list(part_deperissant = round(.part_classes(s[[i]]$parts, 2:3), 4),
                                    part_tres_deperissant = round(.part_classes(s[[i]]$parts, 3), 4))
    }
  }
  list(par_ug = par_ug, sources = sources)
}

#' Alert level of a management unit (pure function, tested)
#'
#' @param m Health measures of one unit (`fordead`, `fast`, `reconfort`).
#' @param langue `"fr"` or `"en"`.
#' @return list: `niveau` (`"forte"`, `"moderee"` or `"aucune"`), `motifs`
#'   (sentences), `intensite` (largest share affected, for sorting).
#' @noRd
.niveau_alerte <- function(m, langue = "fr", seuils = .SEUILS_ALERTE) {
  tx <- .TEXTES_SANTE[[if (identical(langue, "en")) "en" else "fr"]]
  fo <- seuils$forte
  mo <- seuils$moderee
  forte <- moderee <- FALSE
  motifs <- character()
  fd <- m$fordead
  if (!is.null(fd)) {
    part <- fd$part_touchee %||% 0
    ha <- fd$surface_touchee_ha %||% 0
    if (part >= fo$fd_part || ha >= fo$fd_ha || (fd$part_forte %||% 0) >= fo$fd_forte) forte <- TRUE
    else if (part >= mo$fd_part) moderee <- TRUE
    if (part >= mo$fd_part || ha >= fo$fd_ha) {
      motifs <- c(motifs, paste0(sprintf(tx$fd_touchee, .nombre(ha, 1, langue), .pct(part, langue)),
                                 if (!is.null(fd$premiere_anomalie)) paste0(", ", sprintf(tx$fd_depuis, substr(fd$premiere_anomalie, 1, 7)))))
    }
    if ((fd$part_forte %||% 0) >= fo$fd_forte) motifs <- c(motifs, sprintf(tx$fd_forte, .pct(fd$part_forte, langue)))
    if ((fd$coupe_rase_ha %||% 0) >= 0.1) motifs <- c(motifs, sprintf(tx$coupe, .nombre(fd$coupe_rase_ha, 1, langue)))
  }
  intensite <- c(fd$part_touchee %||% 0, m$reconfort$part_tres_deperissant %||% 0)
  for (nom in names(m$fast)) {
    p <- m$fast[[nom]]$part_tres_forte %||% 0
    if (p >= mo$fast_tres_forte) {
      moderee <- TRUE
      motifs <- c(motifs, sprintf(tx$fast, nom, .pct(p, langue)))
      intensite <- c(intensite, p / 2)
    }
  }
  rc <- m$reconfort
  if (!is.null(rc)) {
    if ((rc$part_tres_deperissant %||% 0) >= fo$rec_tres) {
      forte <- TRUE
      motifs <- c(motifs, sprintf(tx$rec_tres, .pct(rc$part_tres_deperissant, langue)))
    } else if ((rc$part_deperissant %||% 0) >= mo$rec) {
      moderee <- TRUE
      motifs <- c(motifs, sprintf(tx$rec, .pct(rc$part_deperissant, langue)))
    }
  }
  list(niveau = if (forte) "forte" else if (moderee) "moderee" else "aucune",
       motifs = motifs, intensite = max(intensite))
}

#' Health alerts of a project (pure function, tested)
#'
#' @param par_ug Measures per unit (`.mesures_sante()$par_ug`).
#' @param ugs data.frame with `ug_id`, `label`, `surface_ha`.
#' @param nom Project name.
#' @param sources Layers read (`id`, `libelle`, `date`).
#' @return list: `n_ugf`, `n_forte`, `n_moderee`, `alertes` (units on
#'   alert, high first, then by share affected), `ugf_saines`, `texte`.
#' @noRd
.synthese_alertes <- function(par_ug, ugs, nom, sources, langue = "fr") {
  tx <- .TEXTES_SANTE[[if (identical(langue, "en")) "en" else "fr"]]
  ids <- as.character(ugs$ug_id)
  niveaux <- lapply(ids, function(i) .niveau_alerte(par_ug[[i]] %||% list(), langue))
  niv <- vapply(niveaux, `[[`, "", "niveau")
  ordre <- order(match(niv, c("forte", "moderee", "aucune")),
                 -vapply(niveaux, function(n) as.numeric(n$intensite), 0))
  alertes <- lapply(ordre[niv[ordre] != "aucune"], function(k) {
    c(list(ug_id = ids[k], label = as.character(ugs$label[k]),
           surface_ha = ugs$surface_ha[k], niveau = niv[k],
           motifs = as.list(niveaux[[k]]$motifs)),
      par_ug[[ids[k]]])
  })
  libelles <- paste(unique(vapply(sources, function(s) sub(" ·.*$", "", s$libelle), "")), collapse = ", ")
  n_f <- sum(niv == "forte")
  n_m <- sum(niv == "moderee")
  texte <- if (!length(sources)) {
    tx$sans_couche
  } else if (!length(alertes)) {
    sprintf(tx$aucune, nom, length(ids), libelles)
  } else {
    a <- alertes[[1]]
    paste0(sprintf(tx$resume, nom, n_f, n_m, length(ids), libelles),
           sprintf(tx$pire, a$label, paste(unlist(a$motifs), collapse = " ; ")))
  }
  list(n_ugf = length(ids), n_forte = n_f, n_moderee = n_m,
       alertes = alertes, ugf_saines = length(ids) - n_f - n_m, texte = texte)
}

#' Forest health alerts of a project
#'
#' Per management unit, from the forest health layers nemetonshiny already
#' computed and cached (FORDEAD dieback severity and first anomaly date, FAST
#' alert masks, RECONFORT health classes): share and area declining, date of
#' the first anomaly, and an alert level (`forte`, `moderee`). Nothing is
#' recomputed and no image is rendered; for the maps, open the Cartes view
#' (`vue_rasters`).
#'
#' @param projet Project id or name.
#' @param langue `"fr"` or `"en"`.
#' @return JSON: `n_ugf`, `n_forte`, `n_moderee`, `alertes` (units on alert,
#'   with `motifs` and the measures per layer), `sources` (layers read and
#'   their date), `absentes` (missing layers and how to produce them),
#'   `seuils`, and a one-paragraph `texte`.
#' @export
alertes_sanitaires <- function(projet, langue = "fr") {
  .mcp_call(langue = langue, {
    id <- .resoudre_projet(projet)
    langue <- if (identical(langue, "en")) "en" else "fr"
    chemin <- .ns("get_project_path")(id)
    cat_r <- .catalogue_rasters(chemin, langue)
    absentes <- cat_r$absentes[intersect(names(cat_r$absentes), c("fordead", "fast", "reconfort"))]
    ugs <- .ugs_rasters(id, chemin, langue)
    if (is.null(ugs) || !nrow(ugs)) {
      .abort("Projet {.val {id}} : aucune unité de gestion lisible.", "nemetonmcp_ug_introuvable")
    }
    mesures <- .mesures_sante(cat_r, ugs)
    nom <- tryCatch(nemetonshiny::projet_etat(id)$nom, error = function(e) NULL) %||% id
    s <- .synthese_alertes(mesures$par_ug, sf::st_drop_geometry(ugs), nom, mesures$sources, langue)
    c(list(projet = id, nom = nom), s,
      list(sources = mesures$sources, absentes = if (length(absentes)) absentes,
           seuils = .SEUILS_ALERTE,
           suite = "vue_rasters (carte), url_app (onglet Suivi sanitaire)"))
  })
}
