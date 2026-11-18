# Outil `regen_essences(projet, ug, top_n)` : les essences les mieux
# adaptees a la regeneration, par unite de gestion, pour la conversation
# (specs/ARCHITECTURE-nemeton-llm.md, « Regeneration : Conversation »,
# outil `regen_essences`).
#
# Classement deterministe du coeur (`nemeton::regen_rank_species()`, spec 039
# de nemetonshiny : tolerances des essences contre conditions de station),
# nourri par les sorties que l'analyse de regeneration de nemetonshiny a mises
# en cache dans le projet (`cache/regeneration/` : sensibilite.gpkg pour la
# chaleur et le VPD, biljou.gpkg pour le bilan hydrique, r7.gpkg pour le gel
# tardif). Rien n'est recalcule ; sans analyse, l'outil le dit. Meme
# classement que l'onglet reGeneration, sans l'axe d'ombrage (le LAI du
# moteur n'est pas conserve sur disque).

.TEXTES_REGEN <- list(
  fr = list(
    resume = "%s : essences les mieux adapt\u00e9es sur %d UGF : %s.",
    tete = "%s (en t\u00eate dans %d UGF)",
    facteur = " Facteur limitant le plus fr\u00e9quent : %s.",
    une = "%s, %s : %s.",
    essence = "%s (%s/100, limit\u00e9 par %s)",
    methode = paste0("Classement d\u00e9terministe du c\u0153ur nemeton (tol\u00e9rances des essences contre conditions de station : chaleur et VPD, s\u00e9cheresse du sol, gel tardif), sans l'axe d'ombrage ; essences envahissantes exclues. ",
                     "Station tir\u00e9e du climat observ\u00e9 (\u00e9t\u00e9s rejou\u00e9s par microclimf sous couvert, printemps pour le gel tardif) : ce n'est pas une projection climatique. ",
                     "Froid d'hiver \u00e9liminatoire (filtre du c\u0153ur, `regen_tmin_decennale()`) : une essence dont la tenue au froid est moins bonne que le minimum d\u00e9cennal de l'UGF (10e percentile des minimums annuels E-OBS 1991-2020, \u00ab un hiver sur dix descend \u00e0 \u00bb) n'est pas class\u00e9e. ",
                     "Thermophilie 8-9 (\u00e9chelle 1-9 du c\u0153ur) : essence m\u00e9diterran\u00e9enne ; la meilleure essence non m\u00e9diterran\u00e9enne est donn\u00e9e en plus."),
    climat = " Station : climat observ\u00e9 (\u00e9t\u00e9s %s, Tmax sous couvert de l'\u00e9t\u00e9 %s, retenu comme caniculaire), pas une projection.",
    climat_inconnu = " Station : climat observ\u00e9 r\u00e9cent, pas une projection.",
    froid = " \u00c9cart\u00e9es pour le froid d'hiver (minimum d\u00e9cennal %s \u00b0C) : %s.",
    froid_une = "%s (tient %s \u00b0C)",
    locale = " Meilleure essence non m\u00e9diterran\u00e9enne : %s.",
    locale_une = "%s (%s/100)",
    aides = " Class\u00e9es plus haut mais non \u00e9ligibles en essence objectif aux aides de l'\u00c9tat en Bourgogne-Franche-Comt\u00e9 : %s.",
    statuts_aides = list(accompagnement = "en accompagnement seulement", hors_liste = "hors liste"),
    methode_aides = " Seules les essences \u00e9ligibles en essence objectif aux aides de l'\u00c9tat en Bourgogne-Franche-Comt\u00e9 sont class\u00e9es (arr\u00eat\u00e9 r\u00e9gional MFR du 6 juillet 2026, annexe 1.1) ; aides = FALSE pour le classement \u00e9cologique seul.",
    facteurs = c(chaleur = "la chaleur", vpd = "la s\u00e9cheresse de l'air", secheresse = "la s\u00e9cheresse",
                 gel = "le gel tardif", ombre = "l'ombre", froid_hiver = "le froid d'hiver"),
    froid_absent = " Froid d'hiver non appliqu\u00e9 : le minimum d\u00e9cennal n'a pas pu \u00eatre calcul\u00e9 (E-OBS indisponible), les essences g\u00e9lives ne sont pas \u00e9cart\u00e9es.",
    avert_d_tmax = "d_tmax est n\u00e9gatif dans %d UGF : chaque cat\u00e9gorie d'ann\u00e9e ne repose que sur une ann\u00e9e simul\u00e9e, la surchauffe de l'ann\u00e9e \u00ab moyenne \u00bb peut d\u00e9passer celle de l'ann\u00e9e \u00ab caniculaire \u00bb ; ne pas y lire un effet du couvert.",
    avert_gel = "Gel tardif (r7_gel_days) fond\u00e9 sur %d printemps seulement : indicatif, fragile.",
    avert_confiance = "`confiance` mesure la fiabilit\u00e9 des traits de l'essence (sources de la table du c\u0153ur), pas la robustesse de la station.",
    aucune_froid = "aucune essence ne tient le froid d'hiver de l'UGF",
    sans_station = "conditions de station inconnues"),
  en = list(
    resume = "%s: best-suited species over %d units: %s.",
    tete = "%s (first in %d units)",
    facteur = " Most frequent limiting factor: %s.",
    une = "%s, %s: %s.",
    essence = "%s (%s/100, limited by %s)",
    methode = paste0("Deterministic ranking from the nemeton core (species tolerances against site conditions: heat and VPD, soil drought, late frost), without the shade axis; invasive species excluded. ",
                     "Site conditions come from the observed climate (summers replayed by microclimf under canopy, springs for late frost): this is not a climate projection. ",
                     "Winter cold as an elimination criterion (core filter, `regen_tmin_decennale()`): a species less hardy than the unit's ten-year minimum (10th percentile of the annual E-OBS minimums, 1991-2020, one winter in ten falls to it) is not ranked. ",
                     "Thermophily 8-9 (core scale 1-9): Mediterranean species; the best non-Mediterranean species is given as well."),
    climat = " Site: observed climate (summers %s, under-canopy T max of summer %s, taken as the heatwave year), not a projection.",
    climat_inconnu = " Site: recent observed climate, not a projection.",
    froid = " Set aside for winter cold (ten-year minimum %s \u00b0C): %s.",
    froid_une = "%s (hardy to %s \u00b0C)",
    locale = " Best non-Mediterranean species: %s.",
    locale_une = "%s (%s/100)",
    aides = " Ranked higher but not eligible as main species for state aid in Bourgogne-Franche-Comte: %s.",
    statuts_aides = list(accompagnement = "companion species only", hors_liste = "not listed"),
    methode_aides = " Only species eligible as main species for state aid in Bourgogne-Franche-Comte are ranked (regional MFR decree of 6 July 2026, annex 1.1); aides = FALSE for the ecological ranking alone.",
    facteurs = c(chaleur = "heat", vpd = "air dryness", secheresse = "drought", gel = "late frost", ombre = "shade", froid_hiver = "winter cold"),
    froid_absent = " Winter cold not applied: the ten-year minimum could not be computed (E-OBS unavailable), frost-sensitive species are not set aside.",
    avert_d_tmax = "d_tmax is negative in %d units: each year category rests on a single simulated year, so the 'average' year's overheating can exceed the 'heatwave' year's; do not read it as a canopy effect.",
    avert_gel = "Late frost (r7_gel_days) rests on %d spring(s) only: indicative, fragile.",
    avert_confiance = "`confiance` measures the reliability of the species traits (sources of the core table), not the robustness of the site.",
    aucune_froid = "no species stands the unit's winter cold",
    sans_station = "unknown site conditions")
)

# Colonnes de station lues dans chaque fichier du cache de regeneration.
.COLONNES_REGEN <- list(
  sensibilite.gpkg = c("tmax_moyenne", "d_tmax", "vpd_canicule", "couverture_pct",
                       "sensibilite", "priorite", "indice_priorite_regen"),
  biljou.gpkg = c("rew_min", "njstress"),
  r7.gpkg = c("r7_gel_days")
)

# Unites de station : les UGF de sensibilite.gpkg, completees par ug_id avec
# les colonnes des autres fichiers presents. NULL sans analyse.
.unites_regen <- function(dossier) {
  f0 <- file.path(dossier, "sensibilite.gpkg")
  if (!file.exists(f0)) return(NULL)
  u <- sf::st_read(f0, quiet = TRUE)
  if (!"ug_id" %in% names(u)) return(NULL)
  u$ug_id <- as.character(u$ug_id)
  garde <- c("ug_id", intersect(.COLONNES_REGEN$sensibilite.gpkg, names(u)))
  u <- u[, garde]
  for (f in setdiff(names(.COLONNES_REGEN), "sensibilite.gpkg")) {
    chemin <- file.path(dossier, f)
    if (!file.exists(chemin)) next
    x <- tryCatch(sf::st_drop_geometry(sf::st_read(chemin, quiet = TRUE)), error = function(e) NULL)
    if (is.null(x) || !"ug_id" %in% names(x)) next
    j <- match(u$ug_id, as.character(x$ug_id))
    for (cc in intersect(.COLONNES_REGEN[[f]], names(x))) u[[cc]] <- x[[cc]][j]
    # Gel tardif non calcule (pas de Tmin) : NA, jamais 0.
    if ("r7_status" %in% names(x) && "r7_gel_days" %in% names(u)) {
      u$r7_gel_days[!as.character(x$r7_status[j]) %in% "calculated"] <- NA_real_
    }
  }
  u
}

# Thermophilie (echelle 1-9 du coeur) a partir de laquelle une essence est
# mediterraneenne. Le froid d'hiver n'a plus de seuil local : c'est le minimum
# decennal du coeur (`nemeton::regen_tmin_decennale()`, nemeton 2026.11.32).
.THERMOPHILIE_MEDITERRANEENNE <- 8

# Base climatique de la station, lue dans le cache de regeneration : annees
# d'ete rejouees par microclimf (cache_AAAA_*_tmax.tif) et celle de l'ete
# chaud (la plus proche de tmax_moyenne + d_tmax), annees du gel tardif
# (meteoland).
.climat_regen <- function(dossier, u) {
  micro <- file.path(dossier, "microclimf")
  f <- list.files(micro, pattern = "^cache_[0-9]{4}_.*_tmax\\.tif$", full.names = TRUE)
  ans <- as.integer(sub("^cache_([0-9]{4})_.*$", "\\1", basename(f)))
  annees <- sort(unique(ans))
  chaud <- NA_integer_
  cible <- if (all(c("tmax_moyenne", "d_tmax") %in% names(u))) {
    mean(as.numeric(u$tmax_moyenne) + as.numeric(u$d_tmax), na.rm = TRUE)
  } else NA_real_
  if (length(annees) >= 2L && is.finite(cible)) {
    moy <- vapply(annees, function(a) tryCatch({
      r <- terra::rast(f[ans == a][1])
      terra::global(r[[1]], "mean", na.rm = TRUE)[[1]]
    }, error = function(e) NA_real_), numeric(1))
    if (any(is.finite(moy))) chaud <- annees[which.min(abs(moy - cible))]
  } else if (length(annees) == 1L) chaud <- annees
  g <- list.files(file.path(dossier, "meteoland"), pattern = "^tmin_.*__y[0-9-]+__doy[0-9]+-[0-9]+\\.tif$")
  gel <- if (length(g)) as.integer(strsplit(sub("^.*__y([0-9-]+)__doy.*$", "\\1", g[1]), "-")[[1]]) else integer()
  list(nature = "observe", projection = FALSE,
       annees_ete = annees, annee_canicule = if (is.na(chaud)) NULL else chaud,
       annees_gel_tardif = gel)
}

#' Winter-cold filter on a long ranking (pure function, tested)
#'
#' @param rang Long ranking of all species (`nemeton::regen_rank_species()`).
#' @param traits Species traits (`code`, `frost_winter_min_c`, `thermophily`).
#' @param seuil Winter minimum (degrees C) a species must stand; `NULL` when
#'   the core already filtered (`exclus`).
#' @param exclus Pairs excluded by the core filter (`attr(, "exclus_froid")`
#'   of `regen_rank_species()`: `ug_id`, `species_code`), reported as `ecartees`.
#' @param top_n Species kept per unit.
#' @param aides Keep only species eligible as main ("objectif") species for
#'   public aid in Bourgogne-Franche-Comte (`.MFR_BFC`).
#' @return list: `rang` (non frost-sensitive species, top_n per unit,
#'   re-ranked), `hors_aide` (not eligible as main species, ranked above the
#'   last kept one, when `aides`), `ecartees` (frost-sensitive species that ranked above the
#'   last kept one), `locale` (best non-Mediterranean species per unit).
#' @noRd
.filtrer_froid <- function(rang, traits, seuil, top_n, aides = FALSE, exclus = NULL) {
  j <- match(rang$species_code, traits$code)
  rang$tenue_froid_c <- suppressWarnings(as.numeric(traits$frost_winter_min_c[j]))
  rang$thermophilie <- suppressWarnings(as.numeric(traits$thermophily[j]))
  rang$gelive <- if (is.null(seuil)) FALSE else !is.na(rang$tenue_froid_c) & rang$tenue_froid_c > seuil
  rang$aides_bfc <- .statut_mfr(rang$species_code)
  rang$hors_aide <- isTRUE(aides) & rang$aides_bfc != "objectif"
  vide <- rang[0, , drop = FALSE]
  parts <- lapply(unique(rang$ug_id), function(id) {
    df <- rang[rang$ug_id == id, , drop = FALSE]
    ok <- df[!is.na(df$rank), , drop = FALSE]
    ok <- ok[order(ok$rank), , drop = FALSE]
    garde <- utils::head(ok[!ok$gelive & !ok$hors_aide, , drop = FALSE], top_n)
    limite <- if (nrow(garde) == top_n) max(garde$rank) else Inf
    ecartees <- utils::head(ok[ok$gelive & ok$rank < limite, , drop = FALSE], top_n)
    hors <- utils::head(ok[!ok$gelive & ok$hors_aide & ok$rank < limite, , drop = FALSE], top_n)
    locale <- utils::head(ok[!ok$gelive & !ok$hors_aide & !is.na(ok$thermophilie) &
                               ok$thermophilie < .THERMOPHILIE_MEDITERRANEENNE, , drop = FALSE], 1)
    if (nrow(garde)) garde$rank <- seq_len(nrow(garde)) else garde <- df[1, , drop = FALSE][NA_integer_, , drop = FALSE]
    if (!nrow(ok)) garde <- df[1, , drop = FALSE]
    garde$ug_id <- id
    if (!is.null(exclus) && nrow(exclus)) {
      # Exclues par le filtre du coeur : absentes du classement, sans note.
      e <- exclus[exclus$ug_id == id, , drop = FALSE]
      k <- match(e$species_code, traits$code)
      ecartees <- data.frame(ug_id = as.character(e$ug_id), species_code = as.character(e$species_code),
                             label = if ("label" %in% names(traits)) as.character(traits$label[k]) else as.character(e$species_code),
                             suitability = rep(NA_real_, nrow(e)),
                             tenue_froid_c = suppressWarnings(as.numeric(traits$frost_winter_min_c[k])),
                             stringsAsFactors = FALSE)
      ecartees <- ecartees[order(ecartees$tenue_froid_c, decreasing = TRUE), , drop = FALSE]
    }
    list(garde = garde, ecartees = ecartees, hors_aide = hors, locale = locale)
  })
  pile <- function(k) { x <- lapply(parts, `[[`, k); if (length(x)) do.call(rbind, x) else vide }
  list(rang = pile("garde"), ecartees = pile("ecartees"), hors_aide = pile("hors_aide"), locale = pile("locale"))
}

# Choix d'une UGF par ug_id ou par nom (casse et accents ignores, nom
# complet puis contenu). Rend les indices retenus ; erreur avec candidats
# sinon. Pure, testee.
.choisir_ug <- function(ug, ids, labels) {
  if (is.null(ug) || !nzchar(trimws(as.character(ug)))) return(seq_along(ids))
  norm <- function(x) tolower(trimws(iconv(as.character(x), "UTF-8", "ASCII//TRANSLIT", sub = "")))
  q <- norm(ug)
  i <- which(ids == ug)
  if (!length(i)) i <- which(norm(labels) == q)
  if (!length(i)) i <- which(grepl(q, norm(labels), fixed = TRUE))
  if (length(i) == 1L) return(i)
  .abort(if (length(i)) "Unit\u00e9 de gestion {.val {ug}} ambigu\u00eb." else "Unit\u00e9 de gestion {.val {ug}} introuvable.",
         "nemetonmcp_ug_introuvable",
         candidats = lapply(utils::head(if (length(i)) i else seq_along(ids), 20),
                            function(k) list(ug_id = ids[k], label = labels[k])))
}

# Essences ecartees pour le froid d'une UGF (liste, NULL si aucune).
.essences_froid <- function(ecartees, id) {
  if (is.null(ecartees) || !nrow(ecartees)) return(NULL)
  e <- ecartees[ecartees$ug_id == id, , drop = FALSE]
  if (!nrow(e)) return(NULL)
  lapply(seq_len(nrow(e)), function(i) Filter(Negate(is.null), list(code = e$species_code[i], nom = e$label[i],
    aptitude = if (is.na(e$suitability[i])) NULL else round(as.numeric(e$suitability[i]), 1),
    tenue_froid_c = e$tenue_froid_c[i])))
}

# Essences d'une UGF classees plus haut mais non eligibles en objectif aux
# aides regionales (liste, NULL si aucune).
.essences_hors_aide <- function(hors, id) {
  if (is.null(hors) || !nrow(hors)) return(NULL)
  e <- hors[hors$ug_id == id, , drop = FALSE]
  if (!nrow(e)) return(NULL)
  lapply(seq_len(nrow(e)), function(i) list(code = e$species_code[i], nom = e$label[i],
    aptitude = round(as.numeric(e$suitability[i]), 1), aides_bfc = e$aides_bfc[i]))
}

# Meilleure essence non mediterraneenne d'une UGF, si elle n'est pas deja
# dans son classement (NULL sinon).
.essence_locale <- function(locale, id, codes) {
  if (is.null(locale) || !nrow(locale)) return(NULL)
  l <- locale[locale$ug_id == id, , drop = FALSE]
  if (!nrow(l) || l$species_code[1] %in% codes) return(NULL)
  list(code = l$species_code[1], nom = l$label[1], aptitude = round(as.numeric(l$suitability[1]), 1),
       facteur_limitant = l$limiting_factor[1], thermophilie = l$thermophilie[1])
}

# Minimum decennal d'hiver (degres C) par UGF, calcule par le coeur
# (`nemeton::regen_tmin_decennale()` : 10e percentile des minimums annuels
# E-OBS TN 1991-2020, ~15 min la premiere fois, fichier KNMI ~5 Go mis en
# cache machine) puis garde dans cache/regeneration/tmin_decennale.rds pour ne
# pas le refaire. NULL si le calcul echoue ou ne rend rien (E-OBS
# indisponible) : le classement est alors rendu sans filtre, et le dit.
.tmin_decennale_projet <- function(u, dossier, calcul = function(x) nemeton::regen_tmin_decennale(x)) {
  f <- file.path(dossier, "tmin_decennale.rds")
  ids <- as.character(u$ug_id)
  lu <- if (file.exists(f)) tryCatch(readRDS(f), error = function(e) NULL)
  if (is.data.frame(lu) && all(c("ug_id", "tmin_decennale") %in% names(lu)) && all(ids %in% lu$ug_id) &&
      !anyNA(lu$tmin_decennale[match(ids, lu$ug_id)])) return(lu)
  r <- tryCatch(suppressWarnings(calcul(u)), error = function(e) NULL)
  if (is.null(r) || !"tmin_decennale" %in% names(r) || all(is.na(r$tmin_decennale))) return(NULL)
  out <- data.frame(ug_id = ids, tmin_decennale = as.numeric(r$tmin_decennale),
                    n_annees = if ("tmin_decennale_n_annees" %in% names(r)) as.integer(r$tmin_decennale_n_annees) else NA_integer_,
                    stringsAsFactors = FALSE)
  # Cache complet seulement : un resultat partiel serait relu a tort.
  if (!anyNA(out$tmin_decennale)) tryCatch(saveRDS(out, f), error = function(e) NULL)
  out
}

# Avertissements a rendre a l'utilisateur (nemeton 2026.11.32-33) : d_tmax
# negatif (une seule annee simulee par categorie), gel tardif sur deux
# printemps, sens de `confiance`. Pure, testee.
.avertissements_regen <- function(u, climat, langue = "fr") {
  tx <- .TEXTES_REGEN[[if (identical(langue, "en")) "en" else "fr"]]
  d <- if ("d_tmax" %in% names(u)) suppressWarnings(as.numeric(u$d_tmax)) else numeric()
  neg <- sum(d < 0, na.rm = TRUE)
  ng <- length(climat$annees_gel_tardif)
  c(if (neg > 0L) sprintf(tx$avert_d_tmax, neg),
    if ("r7_gel_days" %in% names(u) && any(!is.na(u$r7_gel_days)) && ng > 0L && ng <= 2L) sprintf(tx$avert_gel, ng),
    tx$avert_confiance)
}

#' Species summary of a ranking (pure function, tested)
#'
#' @param rang Long ranking (`nemeton::regen_rank_species()`).
#' @param ugs data.frame of the units (`ug_id`, `label`, station columns).
#' @param nom Project name.
#' @return list: `ugf` (per unit: identity, station, species), `tete`
#'   (species first in the most units), `facteurs` (limiting factors of the
#'   first species), `texte`.
#' @noRd
.synthese_essences <- function(rang, ugs, nom, langue = "fr", ecartees = NULL, locale = NULL,
                               climat = NULL, seuil = NULL, hors_aide = NULL) {
  tx <- .TEXTES_REGEN[[if (identical(langue, "en")) "en" else "fr"]]
  rang <- rang[!is.na(rang$rank), , drop = FALSE]
  facteur <- function(x) ifelse(is.na(x), tx$sans_station,
                                ifelse(x %in% names(tx$facteurs), tx$facteurs[x], x))
  num <- function(df, cc) if (cc %in% names(df)) suppressWarnings(as.numeric(df[[cc]])) else rep(NA_real_, nrow(df))
  ugf <- lapply(seq_len(nrow(ugs)), function(k) {
    r <- rang[rang$ug_id == ugs$ug_id[k], , drop = FALSE]
    r <- r[order(r$rank), , drop = FALSE]
    Filter(Negate(is.null), list(
      ug_id = ugs$ug_id[k], label = ugs$label[k],
      priorite = if ("priorite" %in% names(ugs)) isTRUE(as.logical(ugs$priorite[k])),
      sensibilite = signif(num(ugs, "sensibilite")[k], 3),
      tmax_ete = signif(num(ugs, "tmax_moyenne")[k] + num(ugs, "d_tmax")[k], 3),
      rew_min = signif(num(ugs, "rew_min")[k], 3),
      jours_gel = signif(num(ugs, "r7_gel_days")[k], 3),
      essences = lapply(seq_len(nrow(r)), function(i) Filter(Negate(is.null), list(
        rang = as.integer(r$rank[i]), code = r$species_code[i], nom = r$label[i], type = r$type[i],
        aptitude = round(as.numeric(r$suitability[i]), 1),
        facteur_limitant = r$limiting_factor[i], confiance = r$confidence[i],
        aides_bfc = if ("aides_bfc" %in% names(r)) r$aides_bfc[i]))),
      hors_objectif_aides = .essences_hors_aide(hors_aide, ugs$ug_id[k]),
      ecartees_froid = .essences_froid(ecartees, ugs$ug_id[k]),
      non_mediterraneenne = .essence_locale(locale, ugs$ug_id[k], r$species_code)))
  })
  premiers <- rang[rang$rank == 1, , drop = FALSE]
  tete <- if (nrow(premiers)) {
    t <- sort(table(premiers$label), decreasing = TRUE)
    lapply(seq_len(min(3L, length(t))), function(i) list(nom = names(t)[i], ugf = as.integer(t[[i]])))
  } else list()
  fac <- if (nrow(premiers)) sort(table(premiers$limiting_factor), decreasing = TRUE) else integer()
  # Ecartees pour le froid : par UGF pour une seule UGF, sinon les noms
  # distincts du projet ; meilleure non mediterraneenne si hors du classement.
  froid <- if (nrow(ugs) == 1L) ugf[[1]]$ecartees_froid else {
    x <- unlist(lapply(ugf, function(u) u$ecartees_froid), recursive = FALSE)
    x[!duplicated(vapply(x, `[[`, "", "nom"))]
  }
  hors <- if (nrow(ugs) == 1L) ugf[[1]]$hors_objectif_aides else {
    x <- unlist(lapply(ugf, function(u) u$hors_objectif_aides), recursive = FALSE)
    x[!duplicated(vapply(x, `[[`, "", "nom"))]
  }
  sup <- paste0(
    if (length(froid)) sprintf(tx$froid, .nombre(seuil, 0, langue), paste(vapply(froid, function(x) {
      sprintf(tx$froid_une, x$nom, .nombre(x$tenue_froid_c, 0, langue))
    }, ""), collapse = ", ")) else "",
    if (length(hors)) sprintf(tx$aides, paste(vapply(hors, function(x) {
      sprintf("%s (%s)", x$nom, tx$statuts_aides[[x$aides_bfc]])
    }, ""), collapse = ", ")) else "",
    if (nrow(ugs) == 1L && !is.null(ugf[[1]]$non_mediterraneenne)) {
      l <- ugf[[1]]$non_mediterraneenne
      sprintf(tx$locale, sprintf(tx$locale_une, l$nom, .nombre(l$aptitude, 0, langue)))
    } else "",
    if (is.null(climat)) "" else if (length(climat$annees_ete) && !is.null(climat$annee_canicule)) {
      sprintf(tx$climat, paste(climat$annees_ete, collapse = "-"), climat$annee_canicule)
    } else tx$climat_inconnu)
  texte <- if (nrow(ugs) == 1L && length(ugf[[1]]$essences)) {
    e <- ugf[[1]]$essences
    paste0(sprintf(tx$une, nom, ugf[[1]]$label, paste(vapply(e, function(x) {
      sprintf(tx$essence, x$nom, .nombre(x$aptitude, 0, langue), facteur(x$facteur_limitant))
    }, ""), collapse = ", ")), sup)
  } else if (length(tete)) {
    paste0(sprintf(tx$resume, nom, nrow(ugs), paste(vapply(tete, function(x) sprintf(tx$tete, x$nom, x$ugf), ""), collapse = ", ")),
           if (length(fac)) sprintf(tx$facteur, facteur(names(fac)[1])) else "", sup)
  } else if (nrow(ugs) == 1L && length(froid)) {
    paste0(sprintf(tx$une, nom, ugs$label[1], tx$aucune_froid), sup)
  } else {
    sprintf(tx$une, nom, "", tx$sans_station)
  }
  list(ugf = ugf, tete = tete,
       facteurs = if (length(fac)) as.list(stats::setNames(as.integer(fac), names(fac))) else NULL,
       texte = texte, methode = tx$methode)
}

#' Best-suited regeneration species per management unit
#'
#' Deterministic ranking from the nemeton core
#' (`nemeton::regen_rank_species()`: species tolerances against site
#' conditions, the ranking of nemetonshiny's reGeneration tab), fed with the
#' outputs the regeneration analysis cached in the project
#' (`cache/regeneration/`: heat and VPD, soil water balance, late frost).
#' Nothing is recomputed: without an analysis, the error
#' `nemetonmcp_regeneration_absente` says how to run it. The shade axis is
#' left out (the engine's LAI is not kept on disk); invasive species are
#' excluded. Site conditions come from the observed climate (not a
#' projection). Winter cold is an elimination filter of the core: the ten-year
#' minimum per unit (`nemeton::regen_tmin_decennale()`, E-OBS 1991-2020, about
#' 15 minutes the first time, then kept in the project) is passed to
#' `regen_rank_species()`, which does not rank a species less hardy than it;
#' those are in `ecartees_froid` (`attr(, "exclus_froid")`). If the minimum
#' cannot be computed, the ranking is given without the filter and says so.
#' The heat axis is the one corrected in nemeton 2026.11.33; `avertissements`
#' flags a negative `d_tmax`, late frost on two springs, and that
#' `confiance` is about the species traits, not the site. The best
#' non-Mediterranean species is given when it is not in the ranking.
#'
#' @param projet Project id or name.
#' @param ug Optional management unit (id or name) to rank alone.
#' @param top_n Species per unit (1 to 10, default 3).
#' @param aides `TRUE` (default): rank only the species eligible as main
#'   species for state aid in Bourgogne-Franche-Comte (regional MFR decree of
#'   6 July 2026, annex 1.1); the higher-ranked others are listed in
#'   `hors_objectif_aides`. `FALSE`: ecological ranking alone (each species
#'   still carries `aides_bfc`).
#' @param langue `"fr"` or `"en"`.
#' @return JSON: `ugf` (per unit: identity, priority, sensitivity, summer
#'   T max, soil water minimum, frost days, and its ranked species with
#'   suitability 0-100, limiting factor and confidence), `tete` (species
#'   first in the most units), `facteurs`, `texte`, `methode`, `climat`
#'   (observed summers, hot summer, ERA5 winter minimum, late-frost years,
#'   ten-year winter minimum), `avertissements`, `source`. Per unit, `ecartees_froid` and
#'   `non_mediterraneenne` when relevant.
#' @export
regen_essences <- function(projet, ug = NULL, top_n = 3, aides = TRUE, langue = "fr") {
  .mcp_call(langue = langue, {
    id <- .resoudre_projet(projet)
    langue <- if (identical(langue, "en")) "en" else "fr"
    top_n <- max(1L, min(10L, as.integer(top_n %||% 3L)))
    chemin <- .ns("get_project_path")(id)
    dossier <- file.path(chemin, "cache", "regeneration")
    u <- .unites_regen(dossier)
    if (is.null(u) || !nrow(u)) {
      .abort(c("Projet {.val {id}} : aucune analyse de r\u00e9g\u00e9n\u00e9ration en cache.",
               i = "La lancer dans nemetonshiny, onglet R\u00e9g\u00e9n\u00e9ration (url_app)."),
             "nemetonmcp_regeneration_absente")
    }
    geo <- .ug_sf_leger(id, chemin)
    refs <- if (!is.null(geo)) as.character(geo$cadastral_refs)[match(u$ug_id, as.character(geo$ug_id))]
    labels <- if (!is.null(geo)) as.character(geo$label)[match(u$ug_id, as.character(geo$ug_id))] else u$ug_id
    u$label <- .identites_ug(u$ug_id, labels, refs)$label
    climat <- .climat_regen(dossier, u)
    u <- u[.choisir_ug(ug, u$ug_id, u$label), ]
    # Froid d'hiver : minimum decennal par UGF (coeur, garde dans le projet),
    # puis le coeur ecarte lui-meme les essences qui ne le tiennent pas. Tout
    # le pool est classe avant de garder top_n.
    froid <- .tmin_decennale_projet(u, dossier)
    if (!is.null(froid)) u$tmin_decennale <- froid$tmin_decennale[match(u$ug_id, froid$ug_id)]
    rang <- nemeton::regen_rank_species(u, top_n = 1000L, region = "BFC")
    seuil <- if (is.null(froid)) NULL else stats::median(u$tmin_decennale, na.rm = TRUE)
    aides <- !isFALSE(as.logical(aides))
    filtre <- .filtrer_froid(rang, nemeton::european_species_tolerances(), NULL, top_n, aides,
                             exclus = attr(rang, "exclus_froid"))
    nom <- tryCatch(nemetonshiny::projet_etat(id)$nom, error = function(e) NULL) %||% id
    s <- .synthese_essences(filtre$rang, sf::st_drop_geometry(u), nom, langue, filtre$ecartees,
                            filtre$locale, climat, seuil, filtre$hors_aide)
    tx <- .TEXTES_REGEN[[langue]]
    if (aides) s$methode <- paste0(s$methode, tx$methode_aides)
    avert <- .avertissements_regen(sf::st_drop_geometry(u), climat, langue)
    if (is.null(froid)) s$texte <- paste0(s$texte, tx$froid_absent)
    if (length(avert)) s$texte <- paste0(s$texte, " ", paste(avert, collapse = " "))
    c(list(projet = id, nom = nom, n_ugf = nrow(u), top_n = top_n), s,
      list(avertissements = if (length(avert)) as.list(avert),
           climat = c(climat, list(seuil_froid_c = seuil,
                                   tmin_decennale_c = if (!is.null(froid)) round(range(u$tmin_decennale, na.rm = TRUE), 1))),
           aides = list(filtre = aides, region = "Bourgogne-Franche-Comt\u00e9", arrete = .MFR_BFC$arrete, url = .MFR_BFC$url),
           source = list(dossier = "cache/regeneration",
                         date = format(file.mtime(file.path(dossier, "sensibilite.gpkg")), "%Y-%m-%d"))))
  })
}
