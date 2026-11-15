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
    methode = "Classement d\u00e9terministe du c\u0153ur nemeton (tol\u00e9rances des essences contre conditions de station : chaleur et VPD, s\u00e9cheresse du sol, gel tardif), sans l'axe d'ombrage ; essences envahissantes exclues.",
    facteurs = c(chaleur = "la chaleur", vpd = "la s\u00e9cheresse de l'air", secheresse = "la s\u00e9cheresse",
                 gel = "le gel tardif", ombre = "l'ombre"),
    sans_station = "conditions de station inconnues"),
  en = list(
    resume = "%s: best-suited species over %d units: %s.",
    tete = "%s (first in %d units)",
    facteur = " Most frequent limiting factor: %s.",
    une = "%s, %s: %s.",
    essence = "%s (%s/100, limited by %s)",
    methode = "Deterministic ranking from the nemeton core (species tolerances against site conditions: heat and VPD, soil drought, late frost), without the shade axis; invasive species excluded.",
    facteurs = c(chaleur = "heat", vpd = "air dryness", secheresse = "drought", gel = "late frost", ombre = "shade"),
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
  }
  u
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

#' Species summary of a ranking (pure function, tested)
#'
#' @param rang Long ranking (`nemeton::regen_rank_species()`).
#' @param ugs data.frame of the units (`ug_id`, `label`, station columns).
#' @param nom Project name.
#' @return list: `ugf` (per unit: identity, station, species), `tete`
#'   (species first in the most units), `facteurs` (limiting factors of the
#'   first species), `texte`.
#' @noRd
.synthese_essences <- function(rang, ugs, nom, langue = "fr") {
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
      essences = lapply(seq_len(nrow(r)), function(i) list(
        rang = as.integer(r$rank[i]), code = r$species_code[i], nom = r$label[i], type = r$type[i],
        aptitude = round(as.numeric(r$suitability[i]), 1),
        facteur_limitant = r$limiting_factor[i], confiance = r$confidence[i]))))
  })
  premiers <- rang[rang$rank == 1, , drop = FALSE]
  tete <- if (nrow(premiers)) {
    t <- sort(table(premiers$label), decreasing = TRUE)
    lapply(seq_len(min(3L, length(t))), function(i) list(nom = names(t)[i], ugf = as.integer(t[[i]])))
  } else list()
  fac <- if (nrow(premiers)) sort(table(premiers$limiting_factor), decreasing = TRUE) else integer()
  texte <- if (nrow(ugs) == 1L && length(ugf[[1]]$essences)) {
    e <- ugf[[1]]$essences
    sprintf(tx$une, nom, ugf[[1]]$label, paste(vapply(e, function(x) {
      sprintf(tx$essence, x$nom, .nombre(x$aptitude, 0, langue), facteur(x$facteur_limitant))
    }, ""), collapse = ", "))
  } else if (length(tete)) {
    paste0(sprintf(tx$resume, nom, nrow(ugs), paste(vapply(tete, function(x) sprintf(tx$tete, x$nom, x$ugf), ""), collapse = ", ")),
           if (length(fac)) sprintf(tx$facteur, facteur(names(fac)[1])) else "")
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
#' excluded.
#'
#' @param projet Project id or name.
#' @param ug Optional management unit (id or name) to rank alone.
#' @param top_n Species per unit (1 to 10, default 3).
#' @param langue `"fr"` or `"en"`.
#' @return JSON: `ugf` (per unit: identity, priority, sensitivity, summer
#'   T max, soil water minimum, frost days, and its ranked species with
#'   suitability 0-100, limiting factor and confidence), `tete` (species
#'   first in the most units), `facteurs`, `texte`, `methode`, `source`.
#' @export
regen_essences <- function(projet, ug = NULL, top_n = 3, langue = "fr") {
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
    u <- u[.choisir_ug(ug, u$ug_id, u$label), ]
    rang <- nemeton::regen_rank_species(u, top_n = top_n, region = "BFC")
    nom <- tryCatch(nemetonshiny::projet_etat(id)$nom, error = function(e) NULL) %||% id
    s <- .synthese_essences(rang, sf::st_drop_geometry(u), nom, langue)
    c(list(projet = id, nom = nom, n_ugf = nrow(u), top_n = top_n), s,
      list(source = list(dossier = "cache/regeneration",
                         date = format(file.mtime(file.path(dossier, "sensibilite.gpkg")), "%Y-%m-%d"))))
  })
}
