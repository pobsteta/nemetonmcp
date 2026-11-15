# Outil `fiche_projet(projet)` : la fiche d'un projet, une seule source pour
# display_report (VICTOR), les vues (Claude) et le texte (Le Chat).
#
# La fiche reprend l'en-tete de l'Atlas (score global, NDP, confiance,
# familles), ajoute le plan d'actions resume et l'etat du calcul, et porte un
# bloc `display_report` pret a passer tel quel a l'outil de VICTOR. Elle est
# ecrite dans `fiche.json` (dossier des vues du projet) et relue tant que ni
# les indicateurs ni le plan n'ont change : une question de lecture ne relance
# pas projet_lire(). L'etat du calcul, lui, est relu a chaque appel.

.TEXTES_FICHE <- list(
  fr = list(fiche = "fiche Nemeton", score = "Score global", ndp = "Niveau NDP",
            confiance = "Confiance", ugf = "UGF", famille = "Famille", code = "Code",
            note = "Score", forces = "Points forts", faiblesses = "Points faibles",
            prochaines = "Prochaines actions", aucune = "Aucune action \u00e0 venir.",
            calcul = "Calcul", maj = "Indicateurs du", actions = "actions",
            sur = "sur 100", a_recalculer = "\u00e0 recalculer", a_calculer = "\u00e0 calculer",
            perimes = "Scores \u00e0 recalculer : les indicateurs ont \u00e9t\u00e9 mis de c\u00f4t\u00e9%s. Relancer le calcul (lancer_calcul).",
            jamais = "Scores \u00e0 calculer : aucun indicateur n'a encore \u00e9t\u00e9 calcul\u00e9. Lancer le calcul (lancer_calcul).",
            motifs = c(ugf = "apr\u00e8s la modification des unit\u00e9s de gestion",
                       parcelles = "apr\u00e8s la modification des parcelles")),
  en = list(fiche = "Nemeton sheet", score = "Overall score", ndp = "NDP level",
            confiance = "Confidence", ugf = "Units", famille = "Family", code = "Code",
            note = "Score", forces = "Strengths", faiblesses = "Weaknesses",
            prochaines = "Next actions", aucune = "No upcoming action.",
            calcul = "Computation", maj = "Indicators of", actions = "actions",
            sur = "out of 100", a_recalculer = "to recompute", a_calculer = "to compute",
            perimes = "Scores to recompute: the indicators were set aside%s. Run the computation again (lancer_calcul).",
            jamais = "Scores to compute: no indicator has been computed yet. Run the computation (lancer_calcul).",
            motifs = c(ugf = " after the management units changed",
                       parcelles = " after the parcels changed"))
)

# Nombre a la francaise (virgule decimale, espace fine des milliers).
.nombre <- function(x, n = 1, langue = "fr") {
  if (is.null(x) || !length(x) || is.na(x)) return("\u2014")
  formatC(round(as.numeric(x), n), format = "f", digits = n,
          big.mark = if (langue == "fr") "\u202f" else ",",
          decimal.mark = if (langue == "fr") "," else ".")
}

# Resume du plan d'actions : effectifs par statut, et les prochaines actions
# (ni realisees ni abandonnees), par annee puis priorite. Pure, testee.
.resume_plan <- function(actions, n = 5) {
  actions <- actions %||% list()
  statuts <- vapply(actions, function(a) as.character(a$statut %||% "proposee"), "")
  ordre_prio <- c(haute = 1L, moyenne = 2L, basse = 3L)
  a_venir <- Filter(function(a) !(a$statut %||% "") %in% c("realisee", "abandonnee"), actions)
  if (length(a_venir)) {
    annee <- vapply(a_venir, function(a) as.integer(a$annee %||% NA), 1L)
    prio <- vapply(a_venir, function(a) unname(ordre_prio[a$priorite %||% "basse"]) %||% 3L, 1L)
    a_venir <- a_venir[order(annee, prio, na.last = TRUE)]
  }
  prochaines <- lapply(utils::head(a_venir, n), function(a) {
    list(annee = a$annee, type = a$type, ug = a$ug_label %||% a$ug_id,
         statut = a$statut, priorite = a$priorite)
  })
  list(n_actions = length(actions),
       par_statut = if (length(statuts)) as.list(table(statuts)) else stats::setNames(list(), character()),
       prochaines = prochaines)
}

#' Project sheet (pure function, tested)
#'
#' @param entete Atlas header (`.entete_atlas()`).
#' @param plan Plan summary (`.resume_plan()`), or `NULL`.
#' @param surface_ha Total area of the management units.
#' @param scores `NULL` when the indicators are there; otherwise
#'   `list(etat = "perimes" | "absents", motif = "ugf" | ...)`: the sheet
#'   still gives the units and the plan, and says the scores are to compute.
#' @return list: header, families sorted by score, strengths and weaknesses,
#'   plan summary, `texte` (one sentence) and `display_report`; without
#'   scores, `scores_a_calculer = TRUE` and `indicateurs_perimes`.
#' @noRd
.fiche <- function(entete, plan = NULL, surface_ha = NA_real_, scores = NULL) {
  langue <- if (identical(entete$langue, "en")) "en" else "fr"
  tx <- .TEXTES_FICHE[[langue]]
  if (!is.null(scores)) return(.fiche_sans_scores(entete, plan, surface_ha, scores, tx, langue))
  fam <- entete$familles %||% list()
  scores <- vapply(fam, function(f) as.numeric(f$score %||% NA), 0)
  ordre <- order(scores, decreasing = TRUE, na.last = NA)
  classees <- fam[ordre]
  forces <- utils::head(classees, 3)
  faiblesses <- rev(utils::tail(classees, 3))
  nom_fam <- function(f) sprintf("%s (%s)", f$nom, .nombre(f$score, 0, langue))

  confiance <- as.numeric(entete$confidence %||% NA)
  kpis <- list(
    list(label = tx$score, value = paste(.nombre(entete$global_score, 1, langue), "/ 100")),
    list(label = tx$ndp, value = paste0(entete$ndp_level %||% "\u2014",
                                        if (!is.null(entete$ndp_name)) paste0(" \u00b7 ", entete$ndp_name))),
    list(label = tx$confiance, value = if (is.na(confiance)) "\u2014" else
      paste0(.nombre(100 * confiance, 0, langue), "\u00a0%")),
    list(label = tx$ugf, value = paste0(entete$n_ugf %||% "\u2014",
                                        if (!is.na(surface_ha)) paste0(" \u00b7 ", .nombre(surface_ha, 1, langue), "\u00a0ha")))
  )

  md <- c(
    sprintf("**%s** : %s", tx$forces, paste(vapply(forces, nom_fam, ""), collapse = ", ")),
    sprintf("**%s** : %s", tx$faiblesses, paste(vapply(faiblesses, nom_fam, ""), collapse = ", "))
  )
  if (!is.null(plan)) {
    lignes <- vapply(plan$prochaines, function(a) {
      sprintf("- %s : %s, %s (%s)", a$annee %||% "?", a$type %||% "?", a$ug %||% "?", a$statut %||% "?")
    }, "")
    md <- c(md, "", sprintf("**%s** (%d %s)", tx$prochaines, plan$n_actions, tx$actions),
            if (length(lignes)) lignes else tx$aucune)
  }
  if (!is.null(entete$updated_at)) md <- c(md, "", sprintf("_%s %s._", tx$maj, entete$updated_at))

  rapport <- list(
    title = sprintf("%s \u2014 %s", entete$name %||% entete$project_id, tx$fiche),
    kpis = kpis,
    chart = list(type = "bar",
                 categories = vapply(fam, function(f) as.character(f$nom %||% f$code), ""),
                 series = list(list(name = tx$note, data = round(scores, 1)))),
    table = list(columns = c(tx$famille, tx$code, tx$note),
                 rows = lapply(classees, function(f) list(f$nom, f$code, round(as.numeric(f$score), 1)))),
    markdown = paste(md, collapse = "\n")
  )

  texte <- if (langue == "fr") {
    sprintf("%s : score global %s sur 100, niveau NDP %s, %d UGF. Points forts : %s. Points faibles : %s.",
            entete$name %||% entete$project_id, .nombre(entete$global_score, 0, langue),
            entete$ndp_level %||% "?", as.integer(entete$n_ugf %||% 0L),
            paste(vapply(forces, `[[`, "", "nom"), collapse = ", "),
            paste(vapply(faiblesses, `[[`, "", "nom"), collapse = ", "))
  } else {
    sprintf("%s: overall score %s out of 100, NDP level %s, %d units. Strengths: %s. Weaknesses: %s.",
            entete$name %||% entete$project_id, .nombre(entete$global_score, 0, langue),
            entete$ndp_level %||% "?", as.integer(entete$n_ugf %||% 0L),
            paste(vapply(forces, `[[`, "", "nom"), collapse = ", "),
            paste(vapply(faiblesses, `[[`, "", "nom"), collapse = ", "))
  }

  c(entete[setdiff(names(entete), "familles")],
    list(surface_ha = if (is.na(surface_ha)) NULL else round(surface_ha, 2),
         familles = classees,
         forces = vapply(forces, `[[`, "", "code"),
         faiblesses = vapply(faiblesses, `[[`, "", "code"),
         plan = plan,
         texte = texte,
         display_report = rapport))
}

# Lignes du plan d'actions pour le markdown de la fiche.
.md_plan <- function(plan, tx) {
  if (is.null(plan)) return(character())
  lignes <- vapply(plan$prochaines, function(a) {
    sprintf("- %s : %s, %s (%s)", a$annee %||% "?", a$type %||% "?", a$ug %||% "?", a$statut %||% "?")
  }, "")
  c("", sprintf("**%s** (%d %s)", tx$prochaines, plan$n_actions, tx$actions),
    if (length(lignes)) lignes else tx$aucune)
}

# Fiche d'un projet sans indicateurs (perimes apres une modification des UGF,
# ou jamais calcules) : les UGF, la surface et le plan restent vrais, les
# scores sont annonces « a recalculer », jamais a 0. Pure, testee.
.fiche_sans_scores <- function(entete, plan, surface_ha, scores, tx, langue) {
  perimes <- identical(scores$etat, "perimes")
  motif <- if (perimes && !is.null(scores$motif) && scores$motif %in% names(tx$motifs)) {
    paste0(if (langue == "fr") " " else "", tx$motifs[[scores$motif]])
  } else ""
  phrase <- if (perimes) sprintf(tx$perimes, motif) else tx$jamais
  etat <- if (perimes) tx$a_recalculer else tx$a_calculer
  n_ugf <- entete$n_ugf
  surface_txt <- if (!is.na(surface_ha)) paste0(" \u00b7 ", .nombre(surface_ha, 1, langue), "\u00a0ha")
  kpis <- list(
    list(label = tx$score, value = etat),
    list(label = tx$ndp, value = paste0(entete$ndp_level %||% "\u2014",
                                        if (!is.null(entete$ndp_name) && !is.na(entete$ndp_name))
                                          paste0(" \u00b7 ", entete$ndp_name))),
    list(label = tx$ugf, value = paste0(n_ugf %||% "\u2014", surface_txt %||% ""))
  )
  md <- c(sprintf("**%s**", phrase), .md_plan(plan, tx))
  nom <- entete$name %||% entete$project_id
  texte <- if (langue == "fr") {
    paste0(nom, " : ", if (!is.null(n_ugf)) paste0(n_ugf, " UGF") else "UGF inconnues",
           if (!is.na(surface_ha)) paste0(", ", .nombre(surface_ha, 1, langue), " ha") else "",
           ". ", phrase)
  } else {
    paste0(nom, ": ", if (!is.null(n_ugf)) paste0(n_ugf, " units") else "unknown units",
           if (!is.na(surface_ha)) paste0(", ", .nombre(surface_ha, 1, langue), " ha") else "",
           ". ", phrase)
  }
  rapport <- list(
    title = sprintf("%s \u2014 %s", nom, tx$fiche),
    kpis = kpis,
    markdown = paste(md, collapse = "\n")
  )
  c(entete[setdiff(names(entete), c("familles", "global_score", "confidence"))],
    list(global_score = NULL,
         surface_ha = if (is.na(surface_ha)) NULL else round(surface_ha, 2),
         familles = list(), forces = character(), faiblesses = character(),
         scores_a_calculer = TRUE, indicateurs_perimes = perimes,
         motif_perime = if (perimes) scores$motif,
         plan = plan, texte = texte, display_report = rapport))
}

# Etat des scores d'un projet sans indicateurs : « perimes » quand nemetonshiny
# a mis des indicateurs de cote (`metadata$indicateurs_perimes`, le plus
# recent en tete ou en queue selon la version), « absents » sinon. Pure, testee.
.etat_scores <- function(metadata) {
  arch <- metadata$indicateurs_perimes
  if (is.null(arch) || !length(arch)) return(list(etat = "absents"))
  if (is.data.frame(arch)) arch <- lapply(seq_len(nrow(arch)), function(i) as.list(arch[i, , drop = FALSE]))
  if (!is.null(names(arch)) && !is.null(arch$motif)) arch <- list(arch)
  dates <- vapply(arch, function(a) as.character(unlist(a$date %||% a$le %||% NA)[1]), "")
  i <- if (all(is.na(dates))) length(arch) else which.max(rank(dates, na.last = FALSE, ties.method = "last"))
  list(etat = "perimes", motif = as.character(unlist(arch[[i]]$motif %||% NA)[1]))
}

# Surface des UGF ou, a defaut, des parcelles, en hectares.
.surface_projet <- function(familles = NULL, projet = NULL) {
  depuis <- function(x) {
    if (is.null(x) || !nrow(x)) return(NA_real_)
    s <- if (inherits(x, "sf")) sf::st_drop_geometry(x) else x
    col <- intersect(c("surface_m2", "surface_sig_m2", "contenance"), names(s))[1]
    if (!is.na(col)) return(sum(as.numeric(s[[col]]), na.rm = TRUE) / 1e4)
    if (inherits(x, "sf")) return(as.numeric(sum(sf::st_area(x))) / 1e4)
    NA_real_
  }
  v <- tryCatch(depuis(familles), error = function(e) NA_real_)
  if (is.na(v)) v <- tryCatch(depuis(projet$parcels), error = function(e) NA_real_)
  v
}

# Cle de la fiche en cache : elle change quand les indicateurs, le plan ou
# la version du connecteur changent.
.cle_fiche <- function(id, langue) {
  chemin <- .ns("get_project_path")(id) %||% ""
  mtime <- function(f) if (file.exists(f)) format(as.numeric(file.mtime(f)), digits = 15) else "0"
  paste(id, langue, mtime(file.path(chemin, "data", "indicators.parquet")),
        mtime(file.path(chemin, "data", "action_plan.json")),
        mtime(file.path(chemin, "data", "ugs.json")),
        as.character(utils::packageVersion("nemetonmcp")), sep = "|")
}

#' Project sheet
#'
#' One JSON sheet per project, read by VICTOR (`display_report`), the views
#' and plain-text clients: the Atlas header (global score, NDP level,
#' confidence, family scores), strengths and weaknesses, a summary of the
#' action plan, the state of the computation, a one-sentence `texte`, and a
#' `display_report` block (title, KPIs, bar chart, table, markdown) ready to
#' pass to VICTOR's tool. The sheet is cached in `fiche.json`, in the
#' project's view folder, until the indicators, the units or the plan change.
#' Without indicators (set aside after the units changed, or never computed)
#' the sheet still gives the units, the area and the plan, with
#' `scores_a_calculer = TRUE`, `indicateurs_perimes` and a text saying the
#' scores are to (re)compute: never a score of 0 nor « 0 UGF ».
#'
#' @param projet Project id or name.
#' @param langue `"fr"` or `"en"`.
#' @param actualiser Rebuild the sheet even when the cached one is current.
#' @return JSON: the sheet, `fichier` (path of `fiche.json`), `depuis_cache`,
#'   and `calcul` (state of the computation, read on each call).
#' @export
fiche_projet <- function(projet, langue = "fr", actualiser = FALSE) {
  .mcp_call(langue = langue, {
    id <- .resoudre_projet(projet)
    langue <- if (identical(langue, "en")) "en" else "fr"
    calcul <- tryCatch({
      x <- jsonlite::fromJSON(etat_calcul_vue(id), simplifyVector = FALSE)
      if (isTRUE(x$ok)) x[intersect(names(x), c("statut", "progression", "progression_max",
                                                "indicateurs_faits", "indicateurs_total",
                                                "tache", "ecoule_s", "lance_a", "fin_a"))]
    }, error = function(e) NULL)

    fichier <- file.path(.dossier_vues("projets", id), if (langue == "fr") "fiche.json" else "fiche-en.json")
    cle <- .cle_fiche(id, langue)
    fiche <- if (!isTRUE(actualiser) && file.exists(fichier)) {
      tryCatch({
        f <- jsonlite::read_json(fichier, simplifyVector = FALSE)
        if (identical(f$cle, cle)) f
      }, error = function(e) NULL)
    }
    depuis_cache <- !is.null(fiche)
    if (!depuis_cache) {
      lu <- .lire_projet(id, langue)
      sans_scores <- is.null(lu$familles) || !nrow(lu$familles)
      surface <- .surface_projet(if (!sans_scores) lu$familles, lu$projet)
      plan <- tryCatch({
        p <- .ns("load_action_plan")(id)
        if (!is.null(p)) {
          ug <- tryCatch(.ns("ug_build_sf")(lu$projet), error = function(e) NULL)
          labels <- if (!is.null(ug)) stats::setNames(
            .identites_ug(as.character(ug$ug_id), ug$label, ug$cadastral_refs)$label,
            as.character(ug$ug_id))
          base <- .ns("action_plan_annee_base")(p)
          .resume_plan(lapply(p$actions %||% list(), .action_pour_vue, annee_base = base, labels = labels))
        }
      }, error = function(e) NULL)
      scores <- if (sans_scores) .etat_scores(lu$projet$metadata)
      fiche <- c(.fiche(.entete_atlas(id, lu$synthese, langue, lu$projet), plan, surface, scores),
                 list(cle = cle))
      writeLines(.json(fiche), fichier, useBytes = TRUE)
    }
    fiche$cle <- NULL
    c(fiche, list(calcul = calcul, fichier = normalizePath(fichier), depuis_cache = depuis_cache))
  })
}
