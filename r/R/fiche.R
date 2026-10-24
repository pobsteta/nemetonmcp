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
            prochaines = "Prochaines actions", aucune = "Aucune action à venir.",
            calcul = "Calcul", maj = "Indicateurs du", actions = "actions",
            sur = "sur 100"),
  en = list(fiche = "Nemeton sheet", score = "Overall score", ndp = "NDP level",
            confiance = "Confidence", ugf = "Units", famille = "Family", code = "Code",
            note = "Score", forces = "Strengths", faiblesses = "Weaknesses",
            prochaines = "Next actions", aucune = "No upcoming action.",
            calcul = "Computation", maj = "Indicators of", actions = "actions",
            sur = "out of 100")
)

# Nombre a la francaise (virgule decimale, espace fine des milliers).
.nombre <- function(x, n = 1, langue = "fr") {
  if (is.null(x) || !length(x) || is.na(x)) return("—")
  formatC(round(as.numeric(x), n), format = "f", digits = n,
          big.mark = if (langue == "fr") " " else ",",
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
#' @return list: header, families sorted by score, strengths and weaknesses,
#'   plan summary, `texte` (one sentence) and `display_report`.
#' @noRd
.fiche <- function(entete, plan = NULL, surface_ha = NA_real_) {
  langue <- if (identical(entete$langue, "en")) "en" else "fr"
  tx <- .TEXTES_FICHE[[langue]]
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
    list(label = tx$ndp, value = paste0(entete$ndp_level %||% "—",
                                        if (!is.null(entete$ndp_name)) paste0(" · ", entete$ndp_name))),
    list(label = tx$confiance, value = if (is.na(confiance)) "—" else
      paste0(.nombre(100 * confiance, 0, langue), " %")),
    list(label = tx$ugf, value = paste0(entete$n_ugf %||% "—",
                                        if (!is.na(surface_ha)) paste0(" · ", .nombre(surface_ha, 1, langue), " ha")))
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
    title = sprintf("%s — %s", entete$name %||% entete$project_id, tx$fiche),
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

# Cle de la fiche en cache : elle change quand les indicateurs, le plan ou
# la version du connecteur changent.
.cle_fiche <- function(id, langue) {
  chemin <- .ns("get_project_path")(id) %||% ""
  mtime <- function(f) if (file.exists(f)) format(as.numeric(file.mtime(f)), digits = 15) else "0"
  paste(id, langue, mtime(file.path(chemin, "data", "indicators.parquet")),
        mtime(file.path(chemin, "data", "action_plan.json")),
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
#' project's view folder, until the indicators or the plan change.
#'
#' @param projet Project id or name.
#' @param langue `"fr"` or `"en"`.
#' @param actualiser Rebuild the sheet even when the cached one is current.
#' @return JSON: the sheet, `fichier` (path of `fiche.json`), `depuis_cache`,
#'   and `calcul` (state of the computation, read on each call).
#' @export
fiche_projet <- function(projet, langue = "fr", actualiser = FALSE) {
  .mcp_call({
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
      if (is.null(lu$familles) || !nrow(lu$familles)) {
        .abort(c("Projet {.val {id}} : aucun indicateur calculé.",
                 i = "Lancer {.code lancer_calcul} puis suivre {.code etat_calcul}."),
               "nemetonshiny_sans_indicateurs")
      }
      surface <- tryCatch({
        s <- sf::st_drop_geometry(lu$familles)
        col <- intersect(c("surface_m2", "surface_sig_m2"), names(s))[1]
        if (is.na(col)) NA_real_ else sum(as.numeric(s[[col]]), na.rm = TRUE) / 1e4
      }, error = function(e) NA_real_)
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
      fiche <- c(.fiche(.entete_atlas(id, lu$synthese, langue), plan, surface), list(cle = cle))
      writeLines(.json(fiche), fichier, useBytes = TRUE)
    }
    fiche$cle <- NULL
    c(fiche, list(calcul = calcul, fichier = normalizePath(fichier), depuis_cache = depuis_cache))
  })
}
