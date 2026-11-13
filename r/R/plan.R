# Outils du plan d'actions partage (lot 3) : lire le plan d'un projet, y
# ajouter, modifier ou retirer une action, servir les profils experts et
# exporter le paquet terrain Marculus.
#
# Le plan reste celui du projet nemeton (`data/action_plan.json`, avec son
# historique), lu et ecrit par l'API de nemetonshiny (validation, transitions,
# audit, annee civile) : une seule version, partagee avec l'application. Le
# serveur traite une requete a la fois ; chaque ecriture relit le plan sur
# disque juste avant de l'ecrire.

# Identite inscrite dans l'historique : le compte Keycloak (connecteur
# distant, pose par .entrer_contexte) ou l'utilisateur du systeme (local).
.utilisateur_courant <- function() {
  getOption("nemetonmcp.utilisateur", Sys.info()[["user"]] %||% "inconnu")
}

# Droit d'ecrire : pose par le connecteur distant d'apres les roles ; en
# local (stdio), le proprietaire du serveur ecrit.
.peut_ecrire <- function() isTRUE(getOption("nemetonmcp.peut_ecrire", TRUE))

# Ecriture d'un projet existant en deux temps (jeton de confirmation, ADR
# 0002 de nemetonshiny ; brief du 2026-11-10, section 4). Sans jeton : la
# garde de nemetonshiny (calcul en cours, verrou d'edition, projet ancien),
# puis emission d'un jeton qui leve `confirmation_requise` avec l'apercu ;
# rien n'est ecrit. Avec jeton : `.mcp_jeton_consommer()` rejoue lui-meme la
# garde (avant de consommer : un refus laisse le jeton valable), puis verifie
# que `lien` n'a pas change depuis l'apercu. Ne pas appeler la garde avant
# lui. `lien` : ce que la confirmation doit retrouver a l'identique.
.confirmer_ecriture <- function(outil, id, jeton, lien, apercu) {
  if (is.null(jeton) || !nzchar(jeton)) {
    .ns(".mcp_garde_ecriture")(id)
    .ns(".mcp_jeton_emettre")(outil, id, lien, apercu)
  }
  .ns(".mcp_jeton_consommer")(jeton, outil, id, lien)
}

# Apercu d'une action pour la confirmation : ce que l'utilisateur doit voir.
.apercu_action <- function(operation, a, annee_base, ug_labels = NULL) {
  ug <- as.character(a$ug_id %||% "")
  Filter(Negate(is.null), list(
    operation = operation, action_id = a$id,
    ug = if (!is.null(ug_labels) && ug %in% names(ug_labels)) ug_labels[[ug]] else a$ug_id,
    type = a$type, annee = if (!is.null(a$annee_cible)) as.integer(annee_base) + as.integer(a$annee_cible),
    statut = a$statut, priorite = a$priorite))
}

.ug_ids_projet <- function(id) {
  projet <- .ns("load_project")(id)
  ug <- tryCatch(.ns("ug_build_sf")(projet), error = function(e) NULL)
  if (is.null(ug) || !nrow(ug)) return(NULL)
  list(projet = projet, ug = ug, ids = as.character(ug$ug_id))
}

# ---------------------------------------------------------------- lecture

# Action telle que la vue la lit : champs du plan, plus l'annee civile et le
# libelle de l'UG. Pure, testee.
.action_pour_vue <- function(a, annee_base, labels = NULL) {
  a$version <- .version_action(a)
  cible <- suppressWarnings(as.integer(a$annee_cible %||% NA))
  a$annee <- if (is.na(cible)) NULL else if (cible >= 1000L) cible else annee_base + cible
  a$ug_label <- if (!is.null(labels) && !is.null(a$ug_id)) unname(labels[a$ug_id]) %||% a$ug_id else a$ug_id
  a$propose_par_claude <- identical(a$source$origine, "claude")
  # Toujours un tableau JSON, meme avec une seule famille.
  a$objectifs_lies <- as.list(as.character(unlist(a$objectifs_lies)))
  a
}

# Empreinte du contenu d'une action telle qu'elle est stockee : change a
# chaque modification, meme dans la seconde (les horodatages du plan sont a
# la seconde). Sert a detecter qu'une action a change depuis sa lecture.
.version_action <- function(a) {
  a[c("version", "annee", "ug_label", "propose_par_claude")] <- NULL
  substr(as.character(openssl::md5(.json(a))), 1, 12)
}

.catalogue_plan <- function() {
  list(types = .ns("ACTION_PLAN_TYPES"),
       statuts = .ns("ACTION_PLAN_STATUTS"),
       priorites = .ns("ACTION_PLAN_PRIORITES"),
       familles = .ns("ACTION_PLAN_FAMILY_CODES"),
       transitions = .ns("ACTION_PLAN_TRANSITIONS"),
       types_marculus = tryCatch(.ns("MARCULUS_CONTEXT_ACTION_TYPES"), error = function(e) character()))
}

#' Action plan of a project
#'
#' Reads the project's action plan (as nemetonshiny stores it) and, for the
#' Plan view, writes `plan.geojson`: one feature per management unit with
#' its label, area and family scores.
#'
#' @param projet Project id or name.
#' @param langue `"fr"` or `"en"`.
#' @param geojson Also write the management units as GeoJSON (default
#'   `TRUE`; the view's live refresh passes `FALSE`).
#' @param historique Number of most recent history entries returned
#'   (default 300).
#' @param inclure_geojson Also return the GeoJSON inline when under 2 MB.
#' @return JSON: `projet`, `nom`, `horizon_annees`, `annee_base`, `actions`
#'   (with `annee`, the calendar year, and `ug_label`), `historique`,
#'   `commentaires_ug`, `catalogue` (types, statuses, priorities,
#'   transitions), `peut_ecrire`, and `fichier` when `geojson` is true.
#' @export
plan_actions <- function(projet, langue = "fr", geojson = TRUE, historique = 300,
                         inclure_geojson = FALSE) {
  .mcp_call(langue = langue, {
    id <- .resoudre_projet(projet)
    plan <- .ns("load_action_plan")(id)
    if (is.null(plan)) .abort("Projet {.val {id}} introuvable.", "nemetonshiny_projet_introuvable")
    p <- .ug_ids_projet(id)
    labels <- if (!is.null(p$ug)) {
      stats::setNames(.identites_ug(p$ids, p$ug$label, p$ug$cadastral_refs)$label, p$ids)
    }
    base <- .ns("action_plan_annee_base")(plan)
    actions <- lapply(plan$actions %||% list(), .action_pour_vue, annee_base = base, labels = labels)
    audit <- plan$audit %||% list()
    audit <- utils::tail(audit, max(0L, as.integer(historique %||% 300)))

    fichier <- NULL
    if (isTRUE(geojson) && !is.null(p$ug)) {
      fichier <- .ecrire_plan_geojson(id, p, langue)
    }
    list(
      projet = id, nom = p$projet$metadata$name %||% id,
      horizon_annees = plan$horizon_annees, annee_base = base,
      actions = actions, historique = rev(audit),
      commentaires_ug = tryCatch(.ns("load_ug_comments")(id), error = function(e) list()),
      catalogue = .catalogue_plan(),
      peut_ecrire = .peut_ecrire(),
      genere_a = .maintenant(),
      fichier = fichier,
      geojson = if (isTRUE(inclure_geojson)) .geojson_en_ligne(fichier)
    )
  })
}

# UG du projet pour la carte du plan : identite, surface et scores des
# familles quand le projet est calcule (contexte des propositions de Claude).
.ecrire_plan_geojson <- function(id, p, langue) {
  ug <- p$ug
  df <- sf::st_drop_geometry(ug)
  ident <- .identites_ug(as.character(df$ug_id), df$label, df$cadastral_refs)
  props <- data.frame(
    ug_id = as.character(df$ug_id),
    label = ident$label,
    label_cadastre = ident$label_cadastre,
    label_par_defaut = ident$label_par_defaut,
    parcelles = ident$parcelles,
    n_parcelles = if (!is.null(df$n_tenements)) as.integer(df$n_tenements) else NA_integer_,
    groupe = as.character(df$groupe %||% NA_character_),
    surface_ha = if (!is.null(df$surface_m2)) .arrondir(as.numeric(df$surface_m2) / 1e4, 2) else NA_real_,
    stringsAsFactors = FALSE)
  scores <- tryCatch({
    fam <- .lire_projet(id, langue)$familles
    if (is.null(fam)) NULL else .proprietes_atlas(sf::st_drop_geometry(fam))
  }, error = function(e) NULL)
  if (!is.null(scores)) {
    garder <- c("ug_id", grep("^famille_", names(scores), value = TRUE))
    props <- merge(props, scores[, garder, drop = FALSE], by = "ug_id", all.x = TRUE, sort = FALSE)
    props <- props[match(as.character(df$ug_id), props$ug_id), , drop = FALSE]
  }
  x <- sf::st_sf(props, geometry = sf::st_geometry(ug))
  crs_m <- .crs_metrique(tryCatch(as.character(p$projet$parcels$code_insee[1]), error = function(e) ""))
  fichier <- file.path(.dossier_vues("projets", id), "plan.geojson")
  .ecrire_geojson(.simplifier_wgs84(x, 1, crs_m), fichier)
  .ajouter_entete(fichier, list(project_id = id, name = p$projet$metadata$name %||% id,
                                genere_a = .maintenant()))
  normalizePath(fichier)
}

# ---------------------------------------------------------------- ecriture

# Champs qu'une vue ou Claude peut poser sur une action. Les autres
# (id, cree_par, cree_le, modifie_par, modifie_le) sont tenus par le serveur.
.CHAMPS_ACTION <- c("ug_id", "type", "type_libre", "intensite", "annee_cible", "duree",
                    "priorite", "objectifs_lies", "quantite", "statut", "source",
                    "commentaire", "date_martelage")

# Normalise une action venue d'une vue (JSON) ou de Claude : champs connus
# seulement, annee civile convertie en decalage, listes remises en vecteurs.
# Pure, testee.
.normaliser_action <- function(a, annee_base) {
  if (is.character(a)) a <- jsonlite::fromJSON(a, simplifyVector = FALSE)
  if (!is.list(a)) .abort("Action illisible.", "nemetonmcp_action_invalide")
  if (!is.null(a$annee) && is.null(a$annee_cible)) a$annee_cible <- a$annee
  a <- a[intersect(names(a), .CHAMPS_ACTION)]
  if (!is.null(a$annee_cible)) {
    v <- suppressWarnings(as.integer(a$annee_cible))
    a$annee_cible <- if (!is.na(v) && v >= 1000L) v - as.integer(annee_base) else v
  }
  if (!is.null(a$duree)) a$duree <- suppressWarnings(as.integer(a$duree))
  if (!is.null(a$objectifs_lies)) a$objectifs_lies <- as.character(unlist(a$objectifs_lies))
  if (!is.null(a$ug_id)) a$ug_id <- as.character(a$ug_id)
  a
}

.ecrire_plan <- function(id, plan) {
  if (!isTRUE(.ns("save_action_plan")(id, plan))) {
    .abort("Le plan d'actions n'a pas pu \u00eatre enregistr\u00e9.", "nemetonmcp_plan_non_enregistre")
  }
}

.relire_action <- function(id, action_id) {
  .ns("get_action_by_id")(.ns("load_action_plan")(id), action_id)
}

.exiger_ecriture <- function() {
  if (!.peut_ecrire()) .abort("Modification du plan r\u00e9serv\u00e9e aux r\u00f4les d'\u00e9criture.", "nemetonmcp_droits")
}

# Le message de nemetonshiny peut contenir des accolades : pas d'interpolation.
.erreur_validation <- function(e) {
  rlang::abort(paste("Action refus\u00e9e par nemeton :", conditionMessage(e)),
               class = c("nemetonmcp_action_invalide", "nemetonmcp_erreur"))
}

# Libelles des UG, pour l'apercu d'une confirmation.
.labels_ug <- function(u) {
  if (is.null(u) || !"label" %in% names(u$ug)) return(NULL)
  stats::setNames(as.character(u$ug$label), u$ids)
}

#' Add an action to a project's plan
#'
#' Two steps (confirmation token): without `jeton`, nothing is written and
#' the error `confirmation_requise` carries a preview and a token; the same
#' call with the token writes.
#'
#' @param projet Project id or name.
#' @param action Object: `ug_id`, `type` (`type_libre` when `autre`),
#'   `annee` (calendar year) or `annee_cible` (offset), `priorite`, and
#'   optionally `statut` (default `proposee`), `intensite`, `duree`,
#'   `objectifs_lies`, `quantite`, `commentaire`, `source`
#'   (`{origine: "claude"}` for a suggestion by Claude).
#' @param jeton Confirmation token of the first call.
#' @return JSON: the action as stored.
#' @export
ajouter_action <- function(projet, action, jeton = NULL) {
  .mcp_call({
    .exiger_ecriture()
    id <- .resoudre_projet(projet)
    plan <- .ns("load_action_plan")(id)
    base <- .ns("action_plan_annee_base")(plan)
    a <- .normaliser_action(action, base)
    a$statut <- a$statut %||% "proposee"
    u <- .ug_ids_projet(id)
    # Controles metier avant le jeton : une action refusee ne coute pas
    # d'aller-retour de confirmation.
    tryCatch(.ns("add_action_to_plan")(plan, a, ug_ids = u$ids, user = .utilisateur_courant()),
             error = .erreur_validation)
    .confirmer_ecriture("ajouter_action", id, jeton, lien = list(action = a),
                        apercu = .apercu_action("ajout", a, base, .labels_ug(u)))
    # Confirme : relu du disque juste avant d'ecrire.
    plan <- .ns("load_action_plan")(id)
    avant <- length(plan$actions)
    plan <- tryCatch(.ns("add_action_to_plan")(plan, a, ug_ids = u$ids, user = .utilisateur_courant()),
                     error = .erreur_validation)
    .ecrire_plan(id, plan)
    # Relu du disque : la version doit etre celle que lira plan_actions().
    nouvelle <- plan$actions[[avant + 1L]]$id
    list(projet = id, action = .action_pour_vue(.relire_action(id, nouvelle), base))
  })
}

#' Change fields of an action
#'
#' Two steps (confirmation token), as [ajouter_action()].
#'
#' @param projet Project id or name.
#' @param action_id Action id.
#' @param modifications Object of the fields to change (same names as in
#'   [ajouter_action()]); `annee` is a calendar year.
#' @param attendu Optional `version` of the action as the caller last read
#'   it (field of [plan_actions()] actions): when the action changed since,
#'   nothing is written and the error `conflit` carries the current action.
#' @param jeton Confirmation token of the first call.
#' @return JSON: the action as stored.
#' @export
modifier_action <- function(projet, action_id, modifications, attendu = NULL, jeton = NULL) {
  .mcp_call({
    .exiger_ecriture()
    id <- .resoudre_projet(projet)
    plan <- .ns("load_action_plan")(id)
    base <- .ns("action_plan_annee_base")(plan)
    actuelle <- .action_existante(plan, action_id)
    .verifier_version(actuelle, attendu, base)
    m <- .normaliser_action(modifications, base)
    if (!length(m)) .abort("Aucune modification.", "nemetonmcp_action_invalide")
    u <- .ug_ids_projet(id)
    tryCatch(.ns("update_action_in_plan")(plan, action_id, m, ug_ids = u$ids, user = .utilisateur_courant()),
             error = .erreur_validation)
    # La version de l'action fait partie du lien : modifiee par quelqu'un
    # entre l'apercu et la confirmation, le jeton est refuse.
    .confirmer_ecriture("modifier_action", id, jeton,
                        lien = list(action_id = action_id, version = .version_action(actuelle), modifications = m),
                        apercu = c(.apercu_action("modification", actuelle, base, .labels_ug(u)),
                                   list(modifications = m)))
    plan <- .ns("load_action_plan")(id)
    plan <- tryCatch(.ns("update_action_in_plan")(plan, action_id, m, ug_ids = u$ids, user = .utilisateur_courant()),
                     error = .erreur_validation)
    .ecrire_plan(id, plan)
    list(projet = id, action = .action_pour_vue(.relire_action(id, action_id), base))
  })
}

#' Remove an action from a project's plan
#'
#' The removal stays in the plan's history. Two steps (confirmation token),
#' as [ajouter_action()].
#'
#' @param projet Project id or name.
#' @param action_id Action id.
#' @param jeton Confirmation token of the first call.
#' @return JSON: `projet`, `action_id`.
#' @export
supprimer_action <- function(projet, action_id, jeton = NULL) {
  .mcp_call({
    .exiger_ecriture()
    id <- .resoudre_projet(projet)
    plan <- .ns("load_action_plan")(id)
    base <- .ns("action_plan_annee_base")(plan)
    actuelle <- .action_existante(plan, action_id)
    .confirmer_ecriture("supprimer_action", id, jeton,
                        lien = list(action_id = action_id, version = .version_action(actuelle)),
                        apercu = .apercu_action("suppression", actuelle, base, .labels_ug(.ug_ids_projet(id))))
    plan <- .ns("load_action_plan")(id)
    plan <- .ns("delete_action_from_plan")(plan, action_id, user = .utilisateur_courant())
    .ecrire_plan(id, plan)
    list(projet = id, action_id = action_id)
  })
}

.action_existante <- function(plan, action_id) {
  a <- .ns("get_action_by_id")(plan, action_id)
  if (is.null(a)) {
    .abort("Action {.val {action_id}} introuvable : elle a peut-\u00eatre \u00e9t\u00e9 supprim\u00e9e.",
           "nemetonmcp_action_introuvable")
  }
  a
}

.verifier_version <- function(actuelle, attendu, base) {
  if (!is.null(attendu) && nzchar(attendu) && !identical(attendu, .version_action(actuelle))) {
    rlang::abort(sprintf("L'action a \u00e9t\u00e9 modifi\u00e9e par %s pendant votre saisie.",
                         actuelle$modifie_par %||% actuelle$cree_par %||% "un autre compte"),
                 class = c("nemetonmcp_conflit", "nemetonmcp_erreur"),
                 candidats = list(.action_pour_vue(actuelle, base)))
  }
}

# ---------------------------------------------------------------- profils, Marculus

#' Expert profiles
#'
#' The YAML profiles of nemetonshiny (package and user ones), served so that
#' a view or Claude answers with the chosen point of view. They stay the
#' single source.
#'
#' @param langue `"fr"` or `"en"`.
#' @return JSON: `profils`, a list of `cle`, `libelle`, `consigne`.
#' @export
profils_experts <- function(langue = "fr") {
  .mcp_call(langue = langue, {
    langue <- if (identical(langue, "en")) "en" else "fr"
    profils <- .ns("get_expert_profiles")()
    list(profils = unname(lapply(names(profils), function(k) {
      p <- profils[[k]]
      list(cle = k,
           libelle = p$label[[langue]] %||% p$label$fr %||% k,
           consigne = p$prompt[[langue]] %||% p$prompt$fr %||% "")
    })))
  })
}

#' Marculus field bundle of a project
#'
#' One GeoPackage per marking action (thinning, clear-cut, respacing,
#' observation) and the `.marsync` of their contexts, zipped, as
#' nemetonshiny's « Marculus » button does. Nothing in the plan changes.
#'
#' @param projet Project id or name.
#' @return JSON: `fichier` (zip in the project's exports), counts.
#' @export
exporter_marculus <- function(projet) {
  .mcp_call({
    .exiger_ecriture()
    id <- .resoudre_projet(projet)
    dossier <- file.path(.ns("get_project_path")(id), "exports")
    dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(dossier, sprintf("marculus_%s.zip", format(Sys.time(), "%Y%m%d-%H%M%S")))
    res <- .ns("marculus_export_bundle")(id, f)
    if (!isTRUE(res$n_contexts > 0)) {
      .abort("Aucune action de martelage dans le plan (\u00e9claircie, coupe rase, d\u00e9pressage, observation).",
             "nemetonmcp_marculus_vide")
    }
    list(projet = id, fichier = normalizePath(f), chantiers = res$n_contexts,
         geopackages = res$n_gpkg, avec_desserte = isTRUE(res$has_desserte),
         fonds_ortho = res$n_ortho %||% 0L)
  })
}
