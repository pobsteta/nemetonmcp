# Utilitaires communs aux outils MCP de nemetonmcp.
#
# Meme convention de reponse que le serveur MCP de nemetonshiny
# (R/service_mcp.R) : une chaine JSON courte, `{"ok":true,...}` ou
# `{"ok":false,"erreur","classe","candidats"}`. stdout est le canal du
# protocole MCP (stdio) : rien n'y est ecrit ici, les messages partent sur
# stderr via cli.

`%||%` <- function(x, y) if (is.null(x)) y else x

.json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null",
                                na = "null", digits = NA))
}

# Evalue `expr`. Un succes rend `{"ok":true,...}`. Une erreur rend la meme
# forme que nemetonshiny (brief du 2026-11-10, section 2) :
# `{"ok":false,"classe","code","langue","erreur","action","candidats"}`, plus
# les champs d'une confirmation (`projet`, `projet_nom`, `jeton`, `apercu`,
# `expire_a`), et la chaine porte l'attribut `mcp_erreur` : `outils_mcp()` la
# fait passer par `.mcp_vers_outil()` de nemetonshiny, qui la marque
# `isError`. Jamais d'exception de protocole.
.mcp_call <- function(expr, langue = "fr") {
  res <- tryCatch(expr, error = function(e) structure(list(e), class = "mcp_echec"))
  if (inherits(res, "mcp_echec")) return(.erreur_mcp(res[[1]], langue))
  .json(Filter(Negate(is.null), c(list(ok = TRUE), res)))
}

# Code court d'une erreur : la classe sans son prefixe (`nemetonshiny_` ou
# `nemetonmcp_`), alignee sur les codes de nemetonshiny quand le cas existe
# (`projet_introuvable`, `calcul_en_cours`...). Une erreur R sans classe
# connue : `erreur_inconnue`. Pure, testee.
.code_erreur <- function(e) {
  cls <- grep("^(nemetonshiny|nemetonmcp)_", class(e), value = TRUE)
  cls <- setdiff(cls, c("nemetonshiny_erreur", "nemetonmcp_erreur"))
  if (length(cls)) return(list(classe = cls[1], code = sub("^nemeton(shiny|mcp)_", "", cls[1])))
  if (inherits(e, c("nemetonshiny_erreur", "nemetonmcp_erreur"))) {
    return(list(classe = grep("_erreur$", class(e), value = TRUE)[1], code = "erreur"))
  }
  list(classe = "nemetonmcp_erreur_inconnue", code = "erreur_inconnue")
}

# Suite a proposer, par code : phrases propres a nemetonmcp (decision de
# Pascal : seuls les codes s'alignent sur nemetonshiny, pas les textes). Les
# clients decident sur `code` et lisent `action`.
.ACTIONS_ERREUR <- list(
  fr = c(
    projet_introuvable = "Lister les projets (lister_projets) et v\u00e9rifier le nom avec l'utilisateur.",
    projet_ambigu = "Proposer les candidats \u00e0 l'utilisateur et le laisser choisir.",
    projet_verrouille = "Le projet est ouvert dans nemetonshiny : r\u00e9essayer quand il sera ferm\u00e9.",
    projet_ancien = "Proposer de recr\u00e9er le projet avec les m\u00eames parcelles.",
    calcul_en_cours = "Suivre etat_calcul et r\u00e9essayer quand la t\u00e2che en cours sera termin\u00e9e.",
    sans_indicateurs = "Proposer lancer_calcul, puis suivre etat_calcul.",
    confirmation_requise = "Montrer l'aper\u00e7u \u00e0 l'utilisateur ; s'il confirme, rappeler l'outil avec les m\u00eames arguments et le jeton.",
    jeton_invalide = "Redemander un aper\u00e7u : rappeler l'outil sans jeton.",
    parcelles_introuvables = "Signaler \u00e0 l'utilisateur les parcelles introuvables.",
    commune_introuvable = "V\u00e9rifier l'orthographe avec l'utilisateur, ou demander une commune voisine.",
    departement_introuvable = "V\u00e9rifier le d\u00e9partement avec l'utilisateur.",
    departement_ambigu = "Proposer les candidats \u00e0 l'utilisateur et le laisser choisir.",
    section_introuvable = "V\u00e9rifier la section cadastrale avec l'utilisateur.",
    selection_vide = "Demander \u00e0 l'utilisateur de choisir au moins une parcelle.",
    nom_manquant = "Demander un nom de projet \u00e0 l'utilisateur.",
    action_invalide = "Relire le message et corriger l'action (unit\u00e9, type, ann\u00e9e).",
    action_introuvable = "Relire le plan avec plan_actions : l'action a pu \u00eatre supprim\u00e9e.",
    conflit = "Montrer \u00e0 l'utilisateur la version actuelle (candidats) avant de r\u00e9\u00e9crire.",
    ug_introuvable = "V\u00e9rifier l'unit\u00e9 de gestion (ug_id ou nom).",
    indicateur_inconnu = "V\u00e9rifier le code de l'indicateur (B1, A5...).",
    couche_absente = "Choisir une couche parmi les candidats.",
    marculus_vide = "Aucune action de martelage dans le plan : rien \u00e0 exporter.",
    droits = "Op\u00e9ration r\u00e9serv\u00e9e \u00e0 un r\u00f4le d'\u00e9criture : s'adresser \u00e0 l'administrateur du realm.",
    erreur = "Signaler l'erreur \u00e0 l'administrateur du serveur nemeton.",
    erreur_inconnue = "Signaler l'erreur \u00e0 l'administrateur du serveur nemeton."),
  en = c(
    projet_introuvable = "List the projects (lister_projets) and check the name with the user.",
    projet_ambigu = "Offer the candidates to the user and let them choose.",
    projet_verrouille = "The project is open in nemetonshiny: try again once it is closed.",
    projet_ancien = "Offer to recreate the project with the same parcels.",
    calcul_en_cours = "Follow etat_calcul and try again once the running task is over.",
    sans_indicateurs = "Offer lancer_calcul, then follow etat_calcul.",
    confirmation_requise = "Show the preview to the user; if they confirm, call the tool again with the same arguments and the token.",
    jeton_invalide = "Ask for a new preview: call the tool again without a token.",
    parcelles_introuvables = "Tell the user which parcels were not found.",
    commune_introuvable = "Check the spelling with the user, or ask for a neighbouring commune.",
    departement_introuvable = "Check the department with the user.",
    departement_ambigu = "Offer the candidates to the user and let them choose.",
    section_introuvable = "Check the cadastral section with the user.",
    selection_vide = "Ask the user to pick at least one parcel.",
    nom_manquant = "Ask the user for a project name.",
    action_invalide = "Read the message and correct the action (unit, type, year).",
    action_introuvable = "Read the plan again with plan_actions: the action may have been removed.",
    conflit = "Show the user the current version (candidats) before writing again.",
    ug_introuvable = "Check the management unit (ug_id or name).",
    indicateur_inconnu = "Check the indicator code (B1, A5...).",
    couche_absente = "Pick a layer among the candidates.",
    marculus_vide = "No marking action in the plan: nothing to export.",
    droits = "Operation reserved to a write role: contact the realm administrator.",
    erreur = "Report the error to the nemeton server administrator.",
    erreur_inconnue = "Report the error to the nemeton server administrator.")
)

.action_erreur <- function(code, langue = "fr") {
  t <- .ACTIONS_ERREUR[[if (identical(langue, "en")) "en" else "fr"]]
  unname(if (code %in% names(t)) t[[code]] else t[["erreur"]])
}

# Chaine JSON d'erreur, marquee `mcp_erreur`. Pure, testee.
.erreur_mcp <- function(e, langue = "fr") {
  langue <- if (identical(langue, "en")) "en" else "fr"
  ce <- .code_erreur(e)
  texte <- gsub("\\s*\n\\s*", " ", trimws(cli::ansi_strip(conditionMessage(e))))
  x <- Filter(Negate(is.null), list(
    ok = FALSE, classe = ce$classe, code = ce$code, langue = langue,
    erreur = texte, action = .action_erreur(ce$code, langue),
    candidats = e$candidats,
    projet = if (!is.null(e$projet_nom) || !is.null(e$jeton)) e$projet,
    projet_nom = e$projet_nom, jeton = e$jeton, apercu = e$apercu, expire_a = e$expire_a))
  structure(.json(x), mcp_erreur = TRUE)
}

# Variable d'environnement `NEMETON_MCP_<nom>`. Faute de quoi, l'ancienne
# `NEMETON_CLAUDE_<nom>` (avant 0.8.0) est encore lue, avec un avertissement
# (une fois par variable et par session).
.env_mcp <- function(nom, defaut = "") {
  nouvelle <- paste0("NEMETON_MCP_", nom)
  x <- Sys.getenv(nouvelle, NA)
  if (!is.na(x)) return(x)
  ancienne <- paste0("NEMETON_CLAUDE_", nom)
  x <- Sys.getenv(ancienne, NA)
  if (is.na(x)) return(defaut)
  cli::cli_warn("{.envvar {ancienne}} est obsol\u00e8te : la renommer en {.envvar {nouvelle}}.",
                .frequency = "once", .frequency_id = ancienne)
  x
}

.abort <- function(message, class, ...) {
  cli::cli_abort(message, class = c(class, "nemetonmcp_erreur"), ...,
                 call = NULL, .envir = parent.frame())
}

# Fonction interne de nemetonshiny. Un seul point d'acces, pour que la
# dependance aux internes reste visible et facile a remplacer le jour ou
# nemetonshiny les exporte.
.ns <- function(nom) utils::getFromNamespace(nom, "nemetonshiny")

# Minuscules, sans accents ni ponctuation ; « St » et « Ste » developpes.
# Table explicite plutot qu'iconv(ASCII//TRANSLIT), qui depend de la locale
# (un serveur MCP lance en locale C perdrait les lettres accentuees).
.ACCENTS <- c(
  de = "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f9\u00fa\u00fb\u00fc\u00fd\u00ff",
  vers = "aaaaaaceeeeiiiinooooouuuuyy"
)

.normaliser <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  x <- chartr(.ACCENTS[["de"]], .ACCENTS[["vers"]], x)
  x <- gsub("\u0153", "oe", gsub("\u00e6", "ae", x))
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\bste\\b", "sainte", x)
  x <- gsub("\\bst\\b", "saint", x)
  trimws(gsub(" +", " ", x))
}

# Dossier ou sont ecrits les fichiers des vues (GeoJSON a joindre a un
# artefact). L'option `nemetonmcp.dossier_vues` (posee par le connecteur
# distant en mode isolation, un dossier par utilisateur) l'emporte sur
# `NEMETON_MCP_VUES_DIR`, qui l'emporte sur le cache utilisateur.
.dossier_vues <- function(...) {
  racine <- getOption("nemetonmcp.dossier_vues", .env_mcp("VUES_DIR"))
  if (!nzchar(racine)) racine <- file.path(tools::R_user_dir("nemetonmcp", "cache"), "vues")
  d <- file.path(racine, ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  normalizePath(d, mustWork = TRUE)
}

# Systeme metrique adapte a la commune, pour simplifier et mesurer en metres.
.crs_metrique <- function(insee) {
  dep <- substr(as.character(insee), 1, 3)
  switch(dep,
         "971" = 5490, "972" = 5490,  # RGAF09 / UTM 20N
         "973" = 2972,                # RGFG95 / UTM 22N
         "974" = 2975,                # RGR92 / UTM 40S
         "976" = 4471,                # RGM04 / UTM 38S
         2154)                        # RGF93 / Lambert-93
}

# Simplifie en metres puis repasse en WGS84 (RFC 7946).
.simplifier_wgs84 <- function(x, tolerance_m, crs_m = 2154) {
  x <- sf::st_transform(x, crs_m)
  if (tolerance_m > 0) {
    x <- sf::st_simplify(x, preserveTopology = TRUE, dTolerance = tolerance_m)
    x <- x[!sf::st_is_empty(x), ]
  }
  sf::st_transform(x, 4326)
}

# Ecrit un GeoJSON WGS84 compact (6 decimales, ~10 cm) et rend son chemin.
.ecrire_geojson <- function(x, fichier, precision = 6L) {
  if (file.exists(fichier)) unlink(fichier)
  sf::st_write(x, fichier, driver = "GeoJSON", quiet = TRUE,
               layer_options = c(sprintf("COORDINATE_PRECISION=%d", precision),
                                 "RFC7946=YES", "WRITE_NAME=NO"))
  normalizePath(fichier)
}

# Ajoute un membre etranger (RFC 7946, 6.1) en tete d'un FeatureCollection :
# `{"type":"FeatureCollection","nemeton":{...},"features":[...]}`.
.ajouter_entete <- function(fichier, entete, membre = "nemeton") {
  gj <- jsonlite::read_json(fichier, simplifyVector = FALSE)
  out <- list(type = "FeatureCollection")
  out[[membre]] <- entete
  out$features <- gj$features
  writeLines(.json(out), fichier, useBytes = TRUE)
  invisible(fichier)
}

.emprise <- function(x) {
  b <- sf::st_bbox(sf::st_transform(x, 4326))
  round(unname(as.numeric(b)), 6)
}

.taille <- function(fichier) unname(file.size(fichier))

# Lit un GeoJSON pour l'inclure tel quel dans une reponse MCP (appel depuis
# une vue par la capacite `mcp`), seulement s'il reste leger.
.geojson_en_ligne <- function(fichier, plafond = 2e6) {
  if (is.null(fichier) || !file.exists(fichier) || .taille(fichier) > plafond) return(NULL)
  jsonlite::read_json(fichier, simplifyVector = FALSE)
}

.maintenant <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

# Nom affiche des unites de gestion (UGF). A la creation d'un projet,
# nemetonshiny fait une UGF par parcelle et lui donne pour nom la reference
# cadastrale de la parcelle (IDU). Tant que ce nom par defaut n'a pas ete
# change dans l'application, les vues affichent « UGF <n> » (n tire de
# l'identifiant ug_<n>) et gardent la parcelle en information secondaire.
# Un nom choisi dans nemetonshiny est affiche tel quel.
.RE_IDU <- "^[0-9]{5}[0-9A-Z]{3}[0-9A-Z]{2}[0-9]{4}$"

# « 212000000A0036 » -> « A 36 » ; une reference d'une autre forme reste telle.
.ref_courte <- function(idu) {
  idu <- as.character(idu)
  ok <- grepl(.RE_IDU, idu)
  section <- sub("^0+", "", substr(idu, 9, 10))
  numero <- suppressWarnings(as.integer(substr(idu, 11, 14)))
  ifelse(ok & !is.na(numero), paste(section, numero), idu)
}

#' Display identity of management units (pure function, tested)
#'
#' @param ug_id,label Character vectors (nemetonshiny's ids and labels).
#' @param refs Cadastral references of each unit, comma separated
#'   (`cadastral_refs` of nemetonshiny's `ug_build_sf()`), or `NULL`.
#' @return data.frame: `label` (displayed name), `label_cadastre` (the
#'   stored label), `label_par_defaut`, `parcelles` (short references,
#'   comma separated).
#' @noRd
.identites_ug <- function(ug_id, label, refs = NULL) {
  n <- length(ug_id)
  label <- as.character(label %||% rep(NA_character_, n))
  refs <- if (is.null(refs)) rep("", n) else as.character(refs)
  refs[is.na(refs)] <- ""
  liste <- lapply(strsplit(refs, ",", fixed = TRUE), function(x) { x <- trimws(x); x[nzchar(x)] })
  defaut <- vapply(seq_len(n), function(i) {
    l <- label[i]
    is.na(l) || !nzchar(trimws(l)) || l %in% liste[[i]] || grepl(.RE_IDU, l)
  }, logical(1))
  numero <- sub("^.*?([0-9]+)$", "\\1", ug_id)
  a_numero <- grepl("[0-9]+$", ug_id)
  nom <- ifelse(defaut & a_numero, paste("UGF", numero),
                ifelse(is.na(label) | !nzchar(label), ug_id, label))
  data.frame(label = nom, label_cadastre = label, label_par_defaut = defaut,
             parcelles = vapply(liste, function(x) paste(.ref_courte(x), collapse = ", "), ""),
             stringsAsFactors = FALSE)
}
