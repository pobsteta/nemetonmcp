# Journal des durees : une ligne JSON par appel d'outil MCP (nom, duree,
# utilisateur, succes ou code d'erreur, projet vise). Feuille de route de
# specs/ARCHITECTURE-nemeton-llm.md, etape 3 : « mesurer avant d'optimiser ».
#
# Le fichier (JSON Lines) est ecrit hors de stdout, qui porte le protocole
# MCP en stdio ; une ecriture qui echoue ne fait jamais echouer l'outil.
# NEMETON_MCP_JOURNAL : chemin du fichier, ou « non » pour ne rien ecrire ;
# par defaut `outils.jsonl` sous tools::R_user_dir("nemetonmcp", "data").
# Au-dela de 5 Mo, le fichier passe en `.1` (une seule generation gardee).

.JOURNAL_MAX_OCTETS <- 5 * 1024^2

# Chemin du journal, ou NULL s'il est coupe. Pure (lit l'option et
# l'environnement), testee.
.chemin_journal <- function() {
  x <- getOption("nemetonmcp.journal", .env_mcp("JOURNAL"))
  if (isFALSE(x) || tolower(trimws(as.character(x))) %in% c("non", "no", "false", "0", "off")) return(NULL)
  if (is.null(x) || !nzchar(x)) x <- file.path(tools::R_user_dir("nemetonmcp", "data"), "outils.jsonl")
  x
}

# Succes ou code d'erreur d'un resultat d'outil : une erreur de nemetonmcp ou
# de nemetonshiny passe par `ellmer::ContentToolResult(error = <json>)`, ou
# reste une chaine marquee `mcp_erreur`. Pure, testee.
.issue_outil <- function(res) {
  texte <- NULL
  if (inherits(res, "ellmer::ContentToolResult")) {
    err <- tryCatch(res@error, error = function(e) NULL)
    if (is.null(err)) return(list(ok = TRUE, code = NULL))
    texte <- if (inherits(err, "condition")) conditionMessage(err) else as.character(err)
  } else if (isTRUE(attr(res, "mcp_erreur"))) {
    texte <- as.character(res)
  } else {
    return(list(ok = TRUE, code = NULL))
  }
  x <- tryCatch(jsonlite::fromJSON(texte, simplifyVector = FALSE), error = function(e) NULL)
  list(ok = FALSE, code = if (is.list(x)) x$code %||% "erreur" else "erreur")
}

.ecrire_journal <- function(ligne, chemin = .chemin_journal()) {
  if (is.null(chemin)) return(invisible(FALSE))
  tryCatch(suppressWarnings({
    dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(chemin) && isTRUE(file.size(chemin) > .JOURNAL_MAX_OCTETS)) {
      file.rename(chemin, paste0(chemin, ".1"))
    }
    cat(.json(Filter(Negate(is.null), ligne)), "\n", file = chemin, append = TRUE, sep = "")
    invisible(TRUE)
  }), error = function(e) invisible(FALSE))
}

# Evalue l'appel d'outil `expr` (promesse) en le chronometrant, puis ecrit la
# ligne du journal. `projet` : argument du meme nom, s'il existe et a ete
# fourni.
.chronometrer <- function(nom, expr, projet = NULL) {
  debut <- proc.time()[["elapsed"]]
  ts <- .maintenant()
  issue <- list(ok = FALSE, code = "exception")
  on.exit({
    p <- tryCatch(as.character(unlist(projet))[1], error = function(e) NULL)
    .ecrire_journal(list(
      ts = ts, outil = nom,
      duree_ms = round(1000 * (proc.time()[["elapsed"]] - debut)),
      utilisateur = getOption("nemetonmcp.utilisateur", Sys.getenv("USER", NA)),
      transport = getOption("nemetonmcp.transport", "stdio"),
      projet = if (length(p) && !is.na(p)) p,
      ok = issue$ok, code = issue$code))
  }, add = TRUE)
  res <- expr
  issue <- .issue_outil(res)
  res
}

# Enveloppe un outil ellmer : meme nom, description, arguments et
# annotations ; son appel passe par .chronometrer().
.journaliser <- function(outil) {
  f <- S7::S7_data(outil)
  noms <- names(formals(f))
  appel <- as.call(c(list(quote(.f)), stats::setNames(lapply(noms, as.name), noms)))
  g <- f
  body(g) <- as.call(c(list(quote(.chronometrer), quote(.nom), appel),
                       if ("projet" %in% noms) list(projet = quote(projet))))
  environment(g) <- list2env(list(.f = f, .nom = outil@name, .chronometrer = .chronometrer),
                             parent = environment(f))
  S7::S7_data(outil) <- g
  outil
}

# Resume du journal par outil (pure, testee) : appels, erreurs, duree
# mediane, 95e centile et maximum en millisecondes, du plus lent au plus
# rapide (mediane).
.resumer_journal <- function(lignes) {
  vide <- data.frame(outil = character(), appels = integer(), erreurs = integer(),
                     mediane_ms = numeric(), p95_ms = numeric(), max_ms = numeric(),
                     stringsAsFactors = FALSE)
  if (!length(lignes)) return(vide)
  outil <- vapply(lignes, function(l) as.character(l$outil %||% NA), "")
  duree <- vapply(lignes, function(l) as.numeric(l$duree_ms %||% NA), 0)
  ok <- vapply(lignes, function(l) isTRUE(l$ok), TRUE)
  par <- split(seq_along(outil), outil)
  out <- do.call(rbind, lapply(names(par), function(n) {
    i <- par[[n]]
    d <- duree[i]
    data.frame(outil = n, appels = length(i), erreurs = sum(!ok[i]),
               mediane_ms = stats::median(d, na.rm = TRUE),
               p95_ms = unname(stats::quantile(d, 0.95, na.rm = TRUE, type = 7)),
               max_ms = max(d, na.rm = TRUE), stringsAsFactors = FALSE)
  }))
  out <- out[order(-out$mediane_ms, out$outil), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Durations of the MCP tool calls
#'
#' Summarises the call journal written by the server (one JSON line per
#' tool call: tool, duration, user, success or error code, project), to see
#' where time goes before optimising. The journal is written to
#' `NEMETON_MCP_JOURNAL` (a path, or `"non"` to disable it); by default to
#' `outils.jsonl` under `tools::R_user_dir("nemetonmcp", "data")`.
#'
#' @param fichier Journal file.
#' @param depuis Only calls at or after this time (`POSIXct` or
#'   `"AAAA-MM-JJ"`), or `NULL` for all.
#' @param utilisateur Only the calls of this user, or `NULL` for all.
#' @return data.frame, one row per tool: `outil`, `appels`, `erreurs`,
#'   `mediane_ms`, `p95_ms`, `max_ms`, slowest (median) first.
#' @export
journal_outils <- function(fichier = .chemin_journal(), depuis = NULL, utilisateur = NULL) {
  lignes <- if (!is.null(fichier) && file.exists(fichier)) {
    txt <- readLines(fichier, warn = FALSE, encoding = "UTF-8")
    Filter(Negate(is.null), lapply(txt[nzchar(txt)], function(l) {
      tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE), error = function(e) NULL)
    }))
  } else list()
  if (!is.null(depuis)) {
    seuil <- as.POSIXct(depuis)
    quand <- as.POSIXct(vapply(lignes, function(l) as.character(l$ts %||% NA), ""),
                        format = "%Y-%m-%dT%H:%M:%S")
    lignes <- lignes[!is.na(quand) & quand >= seuil]
  }
  if (!is.null(utilisateur)) {
    lignes <- Filter(function(l) identical(as.character(l$utilisateur %||% ""), utilisateur), lignes)
  }
  .resumer_journal(lignes)
}
