# Contrat nemeton -> nemetonmcp (mission 016).
#
# ## Ce que nemetonmcp lit dans nemeton
#
# `nemetonmcp` appelle 12 fonctions exportees du coeur `nemeton` (toujours en
# `nemeton::nom()`, declare dans `Imports` de r/DESCRIPTION), et lit des
# colonnes dans deux tables qu'il rend :
#
#   fichier            fonction(s)                                  lecture
#   -----------------  -------------------------------------------  ----------------------------
#   R/atlas.R          get_famille_col, list_indicators,            colonne d'une famille ;
#                      normalize_indicator                          codes d'indicateurs (character)
#   R/regeneration.R   regen_rank_species, european_species_        classement long (9 colonnes) ;
#                      tolerances                                   traits (code, frost_winter_min_c,
#                                                                   thermophily)
#   R/connaissances.R  db_connect, db_disconnect,                   -
#                      retrieve_knowledge, format_citations
#   R/rasters.R        read_fordead_dieback_mask,                   manifeste RECONFORT (colonnes
#                      read_fordead_layer, reconfort_cache_manifest ci-dessous)
#
# Les tests de nemetonmcp simulent en outre `nemeton:::.mistral_embed`
# (test-connaissances.R) : objet interne, verrouille ci-dessous.
#
# Ce test verrouille ces objets : une fonction renommee, retiree, ou dont les
# arguments changent fait echouer la CI ici, pas chez l'utilisateur. Changer
# ces tables = changer le contrat : prevenir `nemeton` et mettre ce bloc a
# jour. Le dernier test relit r/R/ et echoue si le code appelle un objet de
# `nemeton` absent des tables.
#
# `nemeton` est dans `Imports` : la CI l'installe (job `r` de ci.yml), donc ce
# test ne saute jamais. Aucun skip_*.

# Fonctions : noms des arguments, dans l'ordre (formals()).
.contrat_nemeton_fonctions <- list(
  get_famille_col             = c("code"),
  list_indicators             = c("category", "return_type", "conditionnels"),
  normalize_indicator         = c("indicator", "values", "statut"),
  regen_rank_species          = c("units", "species_pool", "top_n", "weights", "exclude_invasive",
                                  "region", "cover_col", "lai_col", "extinction_k", "id_col",
                                  "include_atlas", "..."),
  european_species_tolerances = c("statut", "confiance", "type", "include_invasif"),
  retrieve_knowledge          = c("con", "query", "top_k", "family_codes", "profile_codes",
                                  "min_similarity", "lang", "embed_provider", "api_key"),
  db_connect                  = c("url", "read_only", "connect_timeout"),
  db_disconnect               = c("con"),
  format_citations            = c("retrieved_chunks", "format", "lang"),
  read_fordead_dieback_mask   = c("con", "zone_id", "run_id", "cache_dir", "apply_zone_mask",
                                  "mask_polygon"),
  read_fordead_layer          = c("con", "zone_id", "layer", "run_id", "cache_dir",
                                  "apply_zone_mask", "mask_polygon"),
  reconfort_cache_manifest    = c("cache_dir", "zone_id", "run_id", "include_range")
)

# Objet interne simule par les tests de nemetonmcp (existence seulement).
.contrat_nemeton_internes <- c(".mistral_embed")

# Colonnes lues par nemetonmcp dans les tables rendues.
.contrat_nemeton_colonnes_classement <- c("ug_id", "rank", "species_code", "label", "type",
                                          "suitability", "limiting_factor", "confidence")
.contrat_nemeton_colonnes_traits <- c("code", "frost_winter_min_c", "thermophily")

.contrat_nemeton_noms <- function() names(.contrat_nemeton_fonctions)

test_that("le contrat nemeton compte 12 fonctions, sans doublon", {
  expect_length(.contrat_nemeton_noms(), 12L)
  expect_false(anyDuplicated(.contrat_nemeton_noms()) > 0L)
})

test_that("chaque fonction de nemeton utilisee existe, exportee, avec les memes arguments", {
  exportes <- getNamespaceExports("nemeton")
  for (nom in .contrat_nemeton_noms()) {
    expect_true(nom %in% exportes, info = paste("non exportee ou absente :", nom))
    obj <- getExportedValue("nemeton", nom)
    expect_true(is.function(obj), info = paste("pas une fonction :", nom))
    expect_identical(names(formals(obj)), .contrat_nemeton_fonctions[[nom]],
                     info = paste("signature changee :", nom))
  }
})

test_that("les objets internes simules par les tests existent", {
  ns <- asNamespace("nemeton")
  for (nom in .contrat_nemeton_internes) {
    expect_true(exists(nom, envir = ns, inherits = FALSE), info = paste("objet absent :", nom))
  }
})

test_that("list_indicators() rend des codes d'indicateurs (character) et get_famille_col() une colonne", {
  ind <- nemeton::list_indicators()
  expect_type(ind, "character")
  expect_gt(length(ind), 0L)
  expect_true(all(grepl("^indicateur_", ind)))
  col <- nemeton::get_famille_col("B")
  expect_type(col, "character")
  expect_length(col, 1L)
  expect_true(startsWith(col, "famille_"))
})

test_that("normalize_indicator() rend un vecteur numerique de meme longueur", {
  n <- nemeton::normalize_indicator(nemeton::list_indicators()[1], c(0.2, 0.5), statut = NULL)
  expect_length(n, 2L)
  expect_true(is.numeric(as.numeric(n)))
})

test_that("european_species_tolerances() porte les colonnes lues par le filtre de froid", {
  tol <- nemeton::european_species_tolerances()
  expect_s3_class(tol, "data.frame")
  expect_true(all(.contrat_nemeton_colonnes_traits %in% names(tol)))
})

test_that("regen_rank_species() rend le classement long aux colonnes lues", {
  sq <- function(x) sf::st_polygon(list(rbind(c(x, 6685000), c(x + 100, 6685000), c(x + 100, 6685100),
                                              c(x, 6685100), c(x, 6685000))))
  u <- sf::st_sf(ug_id = c("a", "b"), tmax_moyenne = c(30, 31), d_tmax = c(1, 2),
                 vpd_canicule = c(2, 3), rew_min = c(0.1, 0.3),
                 geometry = sf::st_sfc(sq(843000), sq(843100), crs = 2154))
  rk <- nemeton::regen_rank_species(u, top_n = 2, region = "BFC")
  expect_s3_class(rk, "data.frame")
  expect_true(all(.contrat_nemeton_colonnes_classement %in% names(rk)))
})

test_that("nemetonmcp n'appelle aucune fonction de nemeton hors contrat", {
  dir <- testthat::test_path("..", "..", "R")
  fichiers <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  # Paquet installe (couverture, R CMD check) : r/R/ ne tient plus de .R.
  skip_if(length(fichiers) == 0L, "sources R absentes (paquet installe)")
  appelles <- unique(unlist(lapply(
    fichiers, function(f) {
      lignes <- readLines(f, encoding = "UTF-8", warn = FALSE)
      lignes <- lignes[!grepl("^\\s*#", lignes)]
      m <- regmatches(lignes, gregexpr("\\bnemeton:::?[A-Za-z_.][A-Za-z0-9_.]*", lignes))
      sub("^nemeton:::?", "", unlist(m))
    })))
  expect_gt(length(appelles), 0L)
  expect_setequal(setdiff(appelles, .contrat_nemeton_noms()), character())
  # Et la table ne garde pas d'objet que le code n'appelle plus.
  expect_setequal(intersect(.contrat_nemeton_noms(), appelles), .contrat_nemeton_noms())
})
