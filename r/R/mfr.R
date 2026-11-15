# Essences eligibles aux aides de l'Etat en Bourgogne-Franche-Comte
# (materiels forestiers de reproduction) : arrete regional
# N DRAAF/SREAF-2026-09 du 6 juillet 2026 (BFC-2026-07-06-00003), annexe 1.1.
# Codes du coeur nemeton (european_species_tolerances()). « objectif » :
# essence principale d'un boisement subventionne ; « accompagnement » :
# seulement associee a une essence objectif. Toute autre essence est hors liste.
# Brief aigora-nemeton du 2026-11-15 (complement « arrete regional MFR »).

.MFR_BFC <- list(
  arrete = "Arrêté régional MFR Bourgogne-Franche-Comté N°DRAAF/SREAF-2026-09 du 6 juillet 2026, annexe 1.1",
  url = "https://draaf.bourgogne-franche-comte.agriculture.gouv.fr/IMG/pdf/ap_mfr_bfc_26_raa.pdf",
  objectif = c(
    # Feuillus
    "acer_campestre", "acer_platanoides", "acer_pseudoplatanus", "alnus_glutinosa", "castanea_sativa",
    "fagus_sylvatica", "juglans_regia", "juglans_nigra", "juglans_nigra_x_regia", "populus_spp_et_hybrides",
    "populus_nigra", "prunus_avium", "quercus_cerris", "quercus_petraea", "quercus_pubescens", "quercus_robur",
    "quercus_rubra", "robinia_pseudoacacia", "sorbus_domestica", "sorbus_torminalis", "tilia_cordata",
    "tilia_platyphyllos",
    # Resineux (pinus_nigra : pin noir d'Autriche, de Salzmann, laricio de Calabre et de Corse)
    "abies_alba", "abies_bornmulleriana", "abies_cephalonica", "cedrus_atlantica", "larix_decidua",
    "larix_x_eurolepis", "picea_abies", "pinus_nigra", "pinus_pinaster", "pinus_sylvestris",
    "pseudotsuga_menziesii"),
  accompagnement = c(
    "acer_opalus", "alnus_incana", "alnus_cordata", "betula_pendula", "betula_pubescens", "carpinus_betulus",
    "malus_sylvestris", "populus_tremula", "pyrus_pyraster", "sorbus_aria", "sorbus_aucuparia",
    "ulmus_glabra", "ulmus_laevis", "ulmus_minor",
    "abies_nordmanniana", "cupressus_arizonica", "pinus_pinea", "pinus_brutia", "pinus_halepensis")
)

# Statut d'aide de chaque code : "objectif", "accompagnement" ou "hors_liste".
.statut_mfr <- function(codes) {
  ifelse(codes %in% .MFR_BFC$objectif, "objectif",
         ifelse(codes %in% .MFR_BFC$accompagnement, "accompagnement", "hors_liste"))
}
