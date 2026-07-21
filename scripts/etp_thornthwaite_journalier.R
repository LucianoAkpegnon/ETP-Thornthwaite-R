# ===============================================================
# 📌 GESTION AUTOMATIQUE DE LA LATITUDE ET DE L’HÉMISPHÈRE
#
# Ce script détecte automatiquement la colonne de latitude
# selon son nom dans le fichier Excel :
#
#   👉 "Latitude_Nord" → Latitude positive (hémisphère Nord)
#   👉 "Latitude_Sud"  → Latitude négative (hémisphère Sud)
#
# Il adapte ensuite automatiquement les calculs de la durée
# du jour (Lm) en fonction de la position géographique réelle.
#
# ⚠️ Assurez-vous qu’un seul de ces deux noms soit présent
# dans chaque feuille, et qu’aucune autre colonne n’utilise
# le mot "Latitude_" dans son nom.
#
# ➕ Ce système permet de mélanger plusieurs stations issues
# des deux hémisphères, dans un même fichier ou répertoire.
# ===============================================================

#==================== CHARGEMENT DES PACKAGES ====================
packages <- c("readxl", "openxlsx", "dplyr", "tcltk", "beepr")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

#==================== MESSAGE D'AVERTISSEMENT VISUEL ====================
avertir_popup <- function(message) {
  ruban <- paste0(rep(c("🟥", "🟩"), 15), collapse = "")
  cat("\n", ruban, "\n")
  cat("🔔 ", toupper(message), "\n")
  cat(ruban, "\n\n")
  beepr::beep(2)
}

#==================== CALCUL DE LA DURÉE DU JOUR (Lm) ====================
declinaison <- function(jour) {
  0.409 * sin(2 * pi * jour / 365 - 1.39)
}

duree_jour <- function(lat, mois) {
  jours_moyens <- c(15, 45, 75, 105, 135, 162, 198, 228, 258, 288, 318, 344)
  jour <- jours_moyens[mois]
  phi <- pi * lat / 180
  delta <- declinaison(jour)
  cos_omega <- -tan(phi) * tan(delta)
  cos_omega <- pmin(pmax(cos_omega, -1), 1)
  omega <- acos(cos_omega)
  return((24 / pi) * omega)
}

#==================== CALCUL ETP PAR THORNTHWAITE ====================
calcul_etp_thornthwaite <- function(df) {
  df <- df %>% mutate(Nm = case_when(
    MOIS == 2 & ((AN %% 4 == 0 & AN %% 100 != 0) | (AN %% 400 == 0)) ~ 29,
    MOIS == 2 ~ 28,
    MOIS %in% c(4, 6, 9, 11) ~ 30,
    TRUE ~ 31
  ))
  
  # Détection automatique du nom de la colonne de latitude
  col_lat <- names(df)[grepl("Latitude_", names(df))]
  
  if (length(col_lat) == 0) stop("⚠️ Aucune colonne de latitude trouvée (attendu : 'Latitude_Nord' ou 'Latitude_Sud').")
  if (length(col_lat) > 1) stop("⚠️ Plusieurs colonnes 'Latitude_*' détectées. Merci de ne conserver qu'une seule.")
  
  # Traitement selon l’hémisphère
  df$Latitude_finale <- df[[col_lat]]
  if (grepl("Latitude_Sud", col_lat)) df$Latitude_finale <- -abs(df$Latitude_finale)
  if (grepl("Latitude_Nord", col_lat)) df$Latitude_finale <- abs(df$Latitude_finale)
  
  df <- df %>% mutate(Lm = mapply(duree_jour, Latitude_finale, MOIS))
  
  I_par_an <- df %>%
    group_by(AN) %>%
    summarise(I = sum((pmax(T_moy_moy, 0) / 5)^1.514))
  
  df <- df %>% left_join(I_par_an, by = "AN")
  df <- df %>% mutate(
    a = 6.75e-7 * I^3 - 7.71e-5 * I^2 + 1.792e-2 * I + 0.49239,
    ETP = 16 * (Lm / 12) * (Nm / 30) * ((10 * T_moy_moy / I)^a)
  )
  
  return(df %>% select(AN, MOIS, Latitude_finale, T_moy_moy, Nm, Lm, I, a, ETP))
}

#==================== TRAITEMENT MULTI-FICHIERS & MULTI-FEUILLES ====================
avertir_popup("OUVERTURE DE LA FENÊTRE DE SÉLECTION DES FICHIERS")
fichiers <- tk_choose.files(caption = "Sélectionnez les fichiers Excel", multi = TRUE,
                            filters = matrix(c("Fichiers Excel", "*.xlsx"), ncol = 2))
if (length(fichiers) == 0) stop("Aucun fichier sélectionné.")

avertir_popup("OUVERTURE DE LA FENÊTRE DE SÉLECTION DU DOSSIER DE SAUVEGARDE")
dossier_sortie <- tk_choose.dir(caption = "Choisissez le dossier de sauvegarde")
if (is.na(dossier_sortie)) stop("Aucun dossier de sortie choisi.")

for (f in fichiers) {
  nom_base <- tools::file_path_sans_ext(basename(f))
  feuilles <- readxl::excel_sheets(f)
  resultats_par_feuille <- list()
  
  for (feuille in feuilles) {
    df <- readxl::read_excel(f, sheet = feuille)
    if (!all(c("AN", "MOIS", "T_moy_moy") %in% names(df)) || !any(grepl("Latitude_", names(df)))) {
      warning(paste("Feuille", feuille, "du fichier", f, "ignorée : colonnes manquantes."))
      next
    }
    res <- calcul_etp_thornthwaite(df)
    resultats_par_feuille[[feuille]] <- res
  }
  
  if (length(resultats_par_feuille) > 0) {
    fichier_sortie <- file.path(dossier_sortie, paste0("ETP_", nom_base, ".xlsx"))
    openxlsx::write.xlsx(resultats_par_feuille, fichier_sortie)
    cat("\u2705 Résultats sauvegardés dans:", fichier_sortie, "\n")
  }
}

cat("\n\u2728 Traitement terminé pour tous les fichiers.\n")
