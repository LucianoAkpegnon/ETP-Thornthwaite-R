# ===============================================================
# 📌 CALCUL AUTOMATIQUE DE L’ETP AVEC THORNTHWAITE
#    Option : Mensuelle OU Journalière redistribuée
# ===============================================================

# ===============================================================
# 📌 MÉTHODOLOGIE : Adaptation de Thornthwaite pour l’ETP journalière
#FORMULE DE THORNTHWAITE : ETP_mens = 16 * (Nm / 12) * (10 * T_moy / I)^a

# Avec :
# - ETP_mens : évapotranspiration potentielle mensuelle (mm/mois)
# - T_moy   : température moyenne mensuelle (°C), si <0 alors 0
# - Nm      : durée moyenne du jour pour le mois (heures)
# - I       : indice thermique annuel = Σ (T_moy/5)^1.514 sur 12 mois
# - a       : coefficient empirique = 6.75e-7*I^3 - 7.71e-5*I^2 + 1.792e-2*I + 0.49239
#
# NOTE : T_moy < 0°C → contribution = 0 pour l'indice I
# La formule de Thornthwaite (1948) est initialement conçue


# pour l’ETP mensuelle. Pour obtenir une estimation journalière,
# la méthode a été adaptée comme suit :
#
# 1) Indice thermique annuel (I)
#    I = Σ (T/5)^1.514   (somme sur les mois > 0 °C)
#
# 2) Coefficient empirique (a)
#    a = 6.75e-7 * I^3 - 7.71e-5 * I^2 + 1.792e-2 * I + 0.49239
#
# 3) Durée astronomique du jour (N_j)
#    - Calculée pour chaque jour en fonction de la latitude
#      et de la déclinaison solaire journalière.
#
# 4) Formule journalière adaptée
#    ETP_j = 16 * (N_j / 12) * ((10 * T_j / I)^a)
#
#    avec :
#    - N_j : durée du jour au jour j (h)
#    - T_j : température moyenne journalière (°C)
#    - I   : indice thermique annuel
#    - a   : coefficient empirique
#
# 5) Avantage
#    Cette adaptation conserve la logique de Thornthwaite,
#    mais intègre la variabilité journalière de la température
#    et de la photopériode → cohérence entre ETP journalière
#    et ETP mensuelle.
#
# ---------------- PSEUDO-CODE SIMPLIFIÉ ----------------
# Pour chaque année :
#   - Calculer I et a
#   Pour chaque jour j :
#       - Calculer la déclinaison solaire δ
#       - Calculer N_j (durée du jour)
#       - Récupérer T_j (température du jour)
#       - Calculer ETP_j avec la formule adaptée

#BIBLIOGRAPHIE : Thornthwaite, C.W. An approach toward a rational classification of climate. Geographical Review, 38(1), 55–94. 1948.
# ===============================================================


#==================== CHARGEMENT DES PACKAGES ====================
packages <- c("readxl", "openxlsx", "dplyr", "rstudioapi", "beepr")
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

#==================== CALCUL DURÉE DU JOUR ====================
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

#==================== CALCUL ETP MENSUELLE ====================
calcul_etp_thornthwaite <- function(df) {
  df <- df %>% mutate(Nm = case_when(
    MOIS == 2 & ((AN %% 4 == 0 & AN %% 100 != 0) | (AN %% 400 == 0)) ~ 29,
    MOIS == 2 ~ 28,
    MOIS %in% c(4, 6, 9, 11) ~ 30,
    TRUE ~ 31
  ))
  
  col_lat <- names(df)[grepl("Latitude_", names(df))]
  if (length(col_lat) == 0) stop("⚠️ Pas de colonne de latitude trouvée.")
  if (length(col_lat) > 1) stop("⚠️ Plusieurs colonnes latitude détectées.")
  
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

#==================== CALCUL ETP JOURNALIÈRE ====================
calcul_etp_thornthwaite_journalier <- function(df) {
  if (!all(c("AN", "MOIS", "JOUR", "T_jour") %in% names(df))) {
    stop("⚠️ Colonnes attendues : AN, MOIS, JOUR, T_jour + latitude")
  }
  
  col_lat <- names(df)[grepl("Latitude_", names(df))]
  if (length(col_lat) == 0) stop("⚠️ Pas de colonne de latitude trouvée.")
  if (length(col_lat) > 1) stop("⚠️ Plusieurs colonnes latitude détectées.")
  
  df$Latitude_finale <- df[[col_lat]]
  if (grepl("Latitude_Sud", col_lat)) df$Latitude_finale <- -abs(df$Latitude_finale)
  if (grepl("Latitude_Nord", col_lat)) df$Latitude_finale <- abs(df$Latitude_finale)
  
  df <- df %>%
    rowwise() %>%
    mutate(
      Jour_annee = as.integer(format(as.Date(sprintf("%d-%02d-%02d", AN, MOIS, JOUR)), "%j")),
      Delta = 0.409 * sin(2 * pi * Jour_annee / 365 - 1.39),
      Phi = pi * Latitude_finale / 180,
      cos_omega = pmin(pmax(-tan(Phi) * tan(Delta), -1), 1),
      omega = acos(cos_omega),
      D_jour = (24 / pi) * omega
    ) %>%
    ungroup()
  
  df_mois <- df %>%
    group_by(AN, MOIS, Latitude_finale) %>%
    summarise(
      T_moy_moy = mean(T_jour, na.rm = TRUE),
      Nm = n(),
      Lm = mean(D_jour, na.rm = TRUE),
      .groups = "drop"
    )
  
  I_par_an <- df_mois %>%
    group_by(AN) %>%
    summarise(I = sum((pmax(T_moy_moy, 0) / 5)^1.514), .groups = "drop")
  
  df_mois <- df_mois %>% left_join(I_par_an, by = "AN") %>%
    mutate(
      a = 6.75e-7 * I^3 - 7.71e-5 * I^2 + 1.792e-2 * I + 0.49239,
      ETP_mens = 16 * (Lm / 12) * (Nm / 30) * ((10 * T_moy_moy / I)^a)
    )
  
  df <- df %>% left_join(df_mois %>% select(AN, MOIS, ETP_mens), by = c("AN", "MOIS"))
  
  df <- df %>%
    group_by(AN, MOIS) %>%
    mutate(
      poids = pmax(T_jour, 0) * D_jour,
      ETP_jour = ETP_mens * (poids / sum(poids, na.rm = TRUE))
    ) %>%
    ungroup()
  
  return(df %>% select(AN, MOIS, JOUR, Latitude_finale, T_jour, D_jour, ETP_jour))
}

#==================== CHOIX DU MODE ====================
choix_mode <- showQuestion(
  title = "Choix du calcul ETP",
  message = "Voulez-vous calculer :",
  ok = "Mensuelle (Thornthwaite)",
  cancel = "Journalière (redistribuée Thornthwaite)"
)

if (isTRUE(choix_mode)) {
  mode_calcul <- "mensuel"
  message("📊 Mode sélectionné : ETP mensuelle")
} else {
  mode_calcul <- "journalier"
  message("📈 Mode sélectionné : ETP journalière")
}

#==================== TRAITEMENT MULTI-FICHIERS ====================
avertir_popup("OUVERTURE DE LA FENÊTRE DE SÉLECTION DES FICHIERS")

fichier <- rstudioapi::selectFile(caption = "Sélectionnez un fichier Excel",
                                  filter = "Fichiers Excel (*.xlsx)",
                                  existing = TRUE)

if (is.null(fichier)) stop("Aucun fichier sélectionné.")
fichiers <- c(fichier)   # vecteur pour garder la logique

avertir_popup("OUVERTURE DE LA FENÊTRE DE SÉLECTION DU DOSSIER DE SAUVEGARDE")

dossier_sortie <- rstudioapi::selectDirectory(caption = "Choisissez le dossier de sauvegarde")
if (is.null(dossier_sortie)) stop("Aucun dossier de sortie choisi.")

for (f in fichiers) {
  nom_base <- tools::file_path_sans_ext(basename(f))
  feuilles <- readxl::excel_sheets(f)
  resultats_par_feuille <- list()
  
  for (feuille in feuilles) {
    df <- readxl::read_excel(f, sheet = feuille)
    
    if (mode_calcul == "mensuel") {
      if (!all(c("AN", "MOIS", "T_moy_moy") %in% names(df)) || !any(grepl("Latitude_", names(df)))) {
        warning(paste("Feuille", feuille, "du fichier", f, "ignorée : colonnes manquantes."))
        next
      }
      res <- calcul_etp_thornthwaite(df)
    } else {
      if (!all(c("AN", "MOIS", "JOUR", "T_jour") %in% names(df)) || !any(grepl("Latitude_", names(df)))) {
        warning(paste("Feuille", feuille, "du fichier", f, "ignorée : colonnes manquantes."))
        next
      }
      res <- calcul_etp_thornthwaite_journalier(df)
    }
    
    resultats_par_feuille[[feuille]] <- res
  }
  
  if (length(resultats_par_feuille) > 0) {
    fichier_sortie <- file.path(dossier_sortie, paste0("ETP_", nom_base, ".xlsx"))
    openxlsx::write.xlsx(resultats_par_feuille, fichier_sortie)
    cat("\u2705 Résultats sauvegardés dans:", fichier_sortie, "\n")
  }
}

cat("\n\u2728 Traitement terminé pour tous les fichiers.\n")
