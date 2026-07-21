# Calcul Automatique de l'Évapotranspiration Potentielle (ETP) - Thornthwaite (1948)

Ce dépôt contient les scripts **R** permettant de calculer l'Évapotranspiration Potentielle (ETP) selon la méthodologie de **Thornthwaite (1948)**.

## 📁 Structure du Projet

- `scripts/etp_thornthwaite_mensuel.R` : Script pour le calcul mensuel classique basé sur les températures moyennes mensuelles et la photopériode.
- `scripts/etp_thornthwaite_journalier.R` : Script d'adaptation pour le calcul journalier redistribué (prenant en compte la variabilité journalière de la température et la durée astronomique du jour $N_j$).
- `docs/` : Contient l'article scientifique princeps de Thornthwaite (1948).

## 🛠️ Prérequis et Packages R

Les scripts vérifient et installent automatiquement les packages suivants si nécessaires :
`readxl`, `openxlsx`, `dplyr`, `rstudioapi`, `beepr`.

## 📚 Référence Scientifique
- **Thornthwaite, C. W. (1948)**. *An approach toward a rational classification of climate*. Geographical Review, 38(1), 55–94. (Disponible dans le dossier `docs/`).