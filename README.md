# Calcul Automatique de l'Évapotranspiration Potentielle (ETP) - Thornthwaite (1948)

Ce dépôt contient un script **R** interactif permettant d'estimer l'Évapotranspiration Potentielle (ETP) selon la méthodologie de **Thornthwaite (1948)**. Le script prend en charge le traitement automatisé de fichiers Excel multi-feuilles et s'adapte aux pas de temps mensuel et journalier.

---

## 🌟 Fonctionnalités

Le script principal (`main_etp_thornthwaite.R`) propose un menu interactif lors de son exécution :

1. **ETP Mensuelle (Thornthwaite classique) :**
   - Calcul basé sur la température moyenne mensuelle ($T_{\text{moy}}$) et la photopériode théorique mensuelle.
2. **ETP Journalière (Redistribuée) :**
   - Adaptation de la méthode originale à l'échelle journalière.
   - Prise en compte de la variabilité journalière de la température et du calcul dynamique de la durée astronomique du jour ($N_j$) selon la latitude et la déclinaison solaire.

---

## 📁 Structure du Projet

```text
ETP-Thornthwaite-R/
├── main_etp_thornthwaite.R   # Script R principal (interactif)
├── docs/
│   └── Thornthwaite_1948_Climate_Classification.pdf  # Article original de 1948
├── .gitignore                # Exclusion des fichiers temporaires et Excel
└── README.md                 # Documentation du projet