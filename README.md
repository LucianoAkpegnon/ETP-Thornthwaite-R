# Calcul Automatique de l'Évapotranspiration Potentielle (ETP) - Thornthwaite (1948)

Ce dépôt contient un script **R** interactif permettant d'estimer l'Évapotranspiration Potentielle (ETP) selon la méthodologie de **Thornthwaite (1948)**. Le script prend en charge le traitement automatisé de fichiers Excel multi-feuilles et s'adapte aux pas de temps mensuel et journalier.

---

## 🌟 Fonctionnalités

Le script principal (`main_etp_thornthwaite.R`) propose un menu interactif lors de son exécution :

1. **ETP Mensuelle (Thornthwaite classique) :**
   - Calcul basé sur la température moyenne mensuelle (`T_moy_mens`) et la photopériode théorique mensuelle.
2. **ETP Journalière (Redistribuée) :**
   - Adaptation de la méthode originale à l'échelle journalière.
   - Prise en compte de la variabilité journalière de la température et du calcul dynamique de la durée astronomique du jour (`N_j`) selon la latitude et la déclinaison solaire.
3. **Gestion Automatique de la Latitude :**
   - Prise en compte dynamique de l'hémisphère (Nord/Sud) pour le calcul de l'ensoleillement, déduite directement des noms de colonnes.

---

<details>
<summary><b>🧠 Cliquer ici pour voir la Méthodologie Mathématique détaillée</b></summary>

### 1️⃣ Méthode Classique : ETP Mensuelle
La formule originale de Thornthwaite est calculée comme suit :

$$ETP_{mens} = 16 \times \left( \frac{N_m}{12} \right) \times \left( \frac{10 \times T_{mens}}{I} \right)^a$$

Avec :
- $ETP_{mens}$ : Évapotranspiration potentielle mensuelle (mm/mois).
- $T_{mens}$ : Température moyenne mensuelle (°C). (Correspond à la colonne `T_moy_mens`). Si $< 0$, on retient $0$.
- $N_m$ : Durée théorique moyenne du jour pour le mois considéré (heures).
- $I$ : Indice thermique annuel, calculé comme la somme sur 12 mois :

$$I = \sum \left( \frac{T_{mens}}{5} \right)^{1.514}$$

- $a$ : Coefficient empirique fonction de $I$ :

$$a = 6.75 \times 10^{-7} I^3 - 7.71 \times 10^{-5} I^2 + 1.792 \times 10^{-2} I + 0.49239$$

### 2️⃣ Adaptation : ETP Journalière Redistribuée
Pour obtenir une estimation journalière cohérente, la méthode a été adaptée par une approche de redistribution proportionnelle :

1. **Durée astronomique du jour ($N_j$) :** Calculée pour chaque jour $j$ en fonction de la latitude du site et de la déclinaison solaire journalière.
2. **Calcul de l'ETP mensuelle globale ($ETP_{mens}$) :** Calculée avec la méthode classique détaillée ci-dessus.
3. **Redistribution Journalière ($ETP_{jour}$) :** L'ETP mensuelle est ensuite redistribuée sur chaque jour du mois au pro-rata d'un poids journalier ($P_j$) combinant température et insolation :

$$P_j = T_j \times N_j$$
*(avec $T_j = 0$ si la température journalière est $< 0$)*

$$ETP_{jour} = ETP_{mens} \times \left( \frac{P_j}{\sum P_j} \right)$$

**Avantage :** Cette méthode conserve rigoureusement le volume évaporatoire mensuel de Thornthwaite, tout en distribuant la demande évaporatoire selon la variabilité journalière réelle de la température et de la photopériode.

</details>

---

## 📁 Structure du Projet

```text
ETP-Thornthwaite-R/
├── main_etp_thornthwaite.R   # Script R principal (interactif)
├── data/
│   └── Modèle_Thornthwaite.xlsx  # Fichier Excel modèle pour le formatage attendu
├── docs/
│   └── Thornthwaite_1948-An_Approach_toward_a_Rational_Classification_of_Climate.pdf  # Article original
├── .gitignore                # Exclusion des fichiers temporaires
└── README.md                 # Documentation du projet