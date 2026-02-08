# ESG Environmental Performance — EUROSTOXX 600 (R Project)

##  Objectif
Analyser les déterminants de la **performance environnementale** des entreprises européennes cotées dans l’indice **EUROSTOXX 600**, mesurée par le **Environmental Pillar Score (EPS)**.

Le projet est réalisé en **deux parties** :
1) Régression linéaire multiple (statistiques descriptives + dataviz + modèle)  
2) Analyse en Composantes Principales (ACP) sur des variables financières

---

##  Données
- Indice : EUROSTOXX 600  
- Variable expliquée : **EPS (Environmental Pillar Score)**
- Dataset : `partie 1-régression/data_eurostoxx600.xlsx`

---

##  Partie 1 — Statistiques descriptives & Régression
**Objectif :** identifier les variables économiques, financières et de gouvernance qui influencent EPS.

Étapes :
- nettoyage et préparation des données (types, NA, renommage)
- statistiques descriptives + visualisations
- régression multiple (avec et sans variable YTD)
- vérification de la multicolinéarité

Rapport : `partie 1-régression/report_part1_regression.pdf`  
Script : `partie 1-régression/script_part1_regression.R`

---

##  Partie 2 — ACP (Analyse en Composantes Principales)
**Objectif :** réduire la dimension des variables financières (retours à différentes périodes) et tester leur contribution à EPS via des composantes.

Résultats clés :
- les 2 premières composantes expliquent ~63% de la variance
- les composantes ACP ne sont pas significatives pour expliquer EPS
- la variable YTD utilisée directement en Partie 1 est plus informative

Rapport : `partie-2-acp/report_part2_acp.pdf`  
Script : `partie-2-acp/script_part2_acp.R`

---

##  Outils
- R (dataviz, régression, ACP)
