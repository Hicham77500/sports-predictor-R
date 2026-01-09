# Sports Predictor (R) ⚽

Système complet de prédiction de résultats de matchs de football utilisant les **Elo ratings** et le **machine learning**.

Prédiction des résultats H/D/A (Victoire domicile / Match nul / Victoire extérieur) avec un pipeline R complet : préparation des données, calcul des Elo ratings dynamiques, entraînement de modèles caret, API REST plumber et interface Shiny interactive.

## 📋 Stack Technique

### Requis
- **R** >= 4.2 (Rscript ou RStudio/VS Code)
- **Packages CRAN**: tidyverse, lubridate, caret, nnet, plumber, shiny, jsonlite, httr, tibble, readr
- **Optionnel**: renv pour figer l'environnement

### Architecture
- **Backend**: API REST avec plumber
- **Frontend**: Interface Shiny interactive
- **ML**: Classification multiclasse (caret + nnet)
- **Features**: Elo ratings dynamiques, statistiques d'équipes, données temporelles

## Structure du projet (principaux fichiers)
```
data/
  raw/matches.csv            # données brutes
  raw/matches_augmented.csv  # données enrichies pour le modèle v2
  processed/                 # sorties éventuelles de prétraitement
models/
  model_v2.rds               # modèle caret entraîné (multiclasse H/D/A)
  elo_map.rds                # Elo courant par équipe
  elo_params.rds             # paramètres Elo utilisés
R/
  elo_utils.R                # calcul Elo + features + split temporel
  train_outcome_model.R      # pipeline d'entraînement (sauve model_v2.rds)
  evaluate_model.R           # métriques et diagnostics
  predict_outcome.R          # prédiction à partir des noms d'équipes
  predict_helpers.R          # utilitaires de prédiction batch
  train_model.R              # ancien pipeline simple (conservé)
  train_augmented.R          # pipeline alternatif avec features supplémentaires
  team_base_elo.R            # Elo init pour équipes inconnues
  plots_for_presentation.R   # génère 7 graphiques vers figures/
api/
  api.R                      # endpoints plumber (/health, /predict_v2, /elo, /top_teams, /stats)
  entrypoint_api.R           # lance l'API sur http://127.0.0.1:8001
shiny-app/
  app.R                      # interface Shiny (port 3838)
figures/                     # 7 PNG générés par plots_for_presentation.R
quickstart.sh                # menu interactif pour actions rapides
ARCHITECTURE.md              # description technique et flux
FILES_GUIDE.md               # guide des fichiers et usages
CLEANUP_SUMMARY.txt          # historique des renommages/suppressions
```

## Démarrage rapide
1) Installer les packages CRAN requis
```r
install.packages(c("tidyverse","lubridate","caret","nnet","plumber","shiny","jsonlite","httr","tibble","readr"))
```

2) Entraîner le modèle (crée models/model_v2.rds, elo_map.rds, elo_params.rds)
```bash
Rscript R/train_outcome_model.R
```

3) Lancer l'API (port 8001)
```bash
Rscript api/entrypoint_api.R
# puis tester : curl -s "http://localhost:8001/predict_v2?home_team=PSG&away_team=Barcelona"
```

4) Lancer Shiny (port 3838)
```bash
Rscript -e 'shiny::runApp("shiny-app", host="0.0.0.0", port=3838)'
# ouvrir http://127.0.0.1:3838
```

5) Générer les figures de présentation (7 PNG 3000x1800)
```bash
Rscript R/plots_for_presentation.R
ls figures/
```

6) Menu interactif (optionnel)
```bash
bash quickstart.sh
```

## Modules clés
| Fichier | Rôle |
|---------|------|
| R/elo_utils.R | Calcul Elo, recent form, préparation train/test |
| R/train_outcome_model.R | Entraînement et sauvegarde du modèle v2 |
| R/evaluate_model.R | Diagnostics (accuracy, log-loss, brier, calibration) |
| R/predict_outcome.R | Prédictions H/D/A à partir des noms d'équipes |
| api/api.R | Endpoints REST (/health, /predict_v2, /elo, /top_teams, /stats) |
| api/entrypoint_api.R | Démarrage de l'API sur 8001 |
| shiny-app/app.R | Interface web pour requêter le modèle |
| R/plots_for_presentation.R | Génération des 7 graphiques dans figures/ |

## Données et modèles
- Entrée principale : data/raw/matches_augmented.csv (907 lignes si dataset fourni)
- Modèles/artefacts : models/model_v2.rds, models/elo_map.rds, models/elo_params.rds
- Équipes inconnues : Elo initial depuis R/team_base_elo.R (sinon 1500 par défaut)

## Utilisation
- **Shiny** : sélectionner deux équipes, choisir qui joue à domicile, cliquer « Prédire » pour obtenir proba H/D/A + Elo.
- **API** :
  ```bash
  curl -s "http://localhost:8001/predict_v2?home_team=PSG&away_team=Barcelona"
  ```
  Réponse: prediction (H/D/A), probabilités H/D/A, elo_diff, flags home_known/away_known.

## Tests et validation
- Test rapide du pipeline de prédiction :
```bash
Rscript R/test_prediction.R
```
- Évaluation détaillée :
```bash
Rscript R/evaluate_model.R
```

## Dépannage
- **Modèle non trouvé** : relancer `Rscript R/train_outcome_model.R` pour générer models/model_v2.rds.
- **Shiny ne démarre pas / port occupé** : `pkill -f "shiny"; sleep 2; Rscript -e 'shiny::runApp("shiny-app", host="0.0.0.0", port=3838)'`
- **API ne répond pas** : vérifier `Rscript api/entrypoint_api.R` et le port 8001.
- **Équipe inconnue** : Elo par défaut 1500 (ou valeur de team_base_elo.R si présente).

## Documentation locale
- ARCHITECTURE.md — vue d'ensemble et flux
- FILES_GUIDE.md — rôle des scripts et chemins de données/modèles
- CLEANUP_SUMMARY.txt — changements récents et renommages
- quickstart.sh — menu interactif (entraînement, API, Shiny, figures)

## Licence
MIT (adapter si besoin)
