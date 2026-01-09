# 🚀 Quick Start Guide

## Installation

### 1️⃣ Cloner le repository
```bash
git clone https://github.com/yourusername/sports-predictor-R.git
cd sports-predictor-R
```

### 2️⃣ Installer les dépendances
```r
# Dans R ou RStudio
packages <- c("tidyverse", "lubridate", "caret", "nnet", 
              "plumber", "shiny", "jsonlite", "httr", "tibble", "readr")
install.packages(packages)
```

## Lancer le projet

### Option 1: Entraîner le modèle
```bash
Rscript R/train_outcome_model.R
```
Génère les fichiers modèles dans `models/`

### Option 2: Lancer l'API
```bash
Rscript api/entrypoint_api.R
```
API disponible sur `http://127.0.0.1:8001`

**Endpoints disponibles:**
- `GET /health` - Vérifier le statut
- `POST /predict_v2` - Prédire un match
- `GET /elo/:team` - Elo courant d'une équipe
- `GET /top_teams` - Top 10 équipes par Elo
- `GET /stats/:team` - Statistiques d'une équipe

### Option 3: Lancer l'interface Shiny
```bash
Rscript -e "shiny::runApp('shiny-app/app.R', port=3838)"
```
Interface disponible sur `http://localhost:3838`

## Exemple d'utilisation de l'API

```bash
# Prédire un match
curl -X POST http://127.0.0.1:8001/predict_v2 \
  -H "Content-Type: application/json" \
  -d '{
    "home_team": "PSG",
    "away_team": "Marseille"
  }'
```

## Structure

```
├── R/                      # Scripts R
│   ├── elo_utils.R        # Calcul Elo
│   ├── train_outcome_model.R  # Entraînement
│   └── predict_outcome.R   # Prédictions
├── api/                    # API REST
├── shiny-app/              # Interface web
├── data/                   # Données
├── models/                 # Modèles entraînés
└── notebooks/              # Analyses
```

## Documentation

- 📖 [ARCHITECTURE.md](ARCHITECTURE.md) - Architecture technique
- 📄 [FILES_GUIDE.md](FILES_GUIDE.md) - Détail des fichiers
- 🤝 [CONTRIBUTING.md](CONTRIBUTING.md) - Guide contribution
- 📝 [CHANGELOG.md](CHANGELOG.md) - Historique des versions

## Troubleshooting

### Les packages ne s'installent pas ?
```r
# Essayez une approche alternative
install.packages("tidyverse", dependencies=TRUE)
```

### L'API ne démarre pas ?
- Vérifiez le port 8001 est disponible
- Consultez les logs: `tail -f api_server.log`

### Modèle pas trouvé ?
```bash
Rscript R/train_outcome_model.R  # Entraîner d'abord
```

## Support

Pour un problème :
1. Consultez la section [Troubleshooting](#troubleshooting)
2. Ouvrez une [Issue](https://github.com/yourusername/sports-predictor-R/issues)
3. Vérifiez les [Issues fermées](https://github.com/yourusername/sports-predictor-R/issues?state=closed)

## License

MIT © Hicham Guendouz - voir [LICENSE](LICENSE)
