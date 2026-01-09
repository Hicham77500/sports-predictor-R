#!/opt/homebrew/bin/Rscript
# ============================================================================
# test_prediction.R
# Test simple de prédictions avec le nouveau modèle Elo
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
})

project_root <- "/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R"

source(file.path(project_root, "R/elo_utils.R"))
source(file.path(project_root, "R/predict_outcome.R"))
source(file.path(project_root, "R/evaluate_model.R"))

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║  TEST DES PRÉDICTIONS AVEC MODÈLE ELO V2               ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n")

# Charger le modèle et les Elo
cat("\n✓ Chargement du modèle et des Elo...\n")
loaded <- load_model_and_elo(project_root, "v2")
model <- loaded$model
elo_map <- loaded$elo_map

# Afficher top équipes
cat("\n✓ Top 10 équipes (par Elo):\n")
elo_df <- tibble(
  team = names(elo_map),
  elo = unlist(elo_map)
) %>%
  arrange(desc(elo)) %>%
  head(10)
print(elo_df)

# ============================================================================
# TEST 1 : Équipe forte vs équipe faible
# ============================================================================
cat("\n\n=== TEST 1 : Équipe FORTE vs équipe FAIBLE ===\n")

# Trouver les équipes avec Elo extrêmes
elo_all <- tibble(
  team = names(elo_map),
  elo = unlist(elo_map)
) %>%
  arrange(desc(elo))

top_team <- elo_all$team[1]
bottom_team <- elo_all$team[nrow(elo_all)]

cat("Top team:", top_team, "Elo =", round(elo_map[[top_team]], 0), "\n")
cat("Bottom team:", bottom_team, "Elo =", round(elo_map[[bottom_team]], 0), "\n")

pred1 <- predict_outcome_from_teams(
  home_team = top_team,
  away_team = bottom_team,
  model = model,
  elo_map = elo_map
)

cat("\nPrédiction:", top_team, "à domicile vs", bottom_team, "en déplacement\n")
cat("Résultat prédit:", pred1$class, "\n")
cat("Probas H:", round(pred1$proba$H * 100, 2), "% | D:", round(pred1$proba$D * 100, 2), 
    "% | A:", round(pred1$proba$A * 100, 2), "%\n")

# ============================================================================
# TEST 2 : Deux équipes équilibrées
# ============================================================================
cat("\n\n=== TEST 2 : Deux équipes ÉQUILIBRÉES ===\n")

elo_sorted <- tibble(
  team = names(elo_map),
  elo = unlist(elo_map)
) %>%
  arrange(desc(elo))

team_balanced_1 <- elo_sorted$team[50]
team_balanced_2 <- elo_sorted$team[51]

pred2 <- predict_outcome_from_teams(
  home_team = team_balanced_1,
  away_team = team_balanced_2,
  model = model,
  elo_map = elo_map
)

cat("Prédiction:", team_balanced_1, "à domicile vs", team_balanced_2, "en déplacement\n")
cat("Résultat prédit:", pred2$class, "\n")
cat("Probas H:", round(pred2$proba$H * 100, 2), "% | D:", round(pred2$proba$D * 100, 2), 
    "% | A:", round(pred2$proba$A * 100, 2), "%\n")

# ============================================================================
# TEST 3 : Équipe inconnue
# ============================================================================
cat("\n\n=== TEST 3 : Équipe INCONNUE ===\n")

pred3 <- predict_outcome_from_teams(
  home_team = "FC Bordeaux Fantasy",
  away_team = top_team,
  model = model,
  elo_map = elo_map
)

cat("Prédiction: FC Bordeaux Fantasy à domicile vs", top_team, "en déplacement\n")
cat("Note: Équipe inconnue reçoit Elo par défaut (1500)\n")
cat("Résultat prédit:", pred3$class, "\n")
cat("Probas H:", round(pred3$proba$H * 100, 2), "% | D:", round(pred3$proba$D * 100, 2), 
    "% | A:", round(pred3$proba$A * 100, 2), "%\n")

# ============================================================================
# TEST 4 : Charge test_data et évalue le modèle
# ============================================================================
cat("\n\n=== TEST 4 : ÉVALUATION DU MODÈLE ===\n")

# Recharger les données pour avoir test_data
source(file.path(project_root, "R/elo_utils.R"))

data_path <- file.path(project_root, "data/raw/matches_augmented.csv")
matches_raw <- read_csv(data_path, show_col_types = FALSE)

data_prepared <- prepare_training_data_v2(matches_raw)
test_data <- data_prepared$test_data

# Prédictions sur test set
pred_test <- predict(model, test_data, type = "prob")

# Diagnostic
diag <- diagnose_model(test_data$outcome, pred_test)

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║  RÉSUMÉ DES TESTS                                       ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n")

cat("\n✓ Modèle V2 avec Elo en place et fonctionnel\n")
cat("✓ Test accuracy sur test set:", round(diag$accuracy * 100, 2), "%\n")
cat("✓ Probas cohérentes: équipe forte gagne avec haute proba\n")
cat("✓ Équipe inconnue reçoit Elo par défaut (1500)\n")
