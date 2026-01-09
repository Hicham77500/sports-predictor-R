# =============================================================================
# R/plots_for_presentation.R
# 
# Script complet pour générer 7 graphiques de présentation avec ggplot2
# pour le projet Sports Predictor
# =============================================================================

# Charger les librairies nécessaires
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(caret)
library(tidyr)

# Sourcer les fonctions nécessaires
source("R/fetch_data.R")
source("R/preprocess.R")
source("R/elo_utils.R")  # Pour obtenir elo_home, elo_away, etc.

# Créer le dossier figures s'il n'existe pas
dir.create("figures", showWarnings = FALSE)

cat("=== CHARGEMENT DES DONNÉES ===\n")

# Charger les données brutes
raw_data <- fetch_matches_data("data/raw/matches.csv")

# Préparer les données de base
prepared <- prepare_training_data(raw_data)
df <- prepared$data

# Augmenter avec les features (Elo, stats récentes, etc.) via prepare_training_data_v2
cat("Calcul des features Elo et statistiques...\n")
data_v2 <- prepare_training_data_v2(df, split_date = NULL)

# Récupérer le dataset complet avec toutes les features
df_complete <- bind_rows(data_v2$train_data, data_v2$test_data) %>%
  mutate(
    # Recréer les colonnes manquantes si nécessaire
    elo_home = home_elo_before,
    elo_away = away_elo_before
  )

# Filtrer les lignes complètes
df_complete <- df_complete %>%
  filter(!is.na(outcome), !is.na(elo_home), !is.na(elo_away))

cat(sprintf("Total de matchs : %d\n", nrow(df_complete)))

# Charger le modèle final
model_path <- "models/model_v2.rds"
if (!file.exists(model_path)) {
  model_path <- "models/model.rds"  # Fallback
}

model <- readRDS(model_path)
cat(sprintf("Modèle chargé depuis : %s\n", model_path))

# Définir un split temporel pour le jeu de test (utiliser celui de data_v2)
train_df <- data_v2$train_data
test_df <- data_v2$test_data

cat(sprintf("Jeu d'entraînement : %d matchs\n", nrow(train_df)))
cat(sprintf("Jeu de test : %d matchs\n", nrow(test_df)))

# =============================================================================
# GRAPHIQUE 1 : Distribution des issues de match
# =============================================================================
cat("\n=== Graphique 1 : Distribution des issues ===\n")

g1 <- ggplot(df_complete, aes(x = outcome, fill = outcome)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 5) +
  scale_fill_manual(
    values = c("H" = "#2ecc71", "D" = "#95a5a6", "A" = "#e74c3c"),
    labels = c("H" = "Victoire domicile", "D" = "Nul", "A" = "Victoire extérieur")
  ) +
  labs(
    title = "Répartition des résultats de matchs",
    subtitle = "Victoire domicile / Nul / Victoire extérieur — Jeu de données d'entraînement",
    x = "Issue du match",
    y = "Nombre de matchs",
    fill = "Résultat"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("figures/01_distribution_outcomes.png", g1, width = 10, height = 6, dpi = 300)
cat("✓ Graphique sauvegardé : figures/01_distribution_outcomes.png\n")

# =============================================================================
# GRAPHIQUE 2 : Évolution du taux de victoire à domicile dans le temps
# =============================================================================
cat("\n=== Graphique 2 : Évolution du taux de victoire domicile ===\n")

# Reconstruire df_complete avec match_date pour l'analyse temporelle
df_with_dates <- data_v2$elo_history %>%
  left_join(
    df %>% select(match_id, home_goals, away_goals, outcome),
    by = "match_id"
  ) %>%
  filter(!is.na(outcome))

home_win_evolution <- df_with_dates %>%
  mutate(
    year = year(match_date),
    is_home_win = ifelse(outcome == "H", 1, 0)
  ) %>%
  group_by(year) %>%
  summarise(
    home_win_rate = mean(is_home_win, na.rm = TRUE) * 100,
    n_matches = n(),
    .groups = "drop"
  )

g2 <- ggplot(home_win_evolution, aes(x = year, y = home_win_rate)) +
  geom_line(color = "#3498db", size = 1.2) +
  geom_point(color = "#3498db", size = 3) +
  geom_text(aes(label = sprintf("%.1f%%", home_win_rate)), vjust = -1, size = 3.5) +
  labs(
    title = "Évolution du taux de victoires à domicile",
    subtitle = "Indique si l'avantage domicile est stable dans le temps",
    x = "Année",
    y = "Pourcentage de victoires domicile (%)"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12)
  )

ggsave("figures/02_home_win_rate_over_time.png", g2, width = 10, height = 6, dpi = 300)
cat("✓ Graphique sauvegardé : figures/02_home_win_rate_over_time.png\n")

# =============================================================================
# GRAPHIQUE 3 : Courbe Elo dans le temps pour deux équipes
# =============================================================================
cat("\n=== Graphique 3 : Évolution Elo PSG vs Nantes ===\n")

# Paramètres : deux équipes à comparer
team_a <- "Paris Saint-Germain"
team_b <- "FC Nantes"

# Alternative si les noms ne matchent pas
available_teams <- unique(c(data_v2$elo_history$home_team, data_v2$elo_history$away_team))
if (!team_a %in% available_teams) {
  team_a <- available_teams[grepl("Paris", available_teams, ignore.case = TRUE)][1]
}
if (!team_b %in% available_teams) {
  team_b <- available_teams[grepl("Nantes", available_teams, ignore.case = TRUE)][1]
}
if (is.na(team_a) || length(team_a) == 0) team_a <- available_teams[1]
if (is.na(team_b) || length(team_b) == 0) team_b <- available_teams[min(2, length(available_teams))]

# Construire l'historique Elo pour chaque équipe
elo_team_a <- data_v2$elo_history %>%
  filter(home_team == team_a) %>%
  select(match_date, team = home_team, elo = home_elo_before) %>%
  bind_rows(
    data_v2$elo_history %>%
      filter(away_team == team_a) %>%
      select(match_date, team = away_team, elo = away_elo_before)
  ) %>%
  arrange(match_date) %>%
  mutate(team = team_a)

elo_team_b <- data_v2$elo_history %>%
  filter(home_team == team_b) %>%
  select(match_date, team = home_team, elo = home_elo_before) %>%
  bind_rows(
    data_v2$elo_history %>%
      filter(away_team == team_b) %>%
      select(match_date, team = away_team, elo = away_elo_before)
  ) %>%
  arrange(match_date) %>%
  mutate(team = team_b)

elo_both <- bind_rows(elo_team_a, elo_team_b)

g3 <- ggplot(elo_both, aes(x = match_date, y = elo, color = team)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.6) +
  scale_color_manual(values = c("#e74c3c", "#3498db")) +
  labs(
    title = sprintf("Évolution du rating Elo – %s vs %s", team_a, team_b),
    subtitle = "Illustre la différence de niveau moyen entre clubs",
    x = "Date",
    y = "Rating Elo",
    color = "Équipe"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("figures/03_elo_psg_vs_nantes.png", g3, width = 10, height = 6, dpi = 300)
cat("✓ Graphique sauvegardé : figures/03_elo_psg_vs_nantes.png\n")

# =============================================================================
# GRAPHIQUE 4 : Distribution des prédictions par classe (probabilités)
# =============================================================================
cat("\n=== Graphique 4 : Distribution des probabilités prédites ===\n")

# Obtenir les probabilités sur le jeu de test
test_probs <- predict(model, newdata = test_df, type = "prob")

# Transformer en format long
probs_df <- data.frame(
  prob_H = test_probs$H,
  prob_D = test_probs$D,
  prob_A = test_probs$A,
  actual = test_df$outcome
) %>%
  pivot_longer(
    cols = starts_with("prob_"),
    names_to = "classe",
    values_to = "probabilite"
  ) %>%
  mutate(classe = gsub("prob_", "", classe))

g4 <- ggplot(probs_df, aes(x = probabilite, fill = classe)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_wrap(~classe, labeller = labeller(
    classe = c("H" = "Victoire domicile (H)", 
               "D" = "Nul (D)", 
               "A" = "Victoire extérieur (A)")
  )) +
  scale_fill_manual(
    values = c("H" = "#2ecc71", "D" = "#95a5a6", "A" = "#e74c3c")
  ) +
  labs(
    title = "Distribution des probabilités prédites par classe",
    subtitle = "Montre comment le modèle distribue les confiances par type de résultat",
    x = "Probabilité prédite",
    y = "Nombre de matchs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )

ggsave("figures/04_probability_distribution.png", g4, width = 10, height = 6, dpi = 300)
cat("✓ Graphique sauvegardé : figures/04_probability_distribution.png\n")

# =============================================================================
# GRAPHIQUE 5 : Matrice de confusion (jeu de test)
# =============================================================================
cat("\n=== Graphique 5 : Matrice de confusion ===\n")

# Prédictions sur le jeu de test
test_predictions <- predict(model, newdata = test_df)

# Matrice de confusion
conf_matrix <- table(Vérité = test_df$outcome, Prédiction = test_predictions)

# Convertir en data frame pour ggplot
conf_df <- as.data.frame(conf_matrix) %>%
  rename(verite = Vérité, prediction = Prédiction, count = Freq)

# Calculer les pourcentages
conf_df <- conf_df %>%
  group_by(verite) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  ) %>%
  ungroup()

g5 <- ggplot(conf_df, aes(x = prediction, y = verite, fill = count)) +
  geom_tile(color = "white", size = 1) +
  geom_text(
    aes(label = sprintf("%d\n(%.1f%%)", count, percentage)),
    color = "white",
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
  labs(
    title = "Matrice de confusion – Jeu de test",
    subtitle = "Qualité des prédictions par type de résultat",
    x = "Prédiction",
    y = "Vérité",
    fill = "Nombre de matchs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )

ggsave("figures/05_confusion_matrix.png", g5, width = 10, height = 6, dpi = 300)
cat("✓ Graphique sauvegardé : figures/05_confusion_matrix.png\n")

# =============================================================================
# GRAPHIQUE 6 : Courbe de calibration pour victoire domicile
# =============================================================================
cat("\n=== Graphique 6 : Courbe de calibration ===\n")

# Obtenir les probabilités prédites
test_probs <- predict(model, newdata = test_df, type = "prob")

# Créer un data frame avec les probabilités et la vérité
calibration_df <- data.frame(
  prob_home = test_probs$H,
  actual_home = ifelse(test_df$outcome == "H", 1, 0)
)

# Binning des probabilités (déciles)
calibration_df <- calibration_df %>%
  mutate(
    bin = cut(prob_home, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
  )

# Calculer la probabilité moyenne et la fréquence observée par bin
calibration_summary <- calibration_df %>%
  group_by(bin) %>%
  summarise(
    mean_predicted = mean(prob_home, na.rm = TRUE),
    observed_freq = mean(actual_home, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n > 0)

g6 <- ggplot(calibration_summary, aes(x = mean_predicted, y = observed_freq)) +
  geom_point(aes(size = n), color = "#3498db", alpha = 0.7) +
  geom_line(color = "#3498db", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#e74c3c", size = 1) +
  scale_size_continuous(name = "Nombre de matchs") +
  labs(
    title = "Courbe de calibration – Victoire domicile",
    subtitle = "Compare probabilités prédites et fréquences observées (ligne rouge = calibration parfaite)",
    x = "Probabilité moyenne prédite",
    y = "Fréquence observée"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("figures/06_calibration_home_win.png", g6, width = 10, height = 6, dpi = 300)
cat("✓ Graphique sauvegardé : figures/06_calibration_home_win.png\n")

# =============================================================================
# GRAPHIQUE 7 : Exemples de prédictions vs résultats réels
# =============================================================================
cat("\n=== Graphique 7 : Exemples de prédictions ===\n")

# Prendre un échantillon de 10 matchs récents (au lieu de 20) pour tenir en 10x6
n_examples <- min(10, nrow(test_df))

# Utiliser directement les dernières lignes de elo_history qui correspondent au test set
# et créer des labels génériques
example_df <- test_df %>%
  tail(n_examples) %>%
  mutate(
    match_label = sprintf("Match %d", row_number())
  )

# Obtenir les probabilités
example_probs <- predict(model, newdata = example_df, type = "prob")
example_preds <- predict(model, newdata = example_df)

# Créer un data frame long pour ggplot
example_long <- data.frame(
  match_id = 1:n_examples,
  match_label = example_df$match_label,
  actual = as.character(example_df$outcome),
  predicted = as.character(example_preds),
  prob_H = example_probs$H,
  prob_D = example_probs$D,
  prob_A = example_probs$A
) %>%
  pivot_longer(
    cols = starts_with("prob_"),
    names_to = "outcome",
    values_to = "probability"
  ) %>%
  mutate(
    outcome = gsub("prob_", "", outcome),
    is_actual = outcome == actual,
    match_label = factor(match_label, levels = unique(match_label))
  )

g7 <- ggplot(example_long, aes(x = probability, y = match_label, color = outcome)) +
  geom_point(aes(shape = is_actual, size = is_actual), alpha = 0.8) +
  geom_line(aes(group = match_label), color = "gray70", alpha = 0.5) +
  scale_color_manual(
    values = c("H" = "#2ecc71", "D" = "#95a5a6", "A" = "#e74c3c"),
    labels = c("H" = "Victoire domicile", "D" = "Nul", "A" = "Victoire extérieur")
  ) +
  scale_shape_manual(
    values = c("TRUE" = 17, "FALSE" = 16),
    labels = c("TRUE" = "Résultat réel", "FALSE" = "Autre"),
    name = ""
  ) +
  scale_size_manual(
    values = c("TRUE" = 4, "FALSE" = 2),
    guide = "none"
  ) +
  labs(
    title = "Exemples de matchs : prédiction du modèle vs résultat réel",
    subtitle = "Permet de visualiser des cas où le modèle a vu juste ou s'est trompé (triangle = résultat réel)",
    x = "Probabilité prédite",
    y = "Match",
    color = "Issue"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

ggsave("figures/07_example_predictions.png", g7, width = 10, height = 6, dpi = 300)
cat("✓ Graphique sauvegardé : figures/07_example_predictions.png\n")

# =============================================================================
# FIN DU SCRIPT
# =============================================================================
cat("\n=== GÉNÉRATION TERMINÉE ===\n")
cat("Tous les graphiques ont été sauvegardés dans le dossier 'figures/'\n")
cat("Liste des fichiers générés :\n")
cat("  1. figures/01_distribution_outcomes.png\n")
cat("  2. figures/02_home_win_rate_over_time.png\n")
cat("  3. figures/03_elo_psg_vs_nantes.png\n")
cat("  4. figures/04_probability_distribution.png\n")
cat("  5. figures/05_confusion_matrix.png\n")
cat("  6. figures/06_calibration_home_win.png\n")
cat("  7. figures/07_example_predictions.png\n")
