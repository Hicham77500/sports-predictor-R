library(dplyr)
library(readr)

#' Générer des données de matches réalistes pour augmenter le dataset
#' Basé sur les statistiques observées dans les données réelles
generate_synthetic_matches <- function(n = 500, seed = 42) {
  set.seed(seed)
  
  cat("Génération de", n, "matchs synthétiques...\n")
  
  # Liste COMPLÈTE des clubs européens et mondiaux
  teams <- c(
    # Premier League
    "Manchester United", "Liverpool", "Arsenal", "Chelsea", "Manchester City",
    "Tottenham", "Newcastle", "Aston Villa", "Brighton", "Fulham",
    "Wolverhampton", "West Ham", "Everton", "Leicester", "Leeds",
    # La Liga
    "Real Madrid", "Barcelona", "Atletico Madrid", "Sevilla", "Real Sociedad",
    "Valencia", "Real Betis", "Villarreal", "Girona", "Real Valladolid",
    # Serie A
    "Juventus", "Inter Milan", "AC Milan", "AS Roma", "Napoli",
    "Lazio", "Fiorentina", "Atalanta", "Torino", "Sampdoria",
    # Bundesliga
    "Bayern Munich", "Borussia Dortmund", "RB Leipzig", "Leverkusen", "Frankfurt",
    "Stuttgart", "Hoffenheim", "Freiburg", "Cologne", "Werder Bremen",
    # Ligue 1
    "PSG", "Marseille", "Monaco", "Lyon", "Lens",
    "Lille", "Nantes", "Rennes", "Nice", "Toulouse",
    # Champions League teams from other leagues
    "Ajax", "PSV Eindhoven", "Benfica", "Sporting CP", "Porto",
    "Celtic", "Rangers", "Galatasaray", "Fenerbahce", "Besiktas",
    "Shakhtar Donetsk", "Dynamo Kiev", "Red Bull Salzburg", "Club Brugge", "Copenhagen",
    # International teams
    "France", "Germany", "England", "Spain", "Italy", "Portugal",
    "Argentina", "Brazil", "Uruguay", "Chile", "Colombia", "Mexico",
    "Netherlands", "Belgium", "Switzerland", "Austria", "Poland",
    "Japan", "South Korea", "Australia", "United States", "Canada"
  )
  
  # Distribution observée des buts (réaliste)
  goals_dist <- function() {
    u <- runif(1)
    if (u < 0.35) return(0)
    if (u < 0.65) return(1)
    if (u < 0.85) return(2)
    if (u < 0.95) return(3)
    return(sample(3:5, 1))
  }
  
  # Créer les matches
  matches <- tibble(
    match_id = 600000 + seq_len(n),
    match_date = seq(from = as.Date("2024-01-01"), by = "days", length.out = n),
    status = "FINISHED",
    competition = sample(c("Premier League", "La Liga", "Serie A", "Bundesliga", "Ligue 1",
                          "Champions League", "International", "Cup"), n, replace = TRUE),
    home_team = sample(teams, n, replace = TRUE),
    away_team = sample(teams, n, replace = TRUE),
    home_goals = sapply(1:n, function(x) goals_dist()),
    away_goals = sapply(1:n, function(x) goals_dist())
  )
  
  # Filtrer les doublons (même équipe à domicile et à l'extérieur)
  matches <- matches %>%
    filter(home_team != away_team)
  
  cat("✓", nrow(matches), "matchs générés avec", length(unique(c(matches$home_team, matches$away_team))), "équipes uniques\n")
  matches
}

#' Fusionner les données réelles avec les données synthétiques
augment_dataset <- function(
  real_data_path = "data/raw/matches.csv",
  output_path = "data/raw/matches_augmented.csv",
  n_synthetic = 500
) {
  cat("=== AUGMENTATION DU DATASET ===\n\n")
  
  # Charger les données réelles
  cat("Chargement des données réelles...\n")
  real_data <- read_csv(real_data_path, show_col_types = FALSE)
  cat("✓", nrow(real_data), "matchs réels\n\n")
  
  # Générer données synthétiques
  synthetic_data <- generate_synthetic_matches(n = n_synthetic)
  cat("\n")
  
  # Fusionner
  combined <- bind_rows(real_data, synthetic_data)
  
  # Supprimer les doublons
  combined <- combined %>%
    distinct(home_team, away_team, match_date, .keep_all = TRUE)
  
  cat("Sauvegarde du dataset augmenté...\n")
  write_csv(combined, output_path)
  
  cat("✓ Succès:\n")
  cat("  - Données réelles: ", nrow(real_data), "\n")
  cat("  - Données synthétiques: ", nrow(synthetic_data), "\n")
  cat("  - Total (après dédoublonnage): ", nrow(combined), "\n")
  cat("  - Fichier: ", output_path, "\n\n")
  
  invisible(combined)
}

if (!interactive()) {
  augment_dataset(n_synthetic = 800)
}
