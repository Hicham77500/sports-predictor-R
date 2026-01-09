#!/opt/homebrew/bin/Rscript
# ============================================================================
# compute_team_stats.R
# Calcule les stats historiques pour chaque équipe
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

compute_team_stats <- function(data_path) {
  cat("\n=== CALCUL DES STATS ÉQUIPES ===\n")
  
  # Charger les données
  cat("Chargement des données...\n")
  matches <- read_csv(data_path, show_col_types = FALSE)
  
  # ============================================================================
  # STATS COMME ÉQUIPE À DOMICILE
  # ============================================================================
  home_stats <- matches %>%
    group_by(home_team) %>%
    summarise(
      home_matches = n(),
      home_wins = sum(outcome == "H"),
      home_draws = sum(outcome == "D"),
      home_losses = sum(outcome == "A"),
      home_goals_for = sum(home_goals),
      home_goals_against = sum(away_goals),
      home_win_rate = sum(outcome == "H") / n(),
      home_draw_rate = sum(outcome == "D") / n(),
      home_loss_rate = sum(outcome == "A") / n(),
      home_avg_goals_for = mean(home_goals),
      home_avg_goals_against = mean(away_goals),
      .groups = "drop"
    )
  
  # ============================================================================
  # STATS COMME ÉQUIPE À L'EXTÉRIEUR
  # ============================================================================
  away_stats <- matches %>%
    group_by(away_team) %>%
    summarise(
      away_matches = n(),
      away_wins = sum(outcome == "A"),
      away_draws = sum(outcome == "D"),
      away_losses = sum(outcome == "H"),
      away_goals_for = sum(away_goals),
      away_goals_against = sum(home_goals),
      away_win_rate = sum(outcome == "A") / n(),
      away_draw_rate = sum(outcome == "D") / n(),
      away_loss_rate = sum(outcome == "H") / n(),
      away_avg_goals_for = mean(away_goals),
      away_avg_goals_against = mean(home_goals),
      .groups = "drop"
    )
  
  # ============================================================================
  # STATS GLOBALES
  # ============================================================================
  global_stats <- matches %>%
    bind_rows(
      matches %>% mutate(team = away_team, goals = away_goals, goals_against = home_goals, result = case_when(outcome == "A" ~ "W", outcome == "D" ~ "D", outcome == "H" ~ "L"))
    ) %>%
    mutate(
      team = coalesce(home_team, team),
      goals = coalesce(home_goals, goals),
      goals_against = coalesce(away_goals, goals_against),
      result = coalesce(outcome, result)
    ) %>%
    filter(!is.na(team)) %>%
    group_by(team) %>%
    summarise(
      total_matches = n() / 2,  # Compte chaque match une fois (home + away)
      total_wins = sum(result == "W" | result == "H"),
      total_draws = sum(result == "D"),
      total_losses = sum(result == "L" | result == "A"),
      total_goals_for = sum(goals, na.rm = TRUE) / 2,
      total_goals_against = sum(goals_against, na.rm = TRUE) / 2,
      global_win_rate = sum(result == "W" | result == "H") / (n() / 2),
      global_draw_rate = sum(result == "D") / (n() / 2),
      global_loss_rate = sum(result == "L" | result == "A") / (n() / 2),
      global_avg_goals = mean(goals, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merger les stats
  team_stats <- home_stats %>%
    full_join(away_stats, by = c("home_team" = "away_team")) %>%
    rename(team = home_team) %>%
    full_join(global_stats, by = c("team" = "team"))
  
  # Remplir les NAs par 0
  team_stats <- team_stats %>%
    replace(is.na(.), 0)
  
  cat("✓ Stats calculées pour", nrow(team_stats), "équipes\n")
  
  return(team_stats)
}

# Exécution principale
if (sys.nframe() == 0) {
  project_root <- "/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R"
  data_path <- file.path(project_root, "data/raw/matches_augmented.csv")
  
  team_stats <- compute_team_stats(data_path)
  
  # Sauvegarder
  output_path <- file.path(project_root, "data/team_stats.csv")
  write_csv(team_stats, output_path)
  cat("✓ Sauvegardé:", output_path, "\n\n")
  
  # Afficher aperçu
  cat("Aperçu des stats:\n")
  print(head(team_stats[, 1:6]))
}
