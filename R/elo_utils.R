#!/opt/homebrew/bin/Rscript
# ============================================================================
# elo_utils.R
# Calcul des ratings Elo et feature engineering pour prédiction de matchs
# ============================================================================
# OBJECTIF : Préparer features complètes (Elo, recent_form) pour entraînement
# FONCTIONS PRINCIPALES :
#   1. compute_elo_ratings() : Elo incrémental par match
#   2. compute_recent_form() : Points récents (fenêtre glissante)
#   3. prepare_training_data_v2() : Fusion Elo + features + split temporel
# OUTPUT   : train_data, test_data, elo_history, elo_current, elo_map
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# Charger éventuellement la table d'Elo de référence si disponible
if (!exists("get_initial_elo")) {
  try({
    source("R/team_base_elo.R")
  }, silent = TRUE)
}
if (!exists("get_initial_elo")) {
  get_initial_elo <- function(team_name, default = 1500) default
}

# ============================================================================
# FONCTION 1 : Calculer les ratings Elo
# ============================================================================
#' Calcule les ratings Elo pour toutes les équipes
#'
#' @param matches data.frame avec colonnes: match_id, match_date, home_team, away_team, 
#'                 home_goals, away_goals (en ordre chronologique)
#' @param initial_elo Elo initial pour nouvelle équipe (défaut 1500)
#' @param k_factor Facteur d'ajustement (défaut 32)
#' @param home_advantage Points d'avantage terrain à ajouter avant calcul (défaut 65)
#'
#' @return list(elo_history, elo_current)
#'   - elo_history : data.frame avec elo après chaque match
#'   - elo_current : data.frame avec dernier elo de chaque équipe
#'
compute_elo_ratings <- function(
  matches,
  initial_elo = 1500,
  k_factor = 32,
  home_advantage = 65
) {
  
  cat("\n=== CALCUL DES ELO RATINGS ===\n")
  
  # Tri par date (CRUCIAL)
  matches <- matches %>%
    arrange(match_date, match_id) %>%
    mutate(match_date = as.Date(match_date))
  
  # Initialiser les Elo courants
  elo_current <- list()
  
  # Résultat Elo (pour tracking)
  elo_history <- list()
  
  # Itérer sur chaque match
  for (i in seq_len(nrow(matches))) {
    match <- matches[i, ]
    
    home_team <- match$home_team
    away_team <- match$away_team
    home_goals <- match$home_goals
    away_goals <- match$away_goals
    match_id <- match$match_id
    match_date <- match$match_date
    
    # Elo avant le match (fallback vers table de référence si inconnue)
    home_elo_before <- if (!is.null(elo_current[[home_team]])) elo_current[[home_team]] else get_initial_elo(home_team)
    away_elo_before <- if (!is.null(elo_current[[away_team]])) elo_current[[away_team]] else get_initial_elo(away_team)
    
    # Déterminer le résultat
    if (home_goals > away_goals) {
      home_result <- 1  # Victoire domicile
      away_result <- 0
    } else if (home_goals < away_goals) {
      home_result <- 0
      away_result <- 1  # Victoire extérieur
    } else {
      home_result <- 0.5  # Nul
      away_result <- 0.5
    }
    
    # Calculer Elo attendu
    elo_diff <- home_elo_before + home_advantage - away_elo_before
    expected_home <- 1 / (1 + 10^(-elo_diff / 400))
    expected_away <- 1 - expected_home
    
    # Mise à jour Elo
    home_elo_after <- home_elo_before + k_factor * (home_result - expected_home)
    away_elo_after <- away_elo_before + k_factor * (away_result - expected_away)
    
    # Stocker les Elo courants
    elo_current[[home_team]] <- home_elo_after
    elo_current[[away_team]] <- away_elo_after
    
    # Historique
    elo_history[[i]] <- tibble(
      match_id = match_id,
      match_date = match_date,
      home_team = home_team,
      away_team = away_team,
      home_elo_before = home_elo_before,
      away_elo_before = away_elo_before,
      home_elo_after = home_elo_after,
      away_elo_after = away_elo_after,
      home_result = home_result,
      away_result = away_result,
      expected_home = expected_home,
      expected_away = expected_away
    )
  }
  
  # Combiner l'historique
  elo_df <- bind_rows(elo_history)
  
  # Dernier Elo pour chaque équipe
  elo_current_df <- tibble(
    team = names(elo_current),
    elo = unlist(elo_current)
  )
  
  cat("✓ Elo calculés pour", nrow(elo_current_df), "équipes\n")
  
  return(list(
    elo_history = elo_df,
    elo_current = elo_current_df,
    elo_map = elo_current
  ))
}

# ============================================================================
# FONCTION 2 : Recent Form (points des N derniers matchs)
# ============================================================================
#' Calcule la forme récente pour chaque équipe
#'
#' @param matches data.frame avec colonnes: match_date, home_team, away_team, 
#'                 home_goals, away_goals
#' @param window Nombre de jours regardés en arrière (défaut 30)
#'
#' @return data.frame avec colonnes: team, match_date, recent_points
#'
compute_recent_form <- function(matches, window = 30) {
  
  matches <- matches %>%
    mutate(match_date = as.Date(match_date)) %>%
    arrange(match_date)
  
  # Pour chaque équipe à domicile
  home_form <- matches %>%
    mutate(
      result = case_when(
        home_goals > away_goals ~ 3,
        home_goals == away_goals ~ 1,
        TRUE ~ 0
      ),
      team = home_team,
      as_home = TRUE
    ) %>%
    select(team, match_date, result, as_home)
  
  # Pour chaque équipe à l'extérieur
  away_form <- matches %>%
    mutate(
      result = case_when(
        away_goals > home_goals ~ 3,
        away_goals == home_goals ~ 1,
        TRUE ~ 0
      ),
      team = away_team,
      as_home = FALSE
    ) %>%
    select(team, match_date, result, as_home)
  
  # Combiner
  form_all <- bind_rows(home_form, away_form) %>%
    arrange(team, match_date)
  
  # Calculer la forme récente (rolling sum des 5 derniers matchs)
  form_recent <- form_all %>%
    group_by(team) %>%
    arrange(team, match_date) %>%
    mutate(
      # Rolling sum manuel (5 derniers matchs)
      recent_points = sapply(
        seq_along(result),
        function(i) sum(result[max(1, i-4):i], na.rm = TRUE)
      ),
      recent_matches = sapply(
        seq_along(result),
        function(i) sum(!is.na(result[max(1, i-4):i]))
      ),
      recent_form = ifelse(recent_matches >= 1, recent_points / recent_matches, 1.5)
    ) %>%
    ungroup() %>%
    select(team, match_date, recent_form, recent_points, recent_matches)
  
  return(form_recent)
}

# ============================================================================
# FONCTION 3 : Préparer les données d'entraînement avec features améliorées
# ============================================================================
#' Prépare les données avec Elo, home advantage, recent form
#'
#' @param matches data.frame brut de matches (home_team, away_team, home_goals, away_goals, match_date)
#' @param split_date Date à partir de laquelle on commence le test set
#' @param elo_params Liste avec initial_elo, k_factor, home_advantage (optionnel)
#'
#' @return list(train_data, test_data, elo_history, elo_current, elo_map)
#'
prepare_training_data_v2 <- function(
  matches,
  split_date = NULL,
  elo_params = list(
    initial_elo = 1500,
    k_factor = 32,
    home_advantage = 65
  )
) {
  
  cat("\n=== PRÉPARATION DONNÉES AVEC ELO ===\n")
  
  matches <- matches %>%
    mutate(match_date = as.Date(match_date)) %>%
    arrange(match_date)
  
  # Créer outcome si absent
  if (!"outcome" %in% colnames(matches)) {
    matches <- matches %>%
      mutate(
        outcome = case_when(
          home_goals > away_goals ~ "H",
          home_goals < away_goals ~ "A",
          TRUE ~ "D"
        )
      )
  }
  
  # Déterminer split_date si absent (80-20)
  if (is.null(split_date)) {
    n_matches <- nrow(matches)
    split_idx <- ceiling(n_matches * 0.8)
    split_date <- matches$match_date[split_idx]
  }
  
  cat("✓ Split date:", as.character(split_date), "\n")
  
  # ÉTAPE 1 : Calculer les Elo ratings de manière INCRÉMENTALE
  # (important : chaque match met à jour les Elo)
  elo_result <- compute_elo_ratings(
    matches,
    initial_elo = elo_params$initial_elo,
    k_factor = elo_params$k_factor,
    home_advantage = elo_params$home_advantage
  )
  
  elo_history <- elo_result$elo_history
  elo_current <- elo_result$elo_current
  elo_map <- elo_result$elo_map
  
  # ÉTAPE 2 : Calculer la forme récente
  recent_form_all <- compute_recent_form(matches, window = 30)
  
  # ÉTAPE 3 : Fusionner les données avec les Elo et forma
  data_with_elo <- matches %>%
    left_join(
      elo_history %>% select(match_id, home_team, away_team, home_elo_before, away_elo_before),
      by = c("match_id", "home_team", "away_team")
    ) %>%
    left_join(
      recent_form_all %>% 
        rename(match_date = match_date, home_team = team, home_recent_form = recent_form) %>%
        select(home_team, match_date, home_recent_form),
      by = c("home_team", "match_date")
    ) %>%
    left_join(
      recent_form_all %>% 
        rename(match_date = match_date, away_team = team, away_recent_form = recent_form) %>%
        select(away_team, match_date, away_recent_form),
      by = c("away_team", "match_date")
    ) %>%
    mutate(
      # Elo difference
      elo_diff = home_elo_before - away_elo_before,
      
      # Home advantage (binaire)
      home_advantage = 1,
      
      # Remplacer les NA de recent_form par la moyenne (1.5 points par match en moyenne)
      home_recent_form = replace_na(home_recent_form, 1.5),
      away_recent_form = replace_na(away_recent_form, 1.5),
      
      # Récente forme diff
      recent_form_diff = home_recent_form - away_recent_form
    ) %>%
    filter(!is.na(elo_diff) & !is.na(outcome)) %>%
    select(
      match_id, match_date, home_team, away_team,
      outcome,
      elo_diff, home_elo_before, away_elo_before,
      home_recent_form, away_recent_form, recent_form_diff,
      home_advantage,
      home_goals, away_goals
    )
  
  cat("✓ Données préparées:", nrow(data_with_elo), "matchs\n")
  
  # ÉTAPE 4 : Split temporel
  train_data <- data_with_elo %>%
    filter(match_date < split_date) %>%
    select(outcome, elo_diff, home_elo_before, away_elo_before, 
           home_recent_form, away_recent_form, recent_form_diff, home_advantage)
  
  test_data <- data_with_elo %>%
    filter(match_date >= split_date) %>%
    select(outcome, elo_diff, home_elo_before, away_elo_before,
           home_recent_form, away_recent_form, recent_form_diff, home_advantage)
  
  # Convertir outcome en facteur
  train_data$outcome <- factor(train_data$outcome, levels = c("H", "D", "A"))
  test_data$outcome <- factor(test_data$outcome, levels = c("H", "D", "A"))
  
  cat("✓ Train set:", nrow(train_data), "matchs\n")
  cat("✓ Test set:", nrow(test_data), "matchs\n")
  
  return(list(
    train_data = train_data,
    test_data = test_data,
    elo_history = elo_history,
    elo_current = elo_current,
    elo_map = elo_map,
    split_date = split_date
  ))
}

# ============================================================================
# FONCTION 4 : Récupérer l'Elo actuel d'une équipe
# ============================================================================
get_team_elo <- function(team, elo_map, default_elo = 1500) {
  val <- elo_map[[team]]
  if (is.null(val)) {
    # Essayer base Elo si disponible, sinon default_elo
    val <- tryCatch(get_initial_elo(team), error = function(...) default_elo)
  }
  return(val)
}
