#!/opt/homebrew/bin/Rscript
# ============================================================================
# predict_outcome.R
# Prédiction des résultats avec gestion des Elo ratings
# ============================================================================
# OBJECTIF : Générer prédictions de match à partir noms équipes + modèle
# FUNCTION : predict_outcome_from_teams()
#   INPUT  : home_team, away_team, model, elo_map
#   OUTPUT : list(class="H|D|A", proba=list(H,D,A), info=list(elos))
# USAGE    : source("R/predict_outcome.R"); predict_outcome_from_teams("PSG", "Nice")
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source("/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R/R/elo_utils.R")

# Charger la base Elo si disponible, sinon fallback interne
if (!exists("get_initial_elo")) {
  try({
    source("/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R/R/team_base_elo.R")
  }, silent = TRUE)
}
if (!exists("get_initial_elo")) {
  get_initial_elo <- function(team_name, default = 1500) default
}

# ============================================================================
# FONCTION 1 : Prédire pour un match spécifique
# ============================================================================
#' Prédit le résultat d'un match basé sur les Elo et forme récente
#'
#' @param home_team Nom de l'équipe à domicile
#' @param away_team Nom de l'équipe à l'extérieur
#' @param model Modèle caret entraîné
#' @param elo_map Map des Elo actuels (list ou data.frame avec colonnes team, elo)
#' @param default_elo Elo par défaut pour équipe inconnue (1500)
#' @param all_matches Optionnel: data.frame de tous les matchs pour calculer recent_form
#'
#' @return list(class, proba)
#'   - class: "H", "D" ou "A"
#'   - proba: liste avec H, D, A probabilities
#'
predict_outcome_from_teams <- function(
  home_team,
  away_team,
  model,
  elo_map,
  default_elo = 1500,
  all_matches = NULL
) {
  
  # Récupérer les Elo
  if (is.list(elo_map)) {
    home_elo <- if (!is.null(elo_map[[home_team]])) elo_map[[home_team]] else get_initial_elo(home_team, default_elo)
    away_elo <- if (!is.null(elo_map[[away_team]])) elo_map[[away_team]] else get_initial_elo(away_team, default_elo)
  } else {
    # Cas où c'est un data.frame
    home_elo <- elo_map %>%
      filter(team == home_team) %>%
      pull(elo) %>%
      { if (length(.) == 0 || is.na(.[1])) get_initial_elo(home_team, default_elo) else .[1] }
    away_elo <- elo_map %>%
      filter(team == away_team) %>%
      pull(elo) %>%
      { if (length(.) == 0 || is.na(.[1])) get_initial_elo(away_team, default_elo) else .[1] }
  }
  
  # Si équipes inconnues, assumer une force similaire
  if (home_team %in% names(elo_map) || (is.data.frame(elo_map) && home_team %in% elo_map$team)) {
    home_known <- TRUE
  } else {
    home_known <- FALSE
  }
  
  if (away_team %in% names(elo_map) || (is.data.frame(elo_map) && away_team %in% elo_map$team)) {
    away_known <- TRUE
  } else {
    away_known <- FALSE
  }
  
  # Calculer recent_form (simplifié)
  # Si pas d'historique, assumer une forme moyenne (1.5 points par match)
  home_recent_form <- 1.5
  away_recent_form <- 1.5
  
  if (!is.null(all_matches)) {
    # TODO: Calculer recent_form à partir de all_matches
    # Pour l'instant, utiliser la valeur par défaut
  }
  
  # Construire le vecteur de features
  features_df <- tibble(
    elo_diff = home_elo - away_elo,
    home_elo_before = home_elo,
    away_elo_before = away_elo,
    home_recent_form = home_recent_form,
    away_recent_form = away_recent_form,
    recent_form_diff = home_recent_form - away_recent_form,
    home_advantage = 1
  )
  
  # Prédiction
  tryCatch({
    proba_df <- predict(model, features_df, type = "prob")
    class <- colnames(proba_df)[apply(proba_df, 1, which.max)]
    
    # Clamper les probas pour éviter les 0 ou 1
    proba_clamped <- pmax(pmin(proba_df[1, ], 1 - 1e-10), 1e-10)
    
    # Normaliser pour s'assurer que la somme = 1
    proba_normalized <- proba_clamped / sum(proba_clamped)
    
    result <- list(
      class = class,
      proba = list(
        H = as.numeric(proba_normalized["H"]),
        D = as.numeric(proba_normalized["D"]),
        A = as.numeric(proba_normalized["A"])
      ),
      info = list(
        home_team = home_team,
        away_team = away_team,
        home_elo = home_elo,
        away_elo = away_elo,
        home_known = home_known,
        away_known = away_known
      )
    )
    
    return(result)
    
  }, error = function(e) {
    stop(paste("Erreur de prédiction:", as.character(e)))
  })
}

# ============================================================================
# FONCTION 2 : Charger modèle et Elo (wrapper pratique)
# ============================================================================
#' Charge le modèle et les Elo depuis les fichiers RDS
#'
#' @param project_root Racine du projet
#' @param model_version "v2" ou autre (défaut "v2")
#'
#' @return list(model, elo_map, elo_params)
#'
load_model_and_elo <- function(project_root, model_version = "v2") {
  
  model_path <- file.path(project_root, paste0("models/model_", model_version, ".rds"))
  elo_map_path <- file.path(project_root, "models/elo_map.rds")
  elo_params_path <- file.path(project_root, "models/elo_params.rds")
  
  if (!file.exists(model_path)) {
    stop(paste("Modèle non trouvé:", model_path))
  }
  
  model <- readRDS(model_path)
  elo_map <- readRDS(elo_map_path)
  elo_params <- readRDS(elo_params_path)
  
  return(list(
    model = model,
    elo_map = elo_map,
    elo_params = elo_params
  ))
}

# ============================================================================
# FONCTION 3 : Prédiction à partir d'un data.frame de matchs
# (pour validation en batch)
# ============================================================================
#' Prédit les résultats d'une liste de matchs
#'
#' @param matches data.frame avec colonnes home_team, away_team
#' @param model Modèle caret
#' @param elo_map Map des Elo
#'
#' @return data.frame avec colonnes home_team, away_team, pred_class, prob_H, prob_D, prob_A
#'
predict_batch <- function(matches, model, elo_map) {
  
  results <- list()
  
  for (i in seq_len(nrow(matches))) {
    match <- matches[i, ]
    
    pred <- predict_outcome_from_teams(
      home_team = match$home_team,
      away_team = match$away_team,
      model = model,
      elo_map = elo_map
    )
    
    results[[i]] <- tibble(
      home_team = match$home_team,
      away_team = match$away_team,
      pred_class = pred$class,
      prob_H = pred$proba$H,
      prob_D = pred$proba$D,
      prob_A = pred$proba$A
    )
  }
  
  return(bind_rows(results))
}
