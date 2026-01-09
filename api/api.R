#!/opt/homebrew/bin/Rscript
# ============================================================================
# api.R
# API Plumber pour prédictions Elo
# ============================================================================
# OBJECTIF : Serveur REST pour prédictions de match
# ENDPOINTS :
#   GET  /health                     : Status serveur
#   POST /predict_v2                 : home_team, away_team → proba H/D/A
#   GET  /elo/<team>                 : Elo rating équipe
# USAGE    : plumb("api/api.R") %>% pr_run(port=8001)
# ============================================================================

suppressPackageStartupMessages({
  library(plumber)
  library(dplyr)
  library(tibble)
  library(jsonlite)
})

project_root <- "/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R"

source(file.path(project_root, "R/elo_utils.R"))
source(file.path(project_root, "R/predict_outcome.R"))
try({ source(file.path(project_root, "R/team_base_elo.R")) }, silent = TRUE)
if (!exists("get_initial_elo")) {
  get_initial_elo <- function(team_name, default = 1500) default
}

# ============================================================================
# Charger le modèle et Elo une fois au démarrage
# ============================================================================
cat("\n✓ Chargement du modèle V2...\n")

loaded <- load_model_and_elo(project_root, "v2")
MODEL <- loaded$model
ELO_MAP <- loaded$elo_map
ELO_PARAMS <- loaded$elo_params

cat("✓ Modèle chargé :", class(MODEL), "\n")
cat("✓ Elo teams :", length(names(ELO_MAP)), "\n")

# ============================================================================
# ENDPOINT 1: Health check
# ============================================================================
#' @get /health
#' @serializer json
function() {
  list(
    status = "OK",
    model = "v2_elo",
    teams = length(names(ELO_MAP)),
    timestamp = Sys.time()
  )
}

# ============================================================================
# ENDPOINT 2: Prédiction basée sur les noms d'équipes
# ============================================================================
#' @post /predict_v2
#' @param home_team Nom de l'équipe à domicile
#' @param away_team Nom de l'équipe à l'extérieur
#' @serializer json
function(home_team, away_team) {
  
  tryCatch({
    # Validation
    if (is.null(home_team) || is.null(away_team)) {
      return(list(
        error = "Missing fields: home_team and away_team required",
        status = "error"
      ))
    }
    
    if (home_team == away_team) {
      return(list(
        error = "home_team and away_team must be different",
        status = "error"
      ))
    }
    
    # Prédiction
    pred <- predict_outcome_from_teams(
      home_team = home_team,
      away_team = away_team,
      model = MODEL,
      elo_map = ELO_MAP
    )
    
    # Réponse
    return(list(
      home_team = home_team,
      away_team = away_team,
      prediction = pred$class,
      probabilities = list(
        H = round(pred$proba$H, 4),
        D = round(pred$proba$D, 4),
        A = round(pred$proba$A, 4)
      ),
      info = list(
        home_elo = round(pred$info$home_elo, 1),
        away_elo = round(pred$info$away_elo, 1),
        elo_diff = round(pred$info$home_elo - pred$info$away_elo, 1),
        home_known = pred$info$home_known,
        away_known = pred$info$away_known
      ),
      status = "ok"
    ))
    
  }, error = function(e) {
    return(list(
      error = as.character(e),
      status = "error"
    ))
  })
}

# ============================================================================
# ENDPOINT 3: Obtenir les Elo actuels d'une équipe
# ============================================================================
#' @get /elo/<team>
#' @serializer json
function(team) {
  
  if (team %in% names(ELO_MAP)) {
    return(list(
      team = team,
      elo = round(ELO_MAP[[team]], 1),
      status = "ok"
    ))
  } else {
    fallback <- get_initial_elo(team, 1500)
    return(list(
      team = team,
      elo = round(fallback, 1),
      message = "Team not found, returning base Elo",
      status = "not_found"
    ))
  }
}

# ============================================================================
# ENDPOINT 4: Top teams
# ============================================================================
#' @get /top_teams/<n:int>
#' @serializer json
function(n = 10) {
  
  if (is.null(n) || n < 1) n <- 10
  if (n > length(names(ELO_MAP))) n <- length(names(ELO_MAP))
  
  top <- tibble(
    team = names(ELO_MAP),
    elo = unlist(ELO_MAP)
  ) %>%
    arrange(desc(elo)) %>%
    head(n) %>%
    mutate(elo = round(elo, 1))
  
  return(list(
    teams = list_parse(top),
    status = "ok"
  ))
}

# ============================================================================
# ENDPOINT 5: Statistiques générales
# ============================================================================
#' @get /stats
#' @serializer json
function() {
  
  elos <- unlist(ELO_MAP)
  
  return(list(
    total_teams = length(names(ELO_MAP)),
    elo_stats = list(
      mean = round(mean(elos), 1),
      median = round(median(elos), 1),
      min = round(min(elos), 1),
      max = round(max(elos), 1),
      sd = round(sd(elos), 1)
    ),
    model = "nnet (multinomial logistic)",
    features = c("elo_diff", "home_elo", "away_elo", "home_recent_form", 
                 "away_recent_form", "home_advantage"),
    accuracy_test = "61.54%",
    status = "ok"
  ))
}
