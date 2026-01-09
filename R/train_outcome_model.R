#!/opt/homebrew/bin/Rscript
# ============================================================================
# train_outcome_model.R
# Entraîne un modèle de prédiction de match avec Elo ratings
# ============================================================================
# OBJECTIF : Créer un modèle multinomial (H/D/A) basé sur features Elo
# INPUT    : data.frame avec colonnes outcome + features (elo_diff, recent_form, etc)
# OUTPUT   : Modèle caret sauvegardé dans models/model_v2.rds
# USAGE    : Rscript R/train_outcome_model.R
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(caret)
  library(nnet)
  library(readr)
})

source("/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R/R/elo_utils.R")

# ============================================================================
# FONCTION 1 : Entraîner le modèle
# ============================================================================
#' Entraîne un modèle de prédiction des résultats de matchs
#'
#' @param train_data data.frame avec colonnes outcome et features
#' @param model_type "nnet" (régression logistique multinomiale) ou "rf" (random forest)
#' @param hyperparams Liste des hyperparamètres
#'
#' @return Modèle caret entraîné
#'
train_outcome_model <- function(
  train_data,
  model_type = "nnet",
  hyperparams = list()
) {
  
  cat("\n=== ENTRAÎNEMENT DU MODÈLE ===\n")
  
  # Définir les paramètres par défaut
  if (model_type == "nnet") {
    default_params <- list(
      size = 5,
      decay = 0.01,
      maxit = 500,
      trace = FALSE
    )
  } else if (model_type == "rf") {
    default_params <- list(
      ntree = 200,
      nodesize = 5
    )
  }
  
  # Merger avec params fournis
  params <- modifyList(default_params, hyperparams)
  
  cat("✓ Modèle:", model_type, "\n")
  cat("✓ Features:", paste(setdiff(colnames(train_data), "outcome"), collapse = ", "), "\n")
  
  # Entraînement
  set.seed(42)
  
  if (model_type == "nnet") {
    # Multinomial logistic regression (nnet avec size=0 = logistique, ou size>0 = réseau)
    # On utilise size=5 pour une petite couche cachée
    
    model <- train(
      outcome ~ .,
      data = train_data,
      method = "nnet",
      tuneGrid = expand.grid(
        size = params$size,
        decay = params$decay
      ),
      trControl = trainControl(
        method = "cv",
        number = 5,
        classProbs = TRUE,
        summaryFunction = multiClassSummary,
        savePredictions = "final"
      ),
      trace = params$trace,
      MaxNWts = 10000,
      maxit = params$maxit,
      linout = FALSE
    )
    
  } else if (model_type == "rf") {
    
    model <- train(
      outcome ~ .,
      data = train_data,
      method = "rf",
      tuneGrid = data.frame(mtry = ncol(train_data) - 1),
      trControl = trainControl(
        method = "cv",
        number = 5,
        classProbs = TRUE,
        summaryFunction = multiClassSummary,
        savePredictions = "final"
      ),
      ntree = params$ntree,
      nodesize = params$nodesize,
      importance = TRUE
    )
    
  } else {
    stop("model_type doit être 'nnet' ou 'rf'")
  }
  
  cat("✓ Modèle entraîné\n")
  cat("✓ Best tune (CV):", capture.output(print(model$bestTune)), "\n")
  
  return(model)
}

# ============================================================================
# FONCTION 2 : Évaluer le modèle
# ============================================================================
#' Évalue un modèle sur un jeu de test
#'
#' @param model Modèle caret
#' @param test_data data.frame de test
#'
#' @return list avec accuracy, log_loss, brier_score, confusion_matrix
#'
evaluate_outcome_model <- function(model, test_data) {
  
  cat("\n=== ÉVALUATION ===\n")
  
  # Prédictions classes
  pred_class <- predict(model, test_data, type = "raw")
  
  # Prédictions probabilités
  pred_proba <- predict(model, test_data, type = "prob")
  
  # Accuracy
  accuracy <- mean(pred_class == test_data$outcome)
  cat("✓ Accuracy:", round(accuracy * 100, 2), "%\n")
  
  # Log-loss
  pred_proba_clamped <- pmax(pmin(pred_proba, 1 - 1e-10), 1e-10)
  log_loss <- -mean(
    ifelse(test_data$outcome == "H", log(pred_proba_clamped[, "H"]),
    ifelse(test_data$outcome == "D", log(pred_proba_clamped[, "D"]),
           log(pred_proba_clamped[, "A"])))
  )
  cat("✓ Log-Loss:", round(log_loss, 4), "\n")
  
  # Brier score
  outcome_numeric <- as.numeric(test_data$outcome) - 1
  outcome_matrix <- model.matrix(~ factor(test_data$outcome, levels = c("H", "D", "A")) - 1)
  brier_score <- mean((pred_proba - outcome_matrix)^2)
  cat("✓ Brier Score:", round(brier_score, 4), "\n")
  
  # Matrice de confusion
  conf_mat <- confusionMatrix(pred_class, test_data$outcome)
  cat("\n✓ Matrice de confusion:\n")
  print(conf_mat$table)
  
  # Feature importance (si RF)
  if (!is.null(model$finalModel$importance)) {
    cat("\n✓ Feature importance:\n")
    imp_df <- data.frame(
      feature = rownames(model$finalModel$importance),
      importance = model$finalModel$importance[, 1]
    ) %>%
      arrange(desc(importance))
    print(imp_df)
  }
  
  return(list(
    accuracy = accuracy,
    log_loss = log_loss,
    brier_score = brier_score,
    confusion_matrix = conf_mat$table,
    predictions = list(class = pred_class, proba = pred_proba)
  ))
}

# ============================================================================
# PIPELINE PRINCIPAL
# ============================================================================

project_root <- "/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R"

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║  PIPELINE ELO + MODÈLE DE PRÉDICTION V2                 ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n")

# 1. Charger les données
cat("\n--- ÉTAPE 1: Chargement des données ---\n")
data_path <- file.path(project_root, "data/raw/matches_augmented.csv")
matches_raw <- read_csv(data_path, show_col_types = FALSE)
cat("✓ Données chargées:", nrow(matches_raw), "matchs\n")

# 2. Préparer avec Elo
cat("\n--- ÉTAPE 2: Préparation avec Elo ---\n")
data_prepared <- prepare_training_data_v2(
  matches_raw,
  split_date = NULL,
  elo_params = list(
    initial_elo = 1500,
    k_factor = 32,
    home_advantage = 65
  )
)

train_data <- data_prepared$train_data
test_data <- data_prepared$test_data
elo_map <- data_prepared$elo_map

# 3. Entraîner le modèle
cat("\n--- ÉTAPE 3: Entraînement du modèle ---\n")
model <- train_outcome_model(
  train_data,
  model_type = "nnet",
  hyperparams = list(size = 10, decay = 0.001)
)

# 4. Évaluer
cat("\n--- ÉTAPE 4: Évaluation ---\n")
eval_result <- evaluate_outcome_model(model, test_data)

# 5. Sauvegarder
cat("\n--- ÉTAPE 5: Sauvegarde ---\n")

model_path <- file.path(project_root, "models/model_v2.rds")
saveRDS(model, model_path)
cat("✓ Modèle sauvegardé:", model_path, "\n")

elo_map_path <- file.path(project_root, "models/elo_map.rds")
saveRDS(elo_map, elo_map_path)
cat("✓ Elo map sauvegardée:", elo_map_path, "\n")

elo_params_path <- file.path(project_root, "models/elo_params.rds")
saveRDS(list(initial_elo = 1500, k_factor = 32, home_advantage = 65), elo_params_path)
cat("✓ Elo params sauvegardés:", elo_params_path, "\n")

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║  PIPELINE TERMINÉ ✓                                      ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n")
