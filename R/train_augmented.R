library(dplyr)
library(caret)
library(readr)

source("R/fetch_data.R")
source("R/preprocess.R")
source("R/augment_data.R")

#' Pipeline complète : Augmenter données + Entraîner modèle
train_with_augmented_data <- function(
  real_data_path = "data/raw/matches.csv",
  model_path = "models/model.rds"
) {
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  PIPELINE: AUGMENTATION + ENTRAÎNEMENT MODÈLE            ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n\n")
  
  # ÉTAPE 1: Augmenter les données
  cat("ÉTAPE 1: Augmenter le dataset\n")
  cat("─────────────────────────────────\n")
  augmented_data <- augment_dataset(
    real_data_path = real_data_path,
    output_path = "data/raw/matches_augmented.csv",
    n_synthetic = 800
  )
  
  # ÉTAPE 2: Préparer pour l'entraînement
  cat("\nÉTAPE 2: Prétraitement des données\n")
  cat("───────────────────────────────────\n")
  prep <- prepare_training_data(augmented_data)
  df <- prep$data
  
  cat("Distribution des outcomes:\n")
  print(table(df$outcome))
  cat("\n")
  
  # ÉTAPE 3: Entraîner le modèle
  cat("ÉTAPE 3: Entraînement du modèle\n")
  cat("────────────────────────────────\n")
  
  # Feature principale: goal_diff
  df_model <- df %>%
    select(goal_diff, outcome) %>%
    mutate(outcome = factor(outcome, levels = c("H", "D", "A")))
  
  # Split 80/20
  set.seed(42)
  idx_train <- sample(1:nrow(df_model), size = floor(0.8 * nrow(df_model)))
  train_data <- df_model[idx_train, ]
  test_data  <- df_model[-idx_train, ]
  
  cat("Train set:", nrow(train_data), "matchs\n")
  cat("Test set:", nrow(test_data), "matchs\n\n")
  
  # Entraîner avec caret - 5-fold CV avec plus de données
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = multiClassSummary
  )
  
  cat("5-Fold Cross-Validation (cela peut prendre du temps)...\n")
  model <- caret::train(
    outcome ~ goal_diff,
    data = train_data,
    method = "rf",
    trControl = ctrl,
    ntree = 200,
    nodesize = 5,
    tuneGrid = data.frame(mtry = 1),
    verboseIter = FALSE
  )
  
  # ÉTAPE 4: Évaluer
  cat("\nÉTAPE 4: Évaluation\n")
  cat("──────────────────\n")
  
  preds_test <- predict(model, test_data)
  acc_test <- mean(preds_test == test_data$outcome)
  
  cat("Accuracy sur test set:", round(acc_test, 3), "\n")
  cat("Nombre de folds CV:", 5, "\n")
  cat("Mean Accuracy CV:", round(mean(model$results$Accuracy), 3), "\n")
  
  # ÉTAPE 5: Sauvegarder
  cat("\nÉTAPE 5: Sauvegarde\n")
  cat("───────────────────\n")
  
  dir.create("models", showWarnings = FALSE)
  saveRDS(model, model_path)
  
  cat("✓ Modèle sauvegardé:", model_path, "\n")
  cat("✓ Feature utilisée: goal_diff\n")
  cat("✓ Données d'entraînement: ", nrow(augmented_data), " matchs\n\n")
  
  cat("╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  ENTRAÎNEMENT TERMINÉ AVEC SUCCÈS ✓                      ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  
  invisible(model)
}

if (!interactive()) {
  train_with_augmented_data()
}
