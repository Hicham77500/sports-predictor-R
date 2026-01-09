library(dplyr)
library(caret)

source("R/fetch_data.R")
source("R/preprocess.R")

#' Entraîner un modèle SIMPLE et ROBUSTE
#' Basé uniquement sur goal_diff qui est le vrai signal
train_outcome_model <- function(
  input_path = "data/raw/matches.csv",
  model_path = "models/model.rds"
) {
  dir.create("models", showWarnings = FALSE)

  cat("=== ENTRAÎNEMENT MODÈLE SIMPLE (GOAL_DIFF ONLY) ===\n\n")
  
  # Charger et préparer
  raw_data <- fetch_matches_data(input_path)
  prep <- prepare_training_data(raw_data)
  df <- prep$data
  
  cat("Nombre de matchs:", nrow(df), "\n")
  cat("Distribution outcomes:\n")
  print(table(df$outcome))

  # Feature principale: goal_diff
  df_model <- df %>%
    select(goal_diff, outcome) %>%
    mutate(outcome = factor(outcome, levels = c("H", "D", "A")))

  # Split 80/20
  set.seed(42)
  idx_train <- sample(1:nrow(df_model), size = floor(0.8 * nrow(df_model)))
  train_data <- df_model[idx_train, ]
  test_data  <- df_model[-idx_train, ]

  # Entraîner avec caret - 3-fold CV seulement
  ctrl <- trainControl(
    method = "cv",
    number = 3,
    classProbs = TRUE,
    summaryFunction = multiClassSummary
  )

  cat("\nEntraînement (3-fold CV)...\n")
  model <- caret::train(
    outcome ~ goal_diff,
    data = train_data,
    method = "rf",
    trControl = ctrl,
    ntree = 50,
    nodesize = 3,
    tuneGrid = data.frame(mtry = 1),
    verboseIter = FALSE
  )

  # Évaluer sur test set
  preds_test <- predict(model, test_data)
  acc_test <- mean(preds_test == test_data$outcome)
  
  cat("\n=== RÉSULTATS ===\n")
  cat("Accuracy sur test set:", round(acc_test, 3), "\n")
  cat("Nombre d'échantillons train:", nrow(train_data), "\n")
  cat("Nombre d'échantillons test:", nrow(test_data), "\n")
  cat("Feature utilisée: goal_diff (différence de buts)\n")
  
  # Sauvegarder
  saveRDS(model, model_path)
  cat("\nModèle sauvegardé:", model_path, "\n\n")

  invisible(model)
}

if (!interactive()) {
  train_outcome_model()
}

# Permet de lancer directement depuis Rscript R/train_model.R
if (sys.nframe() == 0) {
  train_outcome_model()
}
