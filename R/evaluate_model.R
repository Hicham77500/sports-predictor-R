#!/opt/homebrew/bin/Rscript
# ============================================================================
# evaluate_model.R
# Évaluation, diagnostic et calibration du modèle
# ============================================================================
# OBJECTIF : Calculer métriques complètes et générer diagnostics
# FUNCTION : evaluate_outcome_model(model, test_data)
#   INPUT  : Modèle caret + test_data avec colonnes outcome + features
#   OUTPUT : list(accuracy, f1_scores, confusion_matrix, diagnostics)
# USAGE    : eval <- evaluate_outcome_model(model, test_df)
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
})

# ============================================================================
# FONCTION 1 : Courbe de calibration
# ============================================================================
#' Calcule et trace la courbe de calibration
#'
#' @param actual Vecteur des outcomes réels (0, 1, ou facteur)
#' @param predicted Vecteur des probabilités prédites (entre 0 et 1)
#' @param bins Nombre de bins pour la calibration (défaut 10)
#' @param plot Si TRUE, trace le graphique
#'
#' @return data.frame avec colonnes: bin, mean_predicted, mean_actual, n
#'
calibration_curve <- function(actual, predicted, bins = 10, plot = TRUE) {
  
  # Convertir actual en numérique [0, 1]
  if (is.factor(actual)) {
    actual_numeric <- as.numeric(actual == levels(actual)[2])
  } else {
    actual_numeric <- as.numeric(actual)
  }
  
  # Créer les bins
  df <- tibble(
    actual = actual_numeric,
    predicted = predicted
  ) %>%
    mutate(
      bin = cut(predicted, breaks = seq(0, 1, 1/bins), include.lowest = TRUE)
    ) %>%
    group_by(bin) %>%
    summarise(
      mean_predicted = mean(predicted, na.rm = TRUE),
      mean_actual = mean(actual, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(bin))
  
  if (plot) {
    p <- ggplot(df, aes(x = mean_predicted, y = mean_actual, size = n)) +
      geom_point(alpha = 0.6, color = "blue") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      xlim(0, 1) + ylim(0, 1) +
      labs(
        title = "Calibration Curve",
        x = "Mean Predicted Probability",
        y = "Actual Frequency",
        size = "N"
      ) +
      theme_minimal() +
      coord_fixed()
    
    print(p)
  }
  
  return(df)
}

# ============================================================================
# FONCTION 2 : Diagnostic complet du modèle
# ============================================================================
#' Diagnostic complet : accuracy, calibration, reliability
#'
#' @param actual Vecteur des outcomes réels
#' @param predicted Matrice des probabilités prédites (colonnes: classe, lignes: observations)
#' @param class_names Noms des classes
#'
#' @return list avec diagnostics
#'
diagnose_model <- function(actual, predicted, class_names = c("H", "D", "A")) {
  
  cat("\n╔═══════════════════════════════════════════════════════════╗\n")
  cat("║  DIAGNOSTIC DU MODÈLE                                   ║\n")
  cat("╚═══════════════════════════════════════════════════════════╝\n")
  
  # Prédictions
  pred_class <- colnames(predicted)[apply(predicted, 1, which.max)]
  pred_class_factor <- factor(pred_class, levels = class_names)
  actual_factor <- factor(actual, levels = class_names)
  
  # Accuracy globale
  accuracy <- mean(pred_class_factor == actual_factor)
  cat("\n✓ ACCURACY\n")
  cat("  Global Accuracy:", round(accuracy * 100, 2), "%\n")
  
  # Accuracy par classe
  for (cls in class_names) {
    mask <- actual_factor == cls
    if (sum(mask) > 0) {
      acc_cls <- mean(pred_class_factor[mask] == cls)
      cat("  ", cls, "Accuracy:", round(acc_cls * 100, 2), "%\n")
    }
  }
  
  # Log-loss
  cat("\n✓ LOG-LOSS (diminuer c'est bien)\n")
  predicted_clamped <- pmax(pmin(predicted, 1 - 1e-10), 1e-10)
  
  log_loss_total <- 0
  for (i in seq_len(nrow(predicted))) {
    true_class <- actual[i]
    class_idx <- which(class_names == true_class)
    log_loss_total <- log_loss_total + (-log(predicted_clamped[i, class_idx]))
  }
  log_loss_avg <- log_loss_total / nrow(predicted)
  cat("  Mean Log-Loss:", round(log_loss_avg, 4), "\n")
  
  # Brier Score
  cat("\n✓ BRIER SCORE (diminuer c'est bien)\n")
  actual_onehot <- model.matrix(~ factor(actual, levels = class_names) - 1)
  brier <- mean((predicted - actual_onehot)^2)
  cat("  Mean Brier Score:", round(brier, 4), "\n")
  
  # Par classe
  for (j in seq_len(ncol(predicted))) {
    brier_cls <- mean((predicted[, j] - actual_onehot[, j])^2)
    cat("  ", class_names[j], "Brier:", round(brier_cls, 4), "\n")
  }
  
  # ECE (Expected Calibration Error)
  cat("\n✓ ECE - Expected Calibration Error (diminuer c'est bien)\n")
  
  max_proba <- apply(predicted, 1, max)
  ece <- 0
  bins <- 10
  for (b in seq_len(bins)) {
    lower <- (b-1) / bins
    upper <- b / bins
    mask <- max_proba >= lower & max_proba < upper
    
    if (sum(mask) > 0) {
      mean_conf <- mean(max_proba[mask])
      accuracy_conf <- mean(pred_class_factor[mask] == actual_factor[mask])
      ece <- ece + sum(mask) / length(max_proba) * abs(mean_conf - accuracy_conf)
    }
  }
  cat("  ECE:", round(ece, 4), "\n")
  
  # Matrice de confusion
  cat("\n✓ CONFUSION MATRIX\n")
  conf_mat <- table(pred_class_factor, actual_factor)
  print(conf_mat)
  
  # Distribution des probabilités prédites
  cat("\n✓ DISTRIBUTION DES PROBABILITÉS MAXIMALES\n")
  max_proba_mean <- mean(max_proba)
  cat("  Confiance moyenne:", round(max_proba_mean * 100, 2), "%\n")
  cat("  Min:", round(min(max_proba) * 100, 2), "%\n")
  cat("  Max:", round(max(max_proba) * 100, 2), "%\n")
  cat("  Médiane:", round(median(max_proba) * 100, 2), "%\n")
  
  # Combien de prédictions très confiantes (>90%)
  n_very_confident <- sum(max_proba > 0.9)
  pct_very_confident <- (n_very_confident / length(max_proba)) * 100
  cat("  Prédictions >90% confiance:", pct_very_confident, "%\n")
  
  return(list(
    accuracy = accuracy,
    log_loss = log_loss_avg,
    brier_score = brier,
    ece = ece,
    confusion_matrix = conf_mat
  ))
}

# ============================================================================
# FONCTION 3 : Reliability Diagram (visualisation)
# ============================================================================
#' Crée un reliability diagram (diagramme de calibration avancé)
#'
#' @param actual Vecteur des outcomes réels (facteur ou 0/1)
#' @param predicted Vecteur des probabilités prédites
#' @param n_bins Nombre de bins
#'
#' @return ggplot2 plot
#'
reliability_diagram <- function(actual, predicted, n_bins = 10) {
  
  # Convertir actual en 0/1
  if (is.factor(actual)) {
    actual_binary <- as.numeric(actual == levels(actual)[2])
  } else {
    actual_binary <- as.numeric(actual)
  }
  
  # Créer les bins
  df <- tibble(
    actual = actual_binary,
    predicted = predicted
  ) %>%
    mutate(
      bin = cut(predicted, breaks = seq(0, 1, 1/n_bins), 
                include.lowest = TRUE, labels = FALSE)
    ) %>%
    group_by(bin) %>%
    summarise(
      mean_predicted = mean(predicted, na.rm = TRUE),
      mean_actual = mean(actual, na.rm = TRUE),
      n = n(),
      se = sqrt(mean_actual * (1 - mean_actual) / n()),
      .groups = "drop"
    ) %>%
    filter(!is.na(bin))
  
  p <- ggplot(df, aes(x = mean_predicted, y = mean_actual)) +
    geom_errorbar(aes(ymin = mean_actual - 1.96*se, ymax = mean_actual + 1.96*se),
                  width = 0.05, color = "grey") +
    geom_point(aes(size = n), alpha = 0.6, color = "blue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
    geom_area(aes(x = c(0, 1), y = c(0, 1)), alpha = 0.1, color = "red", fill = "red") +
    xlim(0, 1) + ylim(0, 1) +
    labs(
      title = "Reliability Diagram",
      x = "Mean Predicted Probability",
      y = "Observed Frequency",
      size = "Count"
    ) +
    theme_minimal() +
    coord_fixed() +
    theme(aspect.ratio = 1)
  
  return(p)
}

# ============================================================================
# FONCTION 4 : Rapport complet
# ============================================================================
#' Génère un rapport complet d'évaluation
#'
#' @param test_data data.frame avec colonne outcome
#' @param predictions list(class, proba) retourné par predict()
#'
#' @return invisible(list de diagnostics)
#'
generate_evaluation_report <- function(test_data, predictions) {
  
  actual <- test_data$outcome
  pred_class <- predictions$class
  pred_proba <- predictions$proba
  
  # Diagnostics
  diag <- diagnose_model(actual, pred_proba)
  
  # Calibration
  cat("\n✓ CALIBRATION (par classe)\n")
  
  # Pour la classe "H"
  actual_binary_H <- as.numeric(actual == "H")
  cal_H <- calibration_curve(actual_binary_H, pred_proba[, "H"], bins = 5, plot = FALSE)
  
  # Exporter rapports optionnels
  cat("\n✓ Rapport complet généré\n")
  
  return(invisible(diag))
}
