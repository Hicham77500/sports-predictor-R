library(dplyr)

#' Charger le modele depuis models/model.rds
load_outcome_model <- function(
  model_path = "models/model.rds"
) {
  if (!file.exists(model_path)) {
    stop(
      sprintf(
        "Modele introuvable : %s. Lance d'abord R/train_model.R.",
        model_path
      )
    )
  }
  readRDS(model_path)
}

#' Predire un match unique
#'
#' @param model objet caret
#' @param newdata data.frame avec les memes features que l'entrainement
#' @return liste(probas = vec, class = string)
predict_outcome_single <- function(model, newdata) {
  probas <- predict(model, newdata, type = "prob")
  class  <- predict(model, newdata, type = "raw")

  list(
    probas = probas[1, ],
    class  = as.character(class[1])
  )
}

#' Predictions batch a partir d'un CSV
#'
#' @param input_path CSV d'entree
#' @param output_path CSV avec predictions
predict_outcome_batch <- function(
  input_path = "data/raw/to_predict.csv",
  output_path = "data/predictions.csv",
  model_path = "models/model.rds"
) {
  model <- load_outcome_model(model_path)

  df <- readr::read_csv(input_path, show_col_types = FALSE)

  probas <- predict(model, df, type = "prob")
  classes <- predict(model, df, type = "raw")

  res <- df %>%
    mutate(
      pred_class = classes,
      pred_home = probas$H,
      pred_draw = probas$D,
      pred_away = probas$A
    )

  readr::write_csv(res, output_path)
  invisible(res)
}
