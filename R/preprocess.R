# R/preprocess.R

library(dplyr)
library(lubridate)

#' Preparer les donnees pour l'entrainement
#'
#' Cette fonction suppose un CSV avec au minimum :
#' home_team, away_team, home_goals, away_goals, match_date
#' Adapte si ton schema est different.
#'
#' @param raw_data tibble brut retourne par fetch_matches_data()
#' @return liste(list(data = df, target_col = "outcome"))
prepare_training_data <- function(raw_data) {

  df <- raw_data %>%
    mutate(
      match_date = lubridate::ymd(match_date),
      outcome = case_when(
        home_goals > away_goals ~ "H",
        home_goals < away_goals ~ "A",
        TRUE ~ "D"
      ),
      goal_diff = home_goals - away_goals
    ) %>%
    filter(!is.na(outcome))

  df$outcome <- factor(df$outcome, levels = c("H", "D", "A"))

  list(
    data = df,
    target_col = "outcome"
  )
}
