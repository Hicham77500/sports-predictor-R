library(httr)
library(jsonlite)
library(dplyr)
library(readr)

#' Recuperer les matchs depuis l'API football-data.org v4
#'
#' @param date_from date de debut au format "YYYY-MM-DD"
#' @param date_to date de fin au format "YYYY-MM-DD"
#' @param competitions ids de competitions (ex: "PL,CL"), ou NULL pour toutes celles de ton plan
#' @param api_key cle API (X-Auth-Token)
#' @return tibble de matchs normalises
fetch_matches_from_api <- function(
  date_from,
  date_to,
  competitions = NULL,
  api_key = Sys.getenv("FOOTBALL_DATA_API_KEY")
) {
  if (api_key == "") {
    stop("Variable d'env FOOTBALL_DATA_API_KEY manquante.")
  }

  base_url <- "https://api.football-data.org/v4/matches"

  query <- list(
    dateFrom = date_from,
    dateTo   = date_to
  )
  if (!is.null(competitions)) {
    query$competitions <- competitions
  }

  res <- httr::GET(
    url = base_url,
    query = query,
    httr::add_headers("X-Auth-Token" = api_key)
  )

  if (httr::status_code(res) != 200) {
    stop(sprintf("Erreur API HTTP %d", httr::status_code(res)))
  }

  txt  <- httr::content(res, as = "text", encoding = "UTF-8")
  json <- jsonlite::fromJSON(txt, flatten = TRUE)

  if (length(json$matches) == 0) {
    warning("Aucun match retourne par l'API pour cette plage de dates.")
    return(tibble::tibble())
  }

  # Structure v4 : matches[[i]]$homeTeam$name, etc.
  df <- json$matches %>%
    tibble::as_tibble() %>%
    transmute(
      match_id   = id,
      match_date = as.Date(utcDate),
      status     = status,
      competition = competition.name,
      home_team  = homeTeam.name,
      away_team  = awayTeam.name,
      home_goals = score.fullTime.home,
      away_goals = score.fullTime.away
    )

  df
}

#' Sauvegarder les matchs dans data/raw/matches.csv
#'
#' @param date_from, date_to "YYYY-MM-DD"
#' @param competitions ids de competitions ou NULL
#' @param out_path chemin de sortie
fetch_and_save_matches <- function(
  date_from,
  date_to,
  competitions = NULL,
  out_path = "data/raw/matches.csv"
) {
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

  df <- fetch_matches_from_api(
    date_from   = date_from,
    date_to     = date_to,
    competitions = competitions
  )

  if (nrow(df) == 0) {
    warning("Aucun match sauvegarde (dataset vide).")
  } else {
    readr::write_csv(df, out_path)
    message(sprintf("Sauvegarde %d matchs dans %s", nrow(df), out_path))
  }

  invisible(df)
}

#' Charger les donnees locales
fetch_matches_data <- function(
  path = "data/raw/matches.csv"
) {
  if (!file.exists(path)) {
    stop(
      sprintf(
        "Fichier introuvable : %s. Appelle d'abord fetch_and_save_matches().",
        path
      )
    )
  }

  matches <- readr::read_csv(path, show_col_types = FALSE)
  janitor::clean_names(matches)
}
