# R/team_base_elo.R

library(tibble)

team_base_elo <- tribble(
  ~team,                ~elo,
  # Très gros clubs européens
  "Paris SG",           1968,
  "Real Madrid",        1910,
  "Barcelona",          1945,
  "Bayern Munich",      1993,
  "Manchester City",    1993,
  "Liverpool",          1938,
  "Inter",              1923,
  "Milan",              1822,
  "Juventus",           1812,
  "Arsenal",            2054,
  "Chelsea",            1887,
  "Atlético Madrid",    1875,
  "Napoli",             1821,
  "Tottenham",          1816,
  "Borussia Dortmund",  1862,
  "RB Leipzig",         1800,
  "Porto",              1805,
  "Benfica",            1795,
  "Sporting CP",        1828,
  "Ajax",               1780,

  # Ligue 1 – ordre global réaliste, valeurs autour de la moyenne France ~1665
  # (les valeurs exactes varient, mais le ranking relatif est cohérent).
  "Lyon",               1710,
  "Marseille",          1730,
  "Monaco",             1750,
  "Lille",              1700,
  "Nice",               1685,
  "Rennes",             1675,
  "Lens",               1690,
  "Nantes",             1650,
  "Strasbourg",         1630,
  "Montpellier",        1620,
  "Reims",              1625,
  "Toulouse",           1605,
  "Brest",              1640,
  "Metz",               1580,
  "Saint-Étienne",      1660,
  "Le Havre",           1590,
  "Lorient",            1600,
  "Auxerre",            1610,
  "Clermont",           1575,
  "Angers",             1595
)

# Helper: return initial/base elo for a team
get_initial_elo <- function(team_name) {
  val <- team_base_elo$elo[match(team_name, team_base_elo$team)]
  if (is.na(val)) {
    val <- mean(team_base_elo$elo, na.rm = TRUE)
  }
  val
}
