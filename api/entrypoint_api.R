#!/opt/homebrew/bin/Rscript
# ============================================================================
# entrypoint_api.R
# Lance l'API Plumber
# ============================================================================

suppressPackageStartupMessages({
  library(plumber)
})

project_root <- "/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R"

cat("\n╔═══════════════════════════════════════════════════════════╗\n")
cat("║  DÉMARRAGE API V2 (ELO + NNET)                          ║\n")
cat("╚═══════════════════════════════════════════════════════════╝\n")

# Charger l'API
api <- plumb(file.path(project_root, "api/api.R"))

# Lancer sur port 8001 (pour ne pas confliter avec l'ancienne)
cat("\n✓ Serveur lancé sur http://localhost:8001\n")
cat("✓ Endpoints:\n")
cat("   GET  /health\n")
cat("   POST /predict_v2?home_team=PSG&away_team=Barcelona\n")
cat("   GET  /elo/<team>\n")
cat("   GET  /top_teams/<n>\n")
cat("   GET  /stats\n\n")

api$run(host = "127.0.0.1", port = 8001)
