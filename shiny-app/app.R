library(shiny)
library(dplyr)
library(tibble)
library(readr)

# Chemin absolu du projet
project_root <- "/Users/corsair/Documents/IPSSI/Projet IPSSI/sports-predictor-R"

# Charger les utilitaires Elo et prédiction
source(file.path(project_root, "R/elo_utils.R"))
source(file.path(project_root, "R/predict_outcome.R"))

# Charger le modèle et les Elo
loaded <- tryCatch({
  load_model_and_elo(project_root, "v2")
}, error = function(e) {
  cat("Erreur chargement modèle:", as.character(e), "\n")
  NULL
})

if (is.null(loaded)) {
  stop("Impossible de charger le modèle. Avez-vous exécuté train_outcome_model.R ?")
}

MODEL <- loaded$model
ELO_MAP <- loaded$elo_map

# Récupérer la liste des équipes
teams <- sort(unique(names(ELO_MAP)))

ui <- fluidPage(
  titlePanel("⚽ Sports Predictor - Prédiction de Matchs"),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #f5f5f5; padding: 20px;",
      
      h3("Configurez le Match", style = "color: #333;"),
      hr(),
      
      # Sélecteur Équipe 1
      h5("Équipe 1", style = "margin-top: 15px; color: #007bff;"),
      selectInput("team1", "Sélectionner l'équipe", choices = teams, selected = "Manchester United"),
      
      # Sélecteur Équipe 2
      h5("Équipe 2", style = "margin-top: 15px; color: #007bff;"),
      selectInput("team2", "Sélectionner l'équipe", choices = teams, selected = "Manchester City"),
      
      hr(),
      
      # Sélecteur domicile/extérieur
      h5("Point de vue", style = "margin-top: 15px; color: #007bff;"),
      radioButtons("perspective", "Qui joue à domicile ?",
        choices = list("Équipe 1 à domicile" = "team1_home", "Équipe 2 à domicile" = "team2_home"),
        selected = "team1_home"
      ),
      
      hr(),
      
      # Bouton prédiction
      actionButton("predict_btn", "🎯 Prédire le Résultat", 
        class = "btn btn-success btn-lg",
        style = "width: 100%; margin-top: 20px; font-size: 16px;"
      ),
      
      br(), br(),
      p(em("Sélectionnez deux équipes différentes et cliquez sur Prédire"), 
        style = "color: #666; font-size: 12px;")
    ),
    
    mainPanel(
      h3("📊 Résultat de la Prédiction"),
      uiOutput("result")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$predict_btn, {
    tryCatch({
      # VALIDATION
      if (input$team1 == input$team2) {
        output$result <- renderUI({
          div(style = "padding: 20px; background-color: #fff3cd; border: 2px solid #ffc107; border-radius: 8px; color: #856404;",
            h4("⚠️ Erreur de saisie"),
            p("Veuillez sélectionner deux équipes différentes !")
          )
        })
        showNotification("Les deux équipes doivent être différentes", type = "warning")
        return()
      }
      
      # Déterminer qui joue à domicile
      if (input$perspective == "team1_home") {
        home_team <- input$team1
        away_team <- input$team2
        home_label <- input$team1
        away_label <- input$team2
      } else {
        home_team <- input$team2
        away_team <- input$team1
        home_label <- input$team2
        away_label <- input$team1
      }
      
      # Prédiction avec le modèle V2
      pred <- predict_outcome_from_teams(
        home_team = home_team,
        away_team = away_team,
        model = MODEL,
        elo_map = ELO_MAP
      )
      
      # Récupérer les infos
      pred_class <- pred$class
      probas <- pred$proba
      info <- pred$info
      
      # Texte résultat
      match_display <- paste(input$team1, "vs", input$team2)
      
      # Résultat en français
      outcome_text <- switch(pred_class,
        "H" = "Victoire",
        "D" = "Match Nul",
        "A" = "Défaite",
        "Inconnu"
      )
      
      outcome_text_detail <- if (pred_class == "H") {
        paste(home_label, "gagne")
      } else if (pred_class == "D") {
        "Match nul"
      } else {
        paste(away_label, "gagne")
      }
      
      outcome_color <- switch(pred_class,
        "H" = "#28a745",
        "D" = "#ffc107",
        "A" = "#dc3545",
        "#007bff"
      )
      
      # Note d'équipe inconnue
      unknown_note <- ""
      if (!info$home_known || !info$away_known) {
        unknown_teams <- c(
          if (!info$home_known) home_label,
          if (!info$away_known) away_label
        ) %>% paste(collapse = ", ")
        unknown_note <- paste("\n⚠️ Note:", unknown_teams, "non trouvée dans l'historique. Elo par défaut (1500).")
      }
      
      # Afficher résultat
      output$result <- renderUI({
        tagList(
          div(style = paste0("padding: 25px; background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%); border-radius: 10px; margin-bottom: 20px;"),
            h3(style = "color: #333; margin: 0;", match_display),
            p(style = "color: #666; margin: 10px 0; font-size: 14px;",
              paste(home_label, "à domicile vs", away_label, "en déplacement")
            )
          ),
          
          div(style = paste0("padding: 25px; background-color: ", outcome_color, "; border-radius: 10px; color: white; text-align: center; margin-bottom: 20px;"),
            h2(style = "margin: 0;", outcome_text),
            p(style = "margin: 10px 0; font-size: 16px;", outcome_text_detail)
          ),
          
          div(style = "padding: 20px; background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 8px;",
            h4("📊 Probabilités:"),
            div(style = "margin: 10px 0;",
              p(style = "font-size: 15px; margin: 8px 0;",
                span(style = "display: inline-block; width: 150px;", strong(home_label, "gagne:")),
                sprintf("%.1f%%", probas$H * 100),
                div(style = paste0("background-color: #d4edda; height: 20px; width: ", probas$H * 100, "%; border-radius: 4px; display: inline-block; margin-left: 10px;"))
              ),
              p(style = "font-size: 15px; margin: 8px 0;",
                span(style = "display: inline-block; width: 150px;", strong("Match Nul:")),
                sprintf("%.1f%%", probas$D * 100),
                div(style = paste0("background-color: #fff3cd; height: 20px; width: ", probas$D * 100, "%; border-radius: 4px; display: inline-block; margin-left: 10px;"))
              ),
              p(style = "font-size: 15px; margin: 8px 0;",
                span(style = "display: inline-block; width: 150px;", strong(away_label, "gagne:")),
                sprintf("%.1f%%", probas$A * 100),
                div(style = paste0("background-color: #f8d7da; height: 20px; width: ", probas$A * 100, "%; border-radius: 4px; display: inline-block; margin-left: 10px;"))
              )
            ),
            
            # Infos Elo
            hr(),
            h4("🏆 Ratings Elo:"),
            p(style = "font-size: 13px; margin: 5px 0;",
              strong(home_label), ":", round(info$home_elo, 1), "| ",
              strong(away_label), ":", round(info$away_elo, 1), "| ",
              strong("Différence:"), round(info$home_elo - info$away_elo, 1)
            ),
            
            if (unknown_note != "") {
              p(style = "font-size: 12px; color: #ff6b6b; margin-top: 10px;", unknown_note)
            }
          )
        )
      })
    }, error = function(e) {
      output$result <- renderUI({
        div(style = "padding: 20px; background-color: #ffcccc; border: 2px solid #dc3545; border-radius: 8px; color: #721c24;",
          h4("❌ Erreur lors de la prédiction:"),
          p(as.character(e))
        )
      })
      showNotification(paste("Erreur:", as.character(e)), type = "error")
    })
  })
}

shinyApp(ui, server)
