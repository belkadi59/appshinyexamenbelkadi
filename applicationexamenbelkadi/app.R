# Mon application Shiny pour l'examen

# Liste des librairies
library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
thematic::thematic_shiny(font = "auto")

# UI
ui <- fluidPage(
    # Theme de l’application
    theme = bs_theme(
      version = 5,
      bootswatch = "minty"
    ),
    
    # Application title
    titlePanel("Exploration des Diamants"),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
          
            radioButtons(inputId="rose", label = "Colorier les points en rose ?",
                       choices = list("Oui" = "pink", "Non" = "black"), 
                       selected = "pink"),
            
            selectInput(inputId="couleur", label = "Choisir une couleur à filtrer :", 
                        choices = list("D" = "D", "E"="E", "F"="F", "G"="G", "H"="H", "I"="I", "J"="J"), 
                        selected = "D"),
            
            sliderInput(inputId ="prix",
                        label="Prix maximum :",
                        min = 300,
                        max = 20000,
                        value = 300),
            
            actionButton(inputId = "boutton", label = "Visualiser le graph")
        ),

        mainPanel(
          plotly::plotlyOutput(outputId = "diamondsplot"),
          DT::DTOutput(outputId = "tableau")
        )
    )
)

# SERVER
server <- function(input, output) {
  
    #Pour contenir les résultats filtrés
    rv <- reactiveValues(df = NULL, 
                         prix_diamonds = NULL,
                         couleur_diamonds = NULL,
                         color_point = NULL) 
    
    observeEvent(input$boutton, {
      
      # On enregistre les valeurs au moment du clic
      rv$prix_diamonds <- input$prix
      rv$couleur_diamonds <- input$couleur
      rv$color_point <- input$rose
      
      showNotification(
        paste("prix : ",rv$prix_diamonds , " & color : ",rv$couleur_diamonds),
        type = "message"
      )
      # On filtre une seule fois
      rv$df <- diamonds |>
        filter(price > rv$prix_diamonds & color == rv$couleur_diamonds)
    })
  
    # Graphique
    output$diamondsplot <- plotly::renderPlotly({
      req(rv$df)   # empêche l’affichage au démarrage
      legraph <- rv$df |> 
        ggplot(aes(x=carat, y=price)) + 
        geom_point(color = rv$color_point)+
        labs(
          title = paste("prix : ",rv$prix_diamonds , " & color : ",rv$couleur_diamonds)
        )
      plotly::ggplotly(legraph)
    })

    # Tableau
    output$tableau <- DT::renderDT({
      req(rv$df)
      rv$df |> 
        select(-c(x, y, z))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
