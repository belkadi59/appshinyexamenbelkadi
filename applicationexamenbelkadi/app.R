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

        # Show a plot of the generated distribution
        mainPanel(
          textOutput(outputId = "info"),
          plotly::plotlyOutput(outputId = "diamondsplot"),
          DT::DTOutput(outputId = "tableau")
        )
    )
)

# SERVER
server <- function(input, output) {
    observeEvent(input$boutton, {
      showNotification(
        paste("prix : ",input$prix , " & color : ",input$couleur),
        type = "message"
      )
    })
  
    output$info <- renderText({
      paste("prix : ",input$prix , " & color : ",input$couleur)
    })
    
    output$diamondsplot <- plotly::renderPlotly({
      legraph <- diamonds |> 
        filter(price > input$prix & color == input$couleur)|>
        ggplot(aes(x=carat, y=price)) + 
        geom_point(color = input$rose)
      plotly::ggplotly(legraph)
    })

    output$tableau <- DT::renderDT({
      diamonds |> 
        select(-c(x, y, z)) |>
        filter(price > input$prix & color == input$couleur)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
