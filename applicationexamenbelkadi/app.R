# Mon application Shiny pour l'examen

# Liste des librairies
library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)

# UI
ui <- fluidPage(

    # Application title
    titlePanel("Exploration des Diamants"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            radioButtons("rose", label = "Colorier les points en rose ?",
                       choices = list("Oui" = 1, "Non" = 2), 
                       selected = 1),
            
            selectInput("couleur", label = "Choisir une couleur à filtrer :", 
                        choices = list("D" = "D", "E"="E", "F"="F", "G"="G", "H"="H", "I"="I", "J"="J"), 
                        selected = "D"),
            
            sliderInput(inputId ="bins",
                        label="Prix maximum :",
                        min = 300,
                        max = 20000,
                        value = 30),
            
            actionButton(inputId = "boutton", label = "Visualiser le graph")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# SERVER
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
