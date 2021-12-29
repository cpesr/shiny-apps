#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(wikidataESR)

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("WikidataESR: page de test"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width=2,
                     textInput("wdid",
                               "Identifiant wikidata",
                               value = "Q2033119"),
                     checkboxGroupInput("props", 
                                        "Relations", 
                                        choices = list("associé" = "associé", 
                                                       "associé de" = "associé_de", 
                                                       "composante" = "composante",
                                                       "composante de" = "composante_de",
                                                       "prédécesseur" = "prédécesseur",
                                                       "successeur" = "successeur",
                                                       "séparé de" = "séparé_de",
                                                       "absorbé par" = "absorbé_par",
                                                       "membre de" = "membre_de",
                                                       "affilié à" = "affilié_à"
                                        ),
                                        selected = "composante"),
                     sliderInput("depth",
                                 "Profondeur",
                                 min = 1,
                                 max = 10,
                                 value = 2)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            withSpinner(plotOutput("wdPlot"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$wdPlot <- renderPlot({
        wdesr_load_and_plot(input$wdid, input$props, depth = input$depth)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
