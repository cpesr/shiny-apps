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
library(tidyverse)
library(ggcpesrthemes)

theme_cpesr_setup(source="https://data.cpesr.fr/wikidataesr/")

etab <- unique(na.omit(select(filter(kpiESR::esr.etab,Groupe=="Universités et assimilés"),Etablissement,url.wikidata)))
etab$wdid <- substr(etab$url.wikidata,33,100)
etablist <- etab$wdid
names(etablist) <- etab$Etablissement
rand <- sample.int(nrow(etab),1)
rand.wdid=etab[rand,3]

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("WikidataESR: Tenter d’y voir clair dans l’ESR (version béta)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width=2,
                     selectInput("etab", "Etablissement", choices = etablist, selected=rand.wdid),
                     textInput("wdid", "Identifiant wikidata", value = rand.wdid),
                     checkboxGroupInput("props", "Relations", 
                                        choices = list("associé (P527)" = "associé", 
                                                       "associé de (P361)" = "associé_de", 
                                                       "composante (P355)" = "composante",
                                                       "composante de (P749)" = "composante_de",
                                                       "prédécesseur (P1365)" = "prédécesseur",
                                                       "successeur (P1366)" = "successeur",
                                                       "séparé de (P807)" = "séparé_de",
                                                       "absorbé par (P7888)" = "absorbé_par",
                                                       "membre de (P463)" = "membre_de",
                                                       "affilié à (P1416)" = "affilié_à"
                                        ),
                                        selected = "composante"),
                     sliderInput("depth", "Profondeur de recherche", min = 1, max = 10, value = 1),
                     checkboxInput("active_only", "Actifs seulement", value = FALSE),
                     h4("Paramètres graphiques"),
                     sliderInput("size", "Taille (%)", min = 0, max = 300, value = 100),
                     sliderInput("node_size", "Taille noeuds", min = 1, max = 80, value = c(30,60)),
                     sliderInput("label_size", "Taille texte", min = 1, max = 15, value = c(4,6)),
                     sliderInput("label_wrap", "Largeur texte", min = 10, max = 50, value = 15),
                     selectInput("node_label", "Type de label", 
                                 choices = list("Alias" = "alias", 
                                                "Alias et date" = "alias_date", 
                                                "Nom complet" = "long",
                                                "Nom et dat" = "long_date"),
                                 selected = "alias"),
                     selectInput("node_type", "Type de noeud", 
                                 choices = list("Texte" = "text",
                                                "Texte décalé" = "text_repel", 
                                                "Boite" = "label", 
                                                "Boite décalée" = "label_repel"),
                                 selected = "text"),
                     checkboxInput("edge_label", "Labels des arcs", value = TRUE),
                     checkboxInput("edge_arrow", "Flêche", value = FALSE),
                     sliderInput("arrow_gap", "Distance flêche", min = 0, max = 0.3, value = 0.05, step=0.01)
                     ),
        
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                withSpinner(plotOutput("wdPlot", height="600px")),
                htmlOutput("wdurl"),
                h3("Avertissements"),
                h4("Avertissements pour les entités"),
                tableOutput("vertices_warnings"),
                h4("Avertissements pour les relations"),
                tableOutput("edges_warnings"),
                h2("Aide"),
                HTML('<ul>
                     <li><a href="https://github.com/cpesr/wikidataESR/blob/master/README.md#Guide-de-style-pour-les-données-wikidata">
                        Explications générales</a></li>
                     <li><a href="https://github.com/cpesr/wikidataESR/blob/master/README.md#Une-composante-UFRlabo-de-létablissement-est-manquante">
                        Une composante (UFR/labo) de l’établissement est manquante</a></li>
                     <li><a href="https://github.com/cpesr/wikidataESR/blob/master/README.md#Une-association-COMUE-etc-de-létablissement-est-manquante">
                        Une association (COMUE, etc.) de l’établissement est manquante</a></li>
                     <li><a href="https://github.com/cpesr/wikidataESR/blob/master/README.md#Une-tutelle-de-létablissement-est-manquante">
                        Une tutelle de l’établissement est manquante</a></li>
                     <li><a href="https://github.com/cpesr/wikidataESR/blob/master/README.md#Une-adhésion-de-létablissement-à-une-organisation-est-manquante">
                        Une adhésion de l’établissement à une organisation est manquante</a></li>
                     <li><a href="https://github.com/cpesr/wikidataESR/blob/master/README.md#Une-relation-composantetutelleadhésion-a-pris-fin">
                        Une relation (composante/tutelle/adhésion) a pris fin</a></li>
                     <li><a href="https://github.com/cpesr/wikidataESR/blob/master/README.md#Un-établissement-nexiste-plus">
                        Un établissement n’existe plus</a></li>
                     <li><a href="https://github.com/cpesr/wikidataESR/blob/master/README.md#Un-établissement-a-changé-de-statut">
                        Un établissement a changé de statut</a></li>
                     </ul>')
            )
        )

    )
)

wdurl <- function(wdid) {
    url <- paste0('https://www.wikidata.org/wiki/',wdid)
    paste0('<a href=',url,'>',urld,'</a')
}

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    observeEvent(input$etab, { updateTextInput(session, "wdid",value=input$etab)})
    observeEvent(input$wdid, { updateTextInput(session, "etab",value=input$wdid)})
    
    df.g <- reactive(wdesr_get_graph(input$wdid, input$props, depth = input$depth, 
                        active_only = input$active_only))
    
    output$wdPlot <- renderPlot({
        wdesr_ggplot_graph(df.g(),
                            size = input$size / 100,
                            node_size = input$node_size,
                            label_size = input$label_size,
                            label_wrap = input$label_wrap,
                            node_label = input$node_label,
                            node_type = input$node_type,
                            edge_label = input$edge_label,
                            edge_arrow =  input$edge_arrow,
                            arrow_gap = input$arrow_gap
                            ) + cpesr_cap()
    })
    
    output$vertices_warnings <- renderTable(
        wdesr_log_warnings_vertices(df.g()) %>% mutate(lien = paste0("https://www.wikidata.org/wiki/",entité))
        )

    output$edges_warnings <- renderTable(
        wdesr_log_warnings_edges(df.g()) %>% mutate(
            depuis.lien = paste0("https://www.wikidata.org/wiki/",depuis),
            vers.lien =  paste0("https://www.wikidata.org/wiki/",vers)) 
    )
    
    output$wdurl <- renderText({
        url <- paste0('https://www.wikidata.org/wiki/',input$wdid)
        paste0('<h4 style="text-align:center"><a href=',url,'>',url,'</a></h4>')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
