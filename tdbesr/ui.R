#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

fig.height <- 300

kpidesc <- function(kpi) {
  res <- h3("Dictionnaire des variables")
  for(i in seq(1,length(kpiESR::kpiesr_lfc[[kpi]]$factors))) {
      res <- paste(res,tags$li(
        tags$b(kpiESR::kpiesr_lfc[[kpi]]$labels[i]), " : ", kpiESR::kpiesr_lfc[[kpi]]$desc[i]))
  }
  res <- paste(res, h3("Avertissement"), "Ces représentations sont issues de traitements automatiques de données ouvertes, et sont donc potentiellement sujettes à des erreurs.")
  return(tags$ul(HTML(res)))
}

kpiguide_abs <- tags$li(tags$b("Valeurs absolues")," : Les valeurs absolues des indicateurs sont pésentées en barres pour l'année de référence.
            Elles permettent de percevoir l'état le plus récent du groupe ou de l'établissement.
            La première barre correspond à l'",tags$b("indicateur principal")," et les barres suivantes aux ",tags$b("indicateurs secondaires"))
kpiguide_norm <- tags$li(tags$b("Valeurs normalisées")," : Les valeurs normalisées sont les rapports entre les indicateurs secondaires et l'indicateur principal, à l'année de référence.
            Elles permettent la comparaison entre établissements. 
            Le point central donne la valeur pour l'établissement ou le groupe.
            Les autres points donnent les valeurs pour les autres établissements du groupe.
            Le violon montre la densité de la distribution.")
kpiguide_kpi <- tags$li(tags$b("Indicateurs clés de performance"),
            " : Le point central donne la valeur de l'indicateur pour l'établissement ou le groupe.
            Les autres points donnent les valeurs pour les autres établissements du groupe.
            Le violon montre la densité de la distribution.")
kpiguide_evol <- tags$li(tags$b("Evolutions")," : Les évolutions sont montrées en valeur 100 pour la première année disponible dans les données.
            Les lignes indiquent l'évolution du groupe et de l'ensemble de l'ESR dans le périmètre MESRI.
            La zone grise indique les évolutions de la moitié des établissements du groupe.")

kpiguide <- p(
  h3("Guide de lecture"),
  tags$ul(kpiguide_abs,kpiguide_norm,kpiguide_evol)
)

kpiguide_k <- p(
  h3("Guide de lecture"),
  tags$ul(kpiguide_kpi,kpiguide_evol)
)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel("Tableaux de bord de l'ESR", windowTitle = "tdbesr"),
    fluidRow(
      column(2, selectInput(inputId = "groupe", label = "Groupe", choices = kpiESR::kpiesr_shinycfg$groupes)),
      column(2, selectInput(inputId = "etab", label = "Etablissement", choices = kpiESR::kpiesr_shinycfg$etabs$Ensemble)),
      column(4, offset = 4, p("Année de référence : 2020-2021"), 
             p("Contact et demandes de données :", a("data@cpesr.fr",href="mailto:data@cpesr.fr")))
    ),
    
    tabsetPanel(type = "tabs",
                tabPanel("KPI", br(), 
                         fluidRow(
                           plotOutput("kpi.norm", height = fig.height*0.9),
                           plotOutput("kpi.evol", height = fig.height*1.1)),
                         fluidRow(
                           hr(),
                           column(6, kpidesc("K") ),
                           column(6, kpiguide_k) )
                         ),
                tabPanel("Etudiants", br(), fluidRow(
                  column(6,plotOutput("etu.abs", height = fig.height),plotOutput("etu.evol", height = fig.height)),
                  column(6,plotOutput("etu.norm", height = 2*fig.height))),
                  fluidRow(
                    hr(),
                    column(6, kpidesc("ETU") ),
                    column(6, kpiguide) )),
                
                tabPanel("BIATSS", br(), fluidRow(
                  column(6,plotOutput("bia.abs", height = fig.height),plotOutput("bia.evol", height = fig.height)),
                  column(6,plotOutput("bia.norm", height = 2*fig.height))),
                  fluidRow(
                    hr(),
                    column(6, kpidesc("BIA") ),
                    column(6, kpiguide) )),
                
                tabPanel("Enseignants", br(), fluidRow(
                  column(6,plotOutput("ens.abs", height = fig.height),plotOutput("ens.evol", height = fig.height)),
                  column(6,plotOutput("ens.norm", height = 2*fig.height))),
                  fluidRow(
                    hr(),
                    column(6, kpidesc("ENS") ),
                    column(6, kpiguide) )),
                
                tabPanel("Finances", br(), fluidRow(
                  column(6,plotOutput("fin.abs", height = fig.height),plotOutput("fin.evol", height = fig.height)),
                  column(6,plotOutput("fin.norm", height = 2*fig.height))),
                  fluidRow(
                    hr(),
                    column(6, kpidesc("FIN") ),
                    column(6, kpiguide) )),

                tabPanel("Series", br(), sidebarLayout(
                  sidebarPanel(width=2,
                               tags$head(tags$style("#map{height:75vh !important;}")),
                               radioButtons("serie_val", "Type de série :",
                                            c("Valeurs absolues" = "valeur",
                                              "Evolution Valeur 100" = "evolution"),
                                            selected = "evolution"
                                            ),
                               selectInput(inputId = "serie_vars", 
                                           label = "Valeur", 
                                           choices = kpiESR::kpiesr_shinycfg$kpi_list, 
                                           selected = c("kpi.K.ensPetu","kpi.ETU.P.effectif","kpi.ENS.P.effectif"),
                                           multiple = TRUE)),
                  mainPanel(
                    plotOutput("serie", height = 2*fig.height)
                  ))),
                
                tabPanel("Carte", br(), sidebarLayout(
                  sidebarPanel(width=2,
                    tags$head(tags$style("#map{height:75vh !important;}")),
                    selectInput(inputId = "mapx", label = "Axe x", choices = kpiESR::kpiesr_shinycfg$kpi_list, selected = "kpi.K.resPetu"),
                    selectInput(inputId = "mapy", label = "Axe y", choices = kpiESR::kpiesr_shinycfg$kpi_list, selected = "kpi.K.ensPetu"),
                    selectInput(inputId = "maprentrée", label = "Rentrée", choices = c(2020))
                  ),
                  mainPanel(
                    plotOutput("map")
                  )))
                
    )
))
