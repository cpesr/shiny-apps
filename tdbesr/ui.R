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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel("Tableaux de bord de l'ESR"),
    fluidRow(
      column(2,selectInput(inputId = "groupe", label = "Groupe", choices = NULL)),
      column(2,selectInput(inputId = "etab", label = "Etablissement", choices = NULL))
    ),
    
    tabsetPanel(type = "tabs",
                tabPanel("KPI", br(), fluidRow(plotOutput("kpi.norm", height = fig.height),plotOutput("kpi.evol", height = fig.height))),
                
                tabPanel("Etudiants", br(), fluidRow(
                  column(6,plotOutput("etu.abs", height = fig.height),plotOutput("etu.evol", height = fig.height)),
                  column(6,plotOutput("etu.norm", height = 2*fig.height)))),
                
                tabPanel("BIATSS", br(), fluidRow(
                  column(6,plotOutput("bia.abs", height = fig.height),plotOutput("bia.evol", height = fig.height)),
                  column(6,plotOutput("bia.norm", height = 2*fig.height)))),
                
                tabPanel("Enseignants", br(), fluidRow(
                  column(6,plotOutput("ens.abs", height = fig.height),plotOutput("ens.evol", height = fig.height)),
                  column(6,plotOutput("ens.norm", height = 2*fig.height)))),
                
                tabPanel("Finances", br(), fluidRow(
                  column(6,plotOutput("fin.abs", height = fig.height),plotOutput("fin.evol", height = fig.height)),
                  column(6,plotOutput("fin.norm", height = 2*fig.height))))
    )
))
