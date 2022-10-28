#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(kpiESR)
library(ggcpesrthemes)

source("tdbesr-plots.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    plots <- reactive({kpiESR::kpiesr_plot_all(rentrée, id = input$etab, groupe = input$groupe, style.k = k_style, style.o = o_style, style.o.norm = onorm_style)})
  
      output$kpi.norm <- renderPlot({ plots()$k.norm + rm_lt + rL_margin})
      output$kpi.evol <- renderPlot({ plots()$k.evol + rm_label + rL_margin})
      
      output$etu.abs <- renderPlot({ plots()$etu.abs + th_abs})
      output$etu.norm <- renderPlot({ plots()$etu.norm + th_norm })
      output$etu.evol <- renderPlot({ plots()$etu.evol + th_evol })

      output$bia.abs <- renderPlot({ plots()$bia.abs + th_abs })
      output$bia.norm <- renderPlot({ plots()$bia.norm + th_norm })
      output$bia.evol <- renderPlot({ plots()$bia.evol + th_evol })

      output$ens.abs <- renderPlot({ plots()$ens.abs + th_abs })
      output$ens.norm <- renderPlot({ plots()$ens.norm + th_norm })
      output$ens.evol <- renderPlot({ plots()$ens.evol + th_evol })
      
      output$fin.abs <- renderPlot({ plots()$fin.abs + th_abs })
      output$fin.norm <- renderPlot({ plots()$fin.norm + th_norm })
      output$fin.evol <- renderPlot({ plots()$fin.evol + th_evol })
      
                  
    choices_groupe <- reactive({
      choices <- kpiESR::esr.etab %>%
        select(name = Groupe, id = Groupe) %>%
        unique() %>%
        add_row(name ="Ensemble", id = "Ensemble", .before=1)
      res <- choices$id
      names(res) <- choices$name
      return(res)
    })

    choices_etab <- reactive({
      choices <- kpiESR::esr.etab %>%
        filter(Groupe == input$groupe) %>%
        select(name = Etablissement, id = pid) %>%
        unique() %>%
        add_row(name ="Ensemble", id = "Ensemble", .before=1)
      res <- choices$id
      names(res) <- choices$name
      return(res)
    })
    
    observe({
      updateSelectInput(session = session, inputId = "groupe", choices = choices_groupe())
    })
    
    observe({
      updateSelectInput(session = session, inputId = "etab", choices = choices_etab())
    })
})
