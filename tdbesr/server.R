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
fig.res = 96


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #o_style_react <- reactive( { })
  
    plots <- reactive({kpiESR::kpiesr_plot_all(rentrée, id = input$etab, groupe = input$groupe, 
                                               experimental = TRUE,
                                               style.k = if(input$light) k_style_light else k_style, 
                                               style.o =if(input$light) o_style_light else o_style,
                                               style.o.norm = onorm_style)}) %>%
      bindCache(rentrée, input$etab, input$groupe)
  
    output$kpi.norm <- renderPlot({ plots()$k.norm + rm_lt + rL_margin + ggtitle("Indicateurs clés de performance")}, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$kpi.evol <- renderPlot({ plots()$k.evol + th_evol + ggtitle("Evolutions") + cpesr_cap() }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    
    output$etu.abs <- renderPlot({ plots()$etu.abs + th_abs + ggtitle("Valeurs absolues")}, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$etu.norm <- renderPlot({ plots()$etu.norm + th_norm + ggtitle("Valeurs normalisées") + theme_cpesr_cap() }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$etu.evol <- renderPlot({ plots()$etu.evol + th_evol + ggtitle("Evolutions") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)

    output$bia.abs <- renderPlot({ plots()$bia.abs + th_abs + ggtitle("Valeurs absolues") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$bia.norm <- renderPlot({ plots()$bia.norm + th_norm + ggtitle("Valeurs normalisées") + theme_cpesr_cap() }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$bia.evol <- renderPlot({ plots()$bia.evol + th_evol + ggtitle("Evolutions") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)

    output$ens.abs <- renderPlot({ plots()$ens.abs + th_abs + ggtitle("Valeurs absolues") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$ens.norm <- renderPlot({ plots()$ens.norm + th_norm + ggtitle("Valeurs normalisées") + theme_cpesr_cap() }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$ens.evol <- renderPlot({ plots()$ens.evol + th_evol + ggtitle("Evolutions") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    
    output$fin.abs <- renderPlot({ plots()$fin.abs + th_abs + ggtitle("Valeurs absolues") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$fin.norm <- renderPlot({ plots()$fin.norm + th_norm + ggtitle("Valeurs normalisées") + theme_cpesr_cap() }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$fin.evol <- renderPlot({ plots()$fin.evol + th_evol + ggtitle("Evolutions") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)

    output$imo.abs <- renderPlot({ plots()$imo.abs + th_abs + ggtitle("Valeurs absolues") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$imo.norm <- renderPlot({ plots()$imo.norm + th_norm + ggtitle("Valeurs normalisées") + theme_cpesr_cap() }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    output$imo.evol <- renderPlot({ plots()$imo.evol + th_evol + ggtitle("Evolutions") }, res= fig.res) %>%
      bindCache(rentrée, input$etab, input$groupe)
    
    output$serie <- renderPlot(res= fig.res, { 
      kpiESR::kpiesr_plot_line(id = input$etab, vars = input$serie_vars, val = input$serie_val) + theme_cpesr_cap()
    })
    
    output$map <- renderPlot(res = fig.res, { 
      kpiESR::kpiesr_plot_map(input$maprentrée, id = input$etab, groupe = input$groupe, 
                              xvar = input$mapx, yvar = input$mapy ) +
        scale_x_continuous(name=kpiesr_lfc_desc[[input$mapx]], labels=kpiesr_lfc_y_labels[[input$mapx]] ) +
        scale_y_continuous(name=kpiesr_lfc_desc[[input$mapy]], labels=kpiesr_lfc_y_labels[[input$mapy]]) + 
        theme_cpesr_cap()})
    
    choices_rentrée_map <- reactive({
      r <- kpiESR::esr %>%
        transmute(
          Rentrée,
          x = eval(parse(text = input$mapx)),
          y = eval(parse(text = input$mapy)),
        ) %>%
        na.omit() %>%
        arrange(desc(Rentrée)) %>%
        pull(Rentrée) 
    })
    
    observe({
       updateSelectInput(session = session, inputId = "maprentrée", choices = choices_rentrée_map())
    })
    
    observe({
      updateSelectInput(session = session, inputId = "etab", choices = kpiesr_shinycfg$etabs[[input$groupe]])
    })
})
