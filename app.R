library(shiny)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(tidyverse)
library(RColorBrewer)

simple_plot_height <- 400

dataset <- readr::read_delim("data/test_data.txt") %>%
  tidyr::pivot_longer(cols=everything()) %>%
  mutate(log10_value = log10(value)) %>%
  group_by(name) %>%
  mutate(linear_outlier = rstatix::is_outlier(value)) %>%
  mutate(log10_outlier = rstatix::is_outlier(log10_value)) %>%
  ungroup()

box_wrapper <- function(box_id, box_title, panel_tags, collapsible = TRUE) {
  box(
    id = box_id,
    title = box_title,
    width = 6, 
    class = "plotbox",
    collapsible = collapsible,
    panel_tags)
}

ui <- tagList(
  fluidPage(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        tabsetPanel(
          tabPanel(
            title = "Part 1",
            box(
              id = "variplotbox",
              title = NULL,
              width = 8,
              #height = 10,
              class = "plotbox",
              collapsible = FALSE,
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  class = "options",
                  prettyRadioButtons(
                    "plot_choice",
                    label = NULL,
                    choices = c("barplot" = "barplot", "boxplot", "scatter", "violin"),
                    outline = TRUE 
                  )
                ),
                mainPanel(uiOutput("vari_plot"))
              )
            )
          ),
          tabPanel(
            title = "part2",
            uiOutput("info_banner"),
            uiOutput("multiplot"),
            uiOutput("barplot"),
            uiOutput("violinplot"),
            uiOutput("densityplot"),
            # leave this in for now as may want to reinstate at some point
            # box(
            #   width = 12,
            #   class = "plotbox",
            #   title = "data table",
            #   collapsible = TRUE,
            #   DT::dataTableOutput(outputId = "data_table")
            # )
          )
        ),
        #actionButton("browser", "browser")
      )
    )
  )
)  

server <- function(input, output, session) {
  
  observeEvent(input$browser, browser())
  
  ## banner text ----
  output$info_banner <- renderUI({
    bannertags <- tagList(
      p("Some brief instructions here?", class = "banner-text"),
    )
  }) 
  
  output$data_table <- DT::renderDataTable({
    dataset
  })
  

  output$vari_plot <- renderUI({

    if(input$plot_choice == "barplot"){
      variUI <- mod_barplotUI("vari_panel", menu = FALSE, simple_plot_height)
      mod_barplotServer("vari_panel", dataset=dataset, menu=FALSE)
    } else if(input$plot_choice == "boxplot"){
      variUI <- mod_boxplotUI("vari_panel", menu = FALSE, simple_plot_height)
      mod_boxplotServer("vari_panel", dataset=dataset, menu=FALSE)
    } else if(input$plot_choice == "scatter"){
      variUI <- mod_scatterplotUI("vari_panel", simple_plot_height)
      mod_scatterplotServer("vari_panel", dataset=dataset)
    } else if(input$plot_choice == "violin"){
      variUI <- mod_violinplotUI("vari_panel", menu = FALSE, simple_plot_height)
      mod_violinplotServer("vari_panel", dataset=dataset, menu=FALSE)
    }
    
    variUI
    
    # box(
    #   id = "variplotbox",
    #   title = NULL,
    #   width = 8, 
    #   class = "plotbox",
    #   collapsible = FALSE,
    #   variUI
    # )
    
  })
  
  
  ## barplot ----
  output$barplot <- renderUI({
    
    barplotUI <- mod_barplotUI("bar_panel", menu = TRUE)
    mod_barplotServer("bar_panel", dataset=dataset, menu=TRUE)
    box_wrapper(box_id="barplotbox", box_title="bar plot", barplotUI)
  })
  
  ## violinplot ----
  output$violinplot <- renderUI({
    
    violinplotUI <- mod_violinplotUI("violin_panel", menu = TRUE)
    mod_violinplotServer("violin_panel", dataset=dataset, menu=TRUE)
    box_wrapper(box_id="violinplotbox", box_title="violin plot", violinplotUI)
  })
  
  ## violinplot ----
  output$densityplot <- renderUI({
    
    densityplotUI <- mod_densityplotUI("density_panel", menu = TRUE)
    mod_densityplotServer("density_panel", dataset=dataset, menu=TRUE)
    box_wrapper(box_id="densityplotbox", box_title="density plot", densityplotUI)
  })
  
  
  ## multi/barplot ----
  output$multiplot <- renderUI({
    # switch for different plot types
    multiplotUI <- mod_boxplotUI("bp_panel", menu = TRUE)
    mod_boxplotServer("bp_panel", dataset=dataset, menu=TRUE)
    box_wrapper(box_id="plotbox", box_title="box and whisker plot", multiplotUI)
  })
  
  

}

shinyApp(ui, server)