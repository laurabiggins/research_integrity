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

dataset2 <- readr::read_delim("data/ds2.csv") %>%
  tidyr::pivot_longer(cols=-1) %>%
  mutate(log10_value = log10(value)) %>%
  mutate(linear_outlier = FALSE) %>%
  mutate(log10_outlier = FALSE)

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
        radioButtons(
          "dataset_choice", 
          label = NULL, 
          list("Dataset 1"="ds1", "Dataset 2"="ds2"), 
          inline = TRUE
        ),
        uiOutput("info_banner"),
        uiOutput("boxplot"),
        uiOutput("barplot"),
        uiOutput("violinplot"),
        uiOutput("densityplot"),
        actionButton("browser", "browser")
      )
    )
  )
)  

server <- function(input, output, session) {
  
  observeEvent(input$browser, browser())
  
  chosen_ds <- reactive({
    switch(input$dataset_choice, "ds1" = dataset, "ds2" = dataset2)
  })
  
  ## banner text ----
  output$info_banner <- renderUI({
    bannertags <- tagList(
      p("Some brief instructions here?", class = "banner-text")
    )
  }) 
  
  ## boxplot ----
  output$boxplot <- renderUI({
    
    boxplotUI <- mod_boxplotUI("bp_panel", menu = TRUE)
    mod_boxplotServer("bp_panel", dataset=chosen_ds, menu=TRUE)
    box_wrapper(box_id="plotbox", box_title="box and whisker plot", boxplotUI)
  })
  
  ## barplot ----
  output$barplot <- renderUI({
    
    barplotUI <- mod_barplotUI("bar_panel", menu = TRUE)
    mod_barplotServer("bar_panel", dataset=chosen_ds, menu=TRUE)
    box_wrapper(box_id="barplotbox", box_title="bar plot", barplotUI)
  })
  
  ## violinplot ----
  output$violinplot <- renderUI({
    
    violinplotUI <- mod_violinplotUI("violin_panel", menu = TRUE)
    mod_violinplotServer("violin_panel", dataset=chosen_ds, menu=TRUE)
    box_wrapper(box_id="violinplotbox", box_title="violin plot", violinplotUI)
  })
  
  ## densityplot ----
  output$densityplot <- renderUI({
    
    densityplotUI <- mod_densityplotUI("density_panel", menu = TRUE)
    mod_densityplotServer("density_panel", dataset=chosen_ds, menu=TRUE)
    box_wrapper(box_id="densityplotbox", box_title="density plot", densityplotUI)
  })
  
  ## densityplot ----
  output$scatterplot <- renderUI({
    
    scatterplotUI <- mod_scatterplotUI("scatter_panel")
    mod_scatterplotServer("scatter_panel", dataset=chosen_ds())  

    box_wrapper(box_id="scatterplotbox", box_title="scatter plot", scatterplotUI)
  })

}

shinyApp(ui, server)