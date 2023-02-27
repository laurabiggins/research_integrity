library(shiny)
library(magrittr)
library(shinydashboard)
library(ggplot2)

simple_plot_height <- 400

set.seed(1)

dataset <- readRDS("data/ds1.rds")
#dataset2 <- readRDS("data/ds2.rds")
dataset2 <- readRDS("data/ds2_before_after.rds")

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
        shinyWidgets::prettyRadioButtons(
          "dataset_choice", 
          label = NULL, 
          list("Dataset 1"="ds1", "Dataset 2"="ds2"), 
          inline = TRUE
        ),
        uiOutput("info_banner"),
        uiOutput("boxplot"),
        uiOutput("barplot"),
        uiOutput("violinplot"),
        uiOutput("density_or_scatter")#,
        #actionButton("browser", "browser")
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
      p("Create the most informative graph", class = "banner-text")
    )
  }) 
  
  ## boxplot ----
  output$boxplot <- renderUI({

      boxplotUI <- mod_boxplotUI("bp_panel", menu = TRUE)
      box_wrapper(box_id="boxplotbox", box_title="box and whisker plot", boxplotUI)
  })
  
  ## barplot ----
  output$barplot <- renderUI({
    
    barplotUI <- mod_barplotUI("bar_panel", menu = TRUE)
    box_wrapper(box_id="barplotbox", box_title="bar plot", barplotUI)
  })
  
  ## violinplot ----
  output$violinplot <- renderUI({
    
    violinplotUI <- mod_violinplotUI("violin_panel", menu = TRUE)
    box_wrapper(box_id="violinplotbox", box_title="violin plot", violinplotUI)
  })
  
  ## density or scatter plot ----
  output$density_or_scatter <- renderUI({
    
    if(input$dataset_choice == "ds1"){
      plotUI <- mod_densityplotUI("density_panel", menu = TRUE)
      title <- "density plot"
    } else {
      plotUI <- mod_scatterplotUI("paired_line", menu = TRUE)
      title <- "scatter"
    }
    
    box_wrapper(box_id="multiplotbox", box_title=title, plotUI)
  })

  boxplot_paired <- reactive({
    dplyr::if_else(input$dataset_choice == "ds2", TRUE, FALSE)
  })
  
  mod_boxplotServer("bp_panel", dataset=chosen_ds, menu=TRUE, paired=boxplot_paired)
  mod_barplotServer("bar_panel", dataset=chosen_ds, menu=TRUE)
  mod_violinplotServer("violin_panel", dataset=chosen_ds, menu=TRUE)
  mod_densityplotServer("density_panel", dataset=chosen_ds, menu=TRUE)
  mod_scatterplotServer("paired_line", dataset=chosen_ds) 
  
}

shinyApp(ui, server)