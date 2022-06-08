library(shiny)
library(magrittr)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(RColorBrewer)


show_menu <- FALSE

dataset <- readr::read_delim("data/test_data.txt") %>%
  tidyr::pivot_longer(cols=everything()) %>%
  mutate(log10_value = log10(value)) %>%
  group_by(name) %>%
  mutate(linear_outlier = rstatix::is_outlier(value)) %>%
  mutate(log10_outlier = rstatix::is_outlier(log10_value)) %>%
  ungroup()

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
        uiOutput("info_banner"),
        fluidRow(
          column(width = 6, offset = 3,
            box(
              id = "single_plot",
              width = 12,
              class = "plotbox",
              title = "plot",
              radioButtons(
                "plot_type", 
                label = NULL,
                choices = list("bar", "box and whisker"="box", "violin", "density"),
                inline = TRUE
              ),
              uiOutput("multiplot")
            )
          )
        ),
        box(
          width = 12,
          class = "plotbox",
          title = "data table",
          collapsible = TRUE,
          DT::dataTableOutput(outputId = "data_table")
        ),
        actionButton("browser", "browser")
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
  
  output$data_table <- DT::renderDataTable(dataset)

  ## multi/barplot ----
  output$multiplot <- renderUI({
    switch(input$plot_type,
           "bar" =  mod_barplotUI("bar_panel", menu = show_menu),
           "box" = mod_boxplotUI("bp_panel", menu = show_menu),
           "violin" = mod_violinplotUI("violin_panel", menu = show_menu),
           "density" = mod_densityplotUI("density_panel", menu = show_menu)
    )
  })
  
  mod_barplotServer("bar_panel", dataset=dataset, menu=show_menu)
  mod_boxplotServer("bp_panel", dataset=dataset, menu=show_menu)
  mod_violinplotServer("violin_panel", dataset=dataset, menu=show_menu)
  mod_densityplotServer("density_panel", dataset=dataset, menu=show_menu)  
}

shinyApp(ui, server)