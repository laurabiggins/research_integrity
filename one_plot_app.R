library(shiny)
library(magrittr)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(RColorBrewer)


show_menu <- FALSE

ds1 <- readr::read_delim("data/test_data.txt") %>%
  tidyr::pivot_longer(cols=everything()) %>%
  mutate(log10_value = log10(value)) %>%
  group_by(name) %>%
  mutate(linear_outlier = rstatix::is_outlier(value)) %>%
  mutate(log10_outlier = rstatix::is_outlier(log10_value)) %>%
  ungroup()

ds2 <- readr::read_delim("data/ds2.csv") %>%
  tidyr::pivot_longer(cols=-1)

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
        br(),
        fluidRow(
          column(width = 6, offset = 3,
            box(
              id = "single_plot",
              width = 12,
              class = "plotbox",
              title = NULL,
              fluidRow(
                column(width = 6,
                  radioButtons(
                    "plot_type", 
                    label = NULL,
                    choices = list("bar", "box and whisker"="box", "violin", "scatter"),
                    inline = TRUE
                  )
                ),
                column(
                  width = 5, 
                  offset = 1, 
                  prettyRadioButtons(
                   "dataset_choice", 
                   label = NULL, 
                   list("Dataset 1"="ds1", "Dataset 2"="ds2"), 
                   inline = TRUE
                  )
                )
              ),
              br(),
              uiOutput("multiplot")
            )
          )
        ),
        actionButton("browser", "browser")
      )
    )
  )
)  

server <- function(input, output, session) {
  
  observeEvent(input$browser, browser())

  chosen_ds <- reactive({
    switch(input$dataset_choice, "ds1" = ds1, "ds2" = ds2)
  })
  
  ## banner text ----
  output$info_banner <- renderUI({
    bannertags <- tagList(
      p("Some brief instructions here?", class = "banner-text"),
    )
  }) 

  output$multiplot <- renderUI({
    switch(input$plot_type,
           "bar" =  mod_barplotUI("bar_panel", menu = show_menu),
           "box" = mod_boxplotUI("bp_panel", menu = show_menu),
           "violin" = mod_violinplotUI("violin_panel", menu = show_menu),
           "scatter" = mod_scatterplotUI("scatter_panel")
    )
  })
  
  mod_barplotServer("bar_panel", dataset=chosen_ds, menu=show_menu)
  mod_boxplotServer("bp_panel", dataset=chosen_ds, menu=show_menu)
  mod_violinplotServer("violin_panel", dataset=chosen_ds, menu=show_menu)
  mod_scatterplotServer("scatter_panel", dataset=chosen_ds)  
  
}

shinyApp(ui, server)






