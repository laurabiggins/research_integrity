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
        fluidRow(
          column(width = 6, offset = 3,
            box(
              id = "single_plot",
              width = 12,
              class = "plotbox",
              title = NULL,
              radioButtons(
                "plot_type", 
                label = NULL,
                choices = list("bar", "box and whisker"="box", "violin", "scatter"),
                inline = TRUE
              ),
              # tabsetPanel(
              #   tabPanel("Dataset 1",
              #            uiOutput("multiplot")
              #            ),
              #   tabPanel("Dataset 2", uiOutput("multiplot2"))
              # )
              uiOutput("multiplot")
            )
          )
        ),
        fluidRow(
          column(width = 6, offset = 3,
                 box(
                   id = "single_plot2",
                   width = 12,
                   class = "plotbox",
                   title = "Dataset 2",
                   radioButtons(
                     "plot2_type",
                     label = NULL,
                     choices = list("bar", "box and whisker"="box", "violin", "scatter"),
                     inline = TRUE
                   ),
                   uiOutput("multiplot2")
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
  
  # these need to be reactive to work with the modules
  dataset <- reactive(ds1)
  dataset2 <- reactive(ds2)
  
  ## banner text ----
  output$info_banner <- renderUI({
    bannertags <- tagList(
      p("Some brief instructions here?", class = "banner-text"),
    )
  }) 

  ## Plot for dataset 1 ----
  output$multiplot <- renderUI({
    switch(input$plot_type,
           "bar" =  mod_barplotUI("bar_panel", menu = show_menu),
           "box" = mod_boxplotUI("bp_panel", menu = show_menu),
           "violin" = mod_violinplotUI("violin_panel", menu = show_menu),
           "scatter" = mod_scatterplotUI("scatter_panel")
    )
  })
  
  mod_barplotServer("bar_panel", dataset=dataset, menu=show_menu)
  mod_boxplotServer("bp_panel", dataset=dataset, menu=show_menu)
  mod_violinplotServer("violin_panel", dataset=dataset, menu=show_menu)
  mod_scatterplotServer("scatter_panel", dataset=dataset)  
  
  ## Plot for dataset 2 ----
  output$multiplot2 <- renderUI({
    switch(input$plot2_type,
           "bar" =  mod_barplotUI("bar_panel2", menu = show_menu),
           "box" = mod_boxplotUI("bp_panel2", menu = show_menu),
           "violin" = mod_violinplotUI("violin_panel2", menu = show_menu),
           "scatter" = mod_scatterplotUI("scatter_panel2")
    )
  })
  
  mod_barplotServer("bar_panel2", dataset=dataset2, menu=show_menu)
  mod_boxplotServer("bp_panel2", dataset=dataset2, menu=show_menu)
  mod_violinplotServer("violin_panel2", dataset=dataset2, menu=show_menu)
  mod_scatterplotServer("scatter_panel2", dataset=dataset2)  
  
}

shinyApp(ui, server)






