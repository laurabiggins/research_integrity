library(shiny)
library(shiny)
library(magrittr)
library(shinydashboard)
library(plotly)
library(DT)
library(tidyverse)
library(RColorBrewer)


dataset <- readr::read_delim("data/test_data.txt") %>%
  tidyr::pivot_longer(cols=everything()) %>%
  mutate(log10_value = log10(value)) %>%
  group_by(name) %>%
  mutate(linear_outlier = rstatix::is_outlier(value)) %>%
  mutate(log10_outlier = rstatix::is_outlier(log10_value)) %>%
  ungroup()

box_wrapper <- function(box_id, box_title, panel_tags) {
  box(
    id = box_id,
    title = box_title,
    width = 6, 
    class = "plotbox",
    collapsible = TRUE,
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
        uiOutput(outputId = "info_banner"),
        uiOutput("multiplot"),
        uiOutput("barplot"),
        uiOutput("violinplot"),
        box(
          id = "density_plotbox",
          width = 6,
          class = "plotbox",
          title = "density plot",
          collapsible = TRUE,
          sidebarLayout(
            sidebarPanel(
              class = "options",
              width = 3,
              checkboxInput("density_add_line", label="Add line"),
              checkboxInput("density_log_transform", label="Log10 transform"),
              checkboxInput("density_exclude_outliers", label="Exclude outliers"),
            ),
            mainPanel(plotOutput(outputId = "densityplot"))
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
  
  output$data_table <- DT::renderDataTable({
    dataset
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
  
  
  ## multi/barplot ----
  output$multiplot <- renderUI({
    # switch for different plot types
    multiplotUI <- mod_boxplotUI("bp_panel", menu = TRUE)
    mod_boxplotServer("bp_panel", dataset=dataset, menu=TRUE)
    box_wrapper(box_id="plotbox", box_title="box and whisker plot", multiplotUI)
  })
  
  ## density functions ----
  density_data <- reactive({
    if(input$density_exclude_outliers){
      filter(dataset, log10_outlier == FALSE)
    } else dataset
  })
  
  density_base <- reactive({
    values <- dplyr::if_else(input$density_log_transform==TRUE, "log10_value", "value")
    
    dataset %>%
      ggplot(aes(.data[[values]])) +
      geom_histogram(aes(y=..density..), fill = "purple", colour="darkblue")
    
  })
  
  density_obj <- reactive({
    p <- density_base()
    if(input$density_add_line) p <- p + geom_density()
    p
  })
  
  output$densityplot <- renderPlot(density_obj())

}

shinyApp(ui, server)