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

ui <- tagList(
  fluidPage(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    # shinyalert::useShinyalert(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        uiOutput(outputId = "info_banner"),
        box(
          width = 6,
          class = "plotbox",
          title = "box plot",
          collapsible = TRUE,
          sidebarLayout(
            sidebarPanel(width = 3,
              checkboxInput("box_show_points", label="Show points"),
              checkboxInput("box_log_transform", label="Log10 transform"),
              checkboxInput("box_exclude_outliers", label="Exclude outliers"),
              actionButton("browser", "browser")
            ),
            mainPanel(plotOutput(outputId = "boxplot")),
            
          )
        ),
        box(
          width = 6,
          class = "plotbox",
          title = "violin plot",
          collapsible = TRUE,
          sidebarLayout(
            sidebarPanel(width = 3,
              checkboxInput("violin_show_points", label="Show points"),
              checkboxInput("violin_log_transform", label="Log10 transform"),
              checkboxInput("violin_exclude_outliers", label="Exclude outliers"),
              checkboxInput("violin_add_boxplot", label="Add boxplot"),
              actionButton("browser", "browser")
            ),
            mainPanel(plotOutput(outputId = "violinplot"))
          )          
        ),
        box(
          width = 6,
          class = "plotbox",
          title = "density plot",
          collapsible = TRUE,
          sidebarLayout(
            sidebarPanel(
              width = 3,
              checkboxInput("density_add_line", label="Add line"),
              checkboxInput("density_log_transform", label="Log10 transform"),
              checkboxInput("density_exclude_outliers", label="Exclude outliers"),
            ),
            mainPanel(plotOutput(outputId = "densityplot"))
          )
        ),
        box(
          width = 6,
          class = "plotbox",
          title = "data table",
          collapsible = TRUE,
          DT::dataTableOutput(outputId = "data_table")
        ),
      )
    )
  )
)  

server <- function(input, output, session) {
  
  observeEvent(input$browser, browser())
  
  ## boxplot functions ----
  
  box_data <- reactive({
    if(input$box_exclude_outliers){
      filter(dataset, log10_outlier == FALSE)
    } else dataset
  })
  
  output$data_table <- DT::renderDataTable({
    dataset
  })
  
  boxplot_base <- reactive({
    y_axis <- dplyr::if_else(input$box_log_transform==TRUE, "log10_value", "value")
    
    box_data() %>%
      ggplot(aes(x=name, y=.data[[y_axis]])) +
      geom_boxplot()
  })
  
  boxplot_obj <- reactive({
    
    if(input$box_show_points == FALSE) return (boxplot_base())
    boxplot_base() +
      geom_jitter(height = 0, width = 0.3, colour = "blue")
  })
  
  output$boxplot <- renderPlot(boxplot_obj())

  ## violinplot functions ----
  
  violin_data <- reactive({
    if(input$violin_exclude_outliers){
      filter(dataset, log10_outlier == FALSE)
    } else dataset
  })
  
  violin_base <- reactive({
    y_axis <- dplyr::if_else(input$violin_log_transform==TRUE, "log10_value", "value")
    
    violin_data() %>%
      ggplot(aes(x=name, y=.data[[y_axis]])) +
      geom_violin()
  })
  
  violin_obj <- reactive({
    p <- violin_base()
    if(input$violin_add_boxplot) {
      p <- p + geom_boxplot()
    }
    if(input$violin_show_points) {
      p <- p + geom_jitter(height = 0, width = 0.3, colour = "blue")
    }
    p
  })
   
  output$violinplot <- renderPlot(violin_obj())

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