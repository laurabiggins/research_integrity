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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        uiOutput(outputId = "info_banner"),
        box(
          id = "boxplot_plotbox",
          width = 6,
          class = "plotbox",
          title = "box plot",
          collapsible = TRUE,
          sidebarLayout(
            sidebarPanel(
              width = 3,
              class = "options",
              checkboxInput("box_show_points", label="Show points"),
              checkboxInput("box_log_transform", label="Log10 transform"),
              checkboxInput("box_exclude_outliers", label="Exclude outliers")
            ),
            mainPanel(plotOutput(outputId = "boxplot"))
          )
        ),
        box(
          id = "barplot_plotbox",
          width = 6,
          class = "plotbox",
          title = "bar plot",
          collapsible = TRUE,
          sidebarLayout(
            sidebarPanel(
              width = 3,
              class = "options",
              checkboxInput("bar_show_points", label="Show points"),
              checkboxInput("bar_log_transform", label="Log10 transform"),
              checkboxInput("bar_show_errorbars", label="Show errorbars"),
              checkboxInput("bar_exclude_outliers", label="Exclude outliers"),
              radioButtons("bar_median_mean", label=NULL, choices = c("mean", "median"))
            ),
            mainPanel(plotOutput(outputId = "barplot"))
          )
        ),
        box(
          id = "violin_plotbox",
          width = 6,
          class = "plotbox",
          title = "violin plot",
          collapsible = TRUE,
          sidebarLayout(
            sidebarPanel(
              width = 3,
              class = "options",           
              checkboxInput("violin_show_points", label="Show points"),
              checkboxInput("violin_log_transform", label="Log10 transform"),
              checkboxInput("violin_exclude_outliers", label="Exclude outliers"),
              checkboxInput("violin_add_boxplot", label="Add boxplot")
              #actionButton("browser", "browser")
            ),
            mainPanel(plotOutput(outputId = "violinplot"))
          )          
        ),
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
  
  ## barplot functions ----
  
  bar_data <- reactive({
    if(input$bar_exclude_outliers){
      dataset <- filter(dataset, log10_outlier == FALSE)
    } 
    dataset %>%
      group_by(name) %>%
      summarise(
        mean_value = mean(value),
        mean_log10 = mean(log10_value),
        se = sd(value)/(sqrt(n())),
        se_log10 = sd(log10_value)/(sqrt(n()))
      ) %>%
      ungroup() %>%
      right_join(dataset)
    
  })

  
  barplot_base <- reactive({
    y_axis <- dplyr::if_else(input$bar_log_transform==TRUE, "log10_value", "value")
    y_mean <- dplyr::if_else(input$bar_log_transform==TRUE, "mean_log10", "mean_value")
    y_limit <- max((bar_data()[[y_mean]]+bar_data()$se)*1.1)
    
     p <-  bar_data() %>%
        ggplot(aes(x=name, y=.data[[y_axis]])) +
        stat_summary(geom="col", fun = {{input$bar_median_mean}}, fill="#3C6997", color="#F57200") 
  
      if(input$bar_show_errorbars) {
        
       # p <- p + geom_errorbar(aes(ymin=.data[[y_mean]]-se, ymax=.data[[y_mean]]+se)) 
      }
     p #+ ylim(c(0,y_limit))
   
  })
  
  barplot_obj <- reactive({
    
    if(input$bar_show_points == FALSE) return (barplot_base())
    barplot_base() +
      geom_jitter(height = 0, width = 0.3, colour = "blue")
  })
  
  output$barplot <- renderPlot(barplot_obj())
  
  
  
  ## boxplot functions ----
  
  box_data <- reactive({
    if(input$box_exclude_outliers){
      filter(dataset, log10_outlier == FALSE)
    } else dataset
  })
  
  boxplot_base <- reactive({
    y_axis <- dplyr::if_else(input$box_log_transform==TRUE, "log10_value", "value")
    
    box_data() %>%
      ggplot(aes(x=name, y=.data[[y_axis]])) +
      geom_boxplot(fill="#F57200", colour="#3C6997") +
      theme_light()
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
      geom_violin(fill = "#9FD356", alpha=0.7)
  })
  
  violin_obj <- reactive({
    p <- violin_base()
    if(input$violin_add_boxplot) {
      p <- p + geom_boxplot(fill="#F57200", colour="#3C6997", alpha = 0.5)
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