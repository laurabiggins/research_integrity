library(shiny)
library(magrittr)
library(shinydashboard)
library(ggplot2)

show_menu <- FALSE

ds1 <- readRDS("data/ds1.rds")
ds2 <- readRDS("data/ds2.rds")


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
        br(),
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
                column(width = 7,
                  shinyWidgets::prettyRadioButtons(
                    "plot_type", 
                    label = NULL,
                    choices = list("bar", "box and whisker"="box", "violin", "scatter"),
                    inline = TRUE
                  )
                ),
                column(
                  width = 5, 
                  shinyWidgets::prettyRadioButtons(
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
        )#,
        #actionButton("browser", "browser")
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
      p("Choose the most informative representation between 4 possible plots", class = "banner-text"),
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
  mod_boxplotServer("bp_panel", dataset=chosen_ds, menu=show_menu, paired=reactive(FALSE))
  mod_violinplotServer("violin_panel", dataset=chosen_ds, menu=show_menu)
  mod_scatterplotServer("scatter_panel", dataset=chosen_ds)  
  
}

shinyApp(ui, server)






