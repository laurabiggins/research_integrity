mod_boxplotUI <- function(id, menu = TRUE, plot_height=400){
  
  ns <- NS(id)
  
  if(menu == TRUE){
    tags <- tagList(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "options",
          checkboxInput(ns("box_show_points"), label="Show points"),
          checkboxInput(ns("box_log_transform"), label="Log10 transform"),
          checkboxInput(ns("box_exclude_outliers"), label="Exclude outliers"),
          actionButton(ns("browser"), "browser")
        ),
        mainPanel(plotOutput(outputId = ns("boxplot")))
      )
    )
  } else {
    tags <- tagList(
      plotOutput(outputId = ns("boxplot_no_menu"), height = plot_height),
      #uiOutput(ns("plot_panel"), class = "plot_box"),
      #actionButton(ns("browser"), "browser")
    ) 
  }
}

mod_boxplotServer <- function(id, dataset, menu) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())

    box_data <- reactive({
      if(input$box_exclude_outliers){
        filter(dataset(), log10_outlier == FALSE)
      } else dataset()
    })

    boxplot_base <- reactive({
      y_axis <- dplyr::if_else(input$box_log_transform==TRUE, "log10_value", "value")
      outlier_shape <- dplyr::if_else(input$box_show_points, NA_integer_, as.integer(19))
      
      box_data() %>%
        ggplot(aes(x=name, y=.data[[y_axis]])) +
        geom_boxplot(fill="#F57200", colour="#3C6997", outlier.shape=outlier_shape) +
        theme_light()
    })

    boxplot_obj <- reactive({

      if(input$box_show_points == FALSE) return (boxplot_base())
      boxplot_base() +
        geom_jitter(height = 0, width = 0.3, colour = "#3C6997")
    })

    output$boxplot <- renderPlot(boxplot_obj())
    
    # simple boxplot with no options
    output$boxplot_no_menu <- renderPlot({
      dataset() %>%
        ggplot(aes(x=name, y=value)) +
        geom_boxplot(fill="#F57200", colour="#3C6997")
    })
  })
}





