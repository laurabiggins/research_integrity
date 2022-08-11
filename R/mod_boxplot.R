mod_boxplotUI <- function(id, menu = TRUE, plot_height=400){
  
  ns <- NS(id)
  
  shinyjs::useShinyjs()
  
  if(menu == TRUE){
    tags <- tagList(
      shinyjs::useShinyjs(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "options",
          checkboxInput(ns("box_show_points"), label="Show points"),
          checkboxInput(ns("box_log_transform"), label="Log10 transform"),
          checkboxInput(ns("box_exclude_outliers"), label="Exclude outliers"),
          #if(paired==TRUE) 
          checkboxInput(ns("show_paired"), label="Show paired points"),
          #actionButton(ns("browser"), "browser")
        ),
        mainPanel(plotOutput(outputId = ns("boxplot")))
      )
    )
  } else {
    tags <- tagList(
      plotOutput(outputId = ns("boxplot_no_menu"), height = plot_height),
      #actionButton(ns("browser"), "browser")
    ) 
  }
}

mod_boxplotServer <- function(id, dataset, menu, paired) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())

    observe({
      if(paired() == FALSE) {
        # this doesn't work initially as the UI hasn't been initialised, and the code only reruns
        # if paired() is changed
        shinyjs::disable("show_paired")
        req(input$show_paired)
        if(input$show_paired == TRUE) {
          updateCheckboxInput(inputId = "show_paired", label="Show paired points", value = FALSE)
        }
      }
      else {
        shinyjs::enable("show_paired")
      }
    })
    
    # this is icky but I couldn't get it to work nicely - this runs once the UI has been initialised
    # this code will run more than it needs to 
    # there's an open issue here https://github.com/rstudio/shiny/issues/3348
    observeEvent(input$box_show_points, {

      if(paired() == FALSE) shinyjs::disable("show_paired")
      else shinyjs::enable("show_paired")

    })

     
    box_data <- reactive({
      if(input$box_exclude_outliers){
        dplyr::filter(dataset(), log10_outlier == FALSE)
      } else dataset()
    })

    boxplot_base <- reactive({
      y_axis <- dplyr::if_else(input$box_log_transform==TRUE, "log10_value", "value")
      outlier_shape <- dplyr::if_else(input$box_show_points, NA_integer_, as.integer(19))
      
      box_data() %>%
        ggplot(aes(x=name, y=.data[[y_axis]])) +
        geom_boxplot(fill="#F57200", colour="black", alpha = 0.7, outlier.shape=outlier_shape) +
        xlab("")
    })

    boxplot_obj <- reactive({

      p <- boxplot_base()
      if(input$box_show_points == TRUE) {
        p <- p +  
          geom_point(
            shape = 21, 
            colour = "black", 
            alpha = 0.7,
            fill = "#3C6997", 
            size = 3,
            position = position_jitter(seed = 1, height = 0, width = 0.3)
          )
      }
      if(is.null(input$show_paired)) return (p)
      if(paired() == TRUE) {
        if(input$show_paired == TRUE){
          p <- boxplot_base() +
            geom_point(size = 4, alpha = 0.7, fill = "#3C6997", colour="black", shape=21) +
            geom_line(aes(group = Sample), colour="#3C6997", linetype=2) +
            theme(legend.position="none")
        }
      }
      p
    })

    output$boxplot <- renderPlot(boxplot_obj())
    
    # simple boxplot with no options
    output$boxplot_no_menu <- renderPlot({
      dataset() %>%
        ggplot(aes(x=name, y=value)) +
        geom_boxplot(fill="#F57200", colour="black", alpha = 0.7) +
        xlab("")
    })
  })
}





