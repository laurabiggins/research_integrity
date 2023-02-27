mod_densityplotUI <- function(id, menu = TRUE){
  
  ns <- NS(id)
  
  if(menu == TRUE){
    tags <- tagList(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "options",
          checkboxInput(ns("density_add_line"), label="Add line"),
          checkboxInput(ns("density_log_transform"), label="Log10 transform")#,
         # actionButton(ns("browser"), "browser")
        ),
        mainPanel(plotOutput(outputId = ns("densityplot")))
      )
    )
  } else {
    tags <- tagList(
      plotOutput(outputId = ns("densityplot_no_menu"))#,
      #actionButton(ns("browser"), "browser")
    ) 
  }
}

mod_densityplotServer <- function(id, dataset, menu) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())

    density_base <- reactive({
      values <- dplyr::if_else(input$density_log_transform==TRUE, "log10_value", "value")
      
      dataset() %>%
        ggplot(aes(.data[[values]])) +
        geom_histogram(aes(y=after_stat(density)), fill = "purple", colour="darkblue")
      
    })
    
    density_obj <- reactive({
      p <- density_base()
      if(input$density_add_line) p <- p + geom_density()
      p
    })
    
    output$densityplot <- renderPlot(density_obj())
    
    # simple plot with no options
    output$densityplot_no_menu <- renderPlot({
      dataset() %>%
        ggplot(aes(.data$value)) +
        geom_histogram(aes(y=after_stat(density)), fill = "purple", colour="darkblue")
      
    })
    
  })
}





