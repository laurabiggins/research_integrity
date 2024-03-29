mod_violinplotUI <- function(id, menu = TRUE, plot_height=400){
  
  ns <- NS(id)
  
  if(menu == TRUE){
    tags <- tagList(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "options",
          checkboxInput(ns("show_points"), label="Show points"),
          checkboxInput(ns("log_transform"), label="Log10 transform"),
          checkboxInput(ns("exclude_outliers"), label="Exclude outliers"),
          checkboxInput(ns("add_boxplot"), label="Add boxplot")#,
          #actionButton(ns("browser"), "browser")
        ),
        mainPanel(plotOutput(outputId = ns("violinplot")))
      )
    )
  } else {
    tags <- tagList(
      plotOutput(outputId = ns("violinplot_no_menu"), height = plot_height)#,
      #actionButton(ns("browser"), "browser")
    ) 
  }
}

mod_violinplotServer <- function(id, dataset, menu) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    violin_data <- reactive({
      if(input$exclude_outliers){
        dplyr::filter(dataset(), log10_outlier == FALSE)
      } else dataset()
    })
    
    violin_base <- reactive({
      y_axis <- dplyr::if_else(input$log_transform==TRUE, "log10_value", "value")
      
      violin_data() %>%
        ggplot(aes(x=name, y=.data[[y_axis]])) +
        geom_violin(fill = "#9FD356", alpha=0.7, trim=FALSE) +
        xlab("")
    })
    
    violin_obj <- reactive({
      p <- violin_base()
      if(input$add_boxplot) {
        p <- p + geom_boxplot(fill="#3C6997", colour="black", alpha = 0.5, width = 0.4)
      }
      if(input$show_points) {
        p <- p +  
          geom_point(
            shape = 21, 
            colour = "black", 
            alpha = 0.7,
            fill = "#3C6997", 
            size = 3,
            position = position_jitter(seed = 1, height = 0, width = 0.2)
          )
      }
      p
    })
    
    output$violinplot <- renderPlot(violin_obj())

    output$violinplot_no_menu <- renderPlot({
      dataset() %>%
        ggplot(aes(x=name, y=value)) +
        geom_violin(fill = "#9FD356", alpha=0.7, trim=FALSE) +
        xlab("")
    })
    
  })
}





