mod_scatterplotUI <- function(id, plot_height=800){
  
  ns <- NS(id)
  
  tags <- tagList(
    plotOutput(outputId = ns("scatterplot"), height = plot_height),
    #uiOutput(ns("plot_panel"), class = "plot_box"),
    #actionButton(ns("browser"), "browser")
  ) 
}

mod_scatterplotServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    # simple boxplot with no options
    output$scatterplot <- renderPlot({
      dataset %>%
        ggplot(aes(x=name, y=value)) +
        geom_jitter(height = 0, width = 0.3, colour = "blue")
       # geom_boxplot(fill="#F57200", colour="#3C6997")
    })
  })
}

