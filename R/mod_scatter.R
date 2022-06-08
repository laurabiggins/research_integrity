# type = "simple" or "paired_line"
mod_scatterplotUI <- function(id, type = "simple", plot_height=400){
  
  ns <- NS(id)
  
  tags <- tagList(
    switch(type,
           "paired_line" = plotOutput(outputId = ns("paired_line"), height = plot_height),
           "scatterplot" = plotOutput(outputId = ns("scatterplot"), height = plot_height),
           plotOutput(outputId = ns("scatterplot"), height = plot_height)
    ),
    actionButton(ns("browser"), "browser")
  ) 
}

mod_scatterplotServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    # simple scatterplot with no options
    output$scatterplot <- renderPlot({
      dataset() %>%
        ggplot(aes(x=name, y=value)) +
        geom_jitter(height = 0, width = 0.3, colour = "purple")
    })
    
    output$paired_line <- renderPlot({
      dataset() %>%
        ggplot(aes(x=name, y=value, color = Sample)) +
        geom_point(size = 5) +
        geom_line(aes(group = Sample), size = 2) +
        theme(legend.position="none")
    })
    
  })
}


