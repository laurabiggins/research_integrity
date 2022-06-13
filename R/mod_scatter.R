library(Hmisc)

# type = "simple" or "paired_line"
mod_scatterplotUI <- function(id, menu=FALSE, plot_height=400){
  
  ns <- NS(id)
  
  if(menu == TRUE){
    tags <- tagList(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "options",
          radioButtons(ns("scatter_type"), label=NULL, choices = c("independent"="scatter", "paired", "differences")),
          actionButton(ns("browser"), "browser")
        ),
        mainPanel(plotOutput(outputId = ns("scatter")))
      )
    )
  } else {
    tags <- tagList(
      plotOutput(outputId = ns("scatter_no_menu"), height = plot_height),
      actionButton(ns("browser"), "browser")
    ) 
  }
}

mod_scatterplotServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
    output$scatter_no_menu <- renderPlot(simple_scatter_obj())
    
    output$scatter <- renderPlot({
      switch(input$scatter_type,
             "paired" = paired_line_obj(),
             "scatter" = simple_scatter_obj(),
             "differences" = differences()
      )
    })
    
    differences <- reactive({
      
      dataset() %>%
        pivot_wider() %>%
        ggplot(aes("",difference)) +
        geom_point(
          position = position_jitter(seed = 1, height = 0, width = 0.1), 
          shape = 21,
          size = 3,
          fill = "purple", 
          colour = "black",
          alpha = 0.7) +
        stat_summary(geom="errorbar", fun = "mean", fun.min="mean", fun.max="mean", width=0.4, size =1) +
        stat_summary(geom="errorbar", fun.data = "mean_cl_normal", width=0.15) 
      
    })
    
    # simple scatterplot with no options
    simple_scatter_obj <- reactive({
      
      dataset() %>%
        ggplot(aes(x=name, y=value)) +
        geom_point(
          position = position_jitter(seed = 1, height = 0, width = 0.2), 
          shape = 21,
          size = 3,
          fill = "purple", 
          colour = "black",
          alpha = 0.7) +
        xlab("") +
        stat_summary(geom="errorbar", fun = "mean", fun.min="mean", fun.max="mean", width=0.4, size =1)
    })
    
    paired_line_obj <- reactive({
      dataset() %>%
        ggplot(aes(x=name, y=value)) +#, color = Sample)) +
        geom_point(size = 4, fill = "purple", colour="black", shape=21) +
        geom_line(aes(group = Sample), size = 1,  colour="purple") +
        theme(legend.position="none")
    })
    
  })
}


