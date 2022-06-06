mod_barplotUI <- function(id, menu = TRUE, plot_height=800){
  
  ns <- NS(id)
  
  if(menu == TRUE){
    tags <- tagList(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "options",
          checkboxInput(ns("bar_show_points"), label="Show points"),
          checkboxInput(ns("bar_log_transform"), label="Log10 transform"),
          checkboxInput(ns("bar_show_errorbars"), label="Show errorbars"),
          checkboxInput(ns("bar_exclude_outliers"), label="Exclude outliers"),
          radioButtons(ns("bar_median_mean"), label=NULL, choices = c("mean", "median")),
          actionButton(ns("browser"), "browser")
        ),
        mainPanel(plotOutput(outputId = ns("barplot")))
      )
    )
  } else {
    tags <- tagList(
      plotOutput(outputId = ns("barplot_no_menu"), height = plot_height),
     # actionButton(ns("browser"), "browser")
    ) 
  }
}

# errorbars - show mean +- se
# median +- IQR

mod_barplotServer <- function(id, dataset, menu) {
  moduleServer(id, function(input, output, session) {
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())
    
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
          se_log10 = sd(log10_value)/(sqrt(n())),
          iqr = IQR(value),
          iqr_log10 = IQR(log10_value)
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
    
    # simple boxplot with no options
    # # this code should be simplified - we don't need to summarise this each time
    output$barplot_no_menu <- renderPlot({
      dataset %>%
        group_by(name) %>%
        summarise(
          mean_value = mean(value),
          se = sd(value)/(sqrt(n())),
        ) %>%
        ungroup() %>%
        right_join(dataset) %>%
        ggplot(aes(x=name, y=mean_value)) +
          stat_summary(geom="col", fun = mean, fill="#3C6997", color="#F57200") +
          geom_errorbar(aes(ymin=mean_value-se, ymax=mean_value+se), width=0.2, size=1) 
    })
    
  })
}





