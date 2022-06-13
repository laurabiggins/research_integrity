mod_barplotUI <- function(id, menu = TRUE, plot_height=400){
  
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
          radioButtons(ns("bar_median_mean"), label=NULL, choices = c("mean", "median"))#,
          #actionButton(ns("browser"), "browser")
        ),
        mainPanel(plotOutput(outputId = ns("barplot")))
      )
    )
  } else {
    tags <- tagList(
      plotOutput(outputId = ns("barplot_no_menu"), height = plot_height)#,
      #actionButton(ns("browser"), "browser")
    ) 
  }
}

# errorbars - show mean +- se
# median +- IQR

mod_barplotServer <- function(id, dataset, menu) {
  moduleServer(id, function(input, output, session) {
    
    set.seed(1)
    
    ns_server <- NS(id)
    
    observeEvent(input$browser, browser())

    filtered_data <- reactive({
      if(input$bar_exclude_outliers){
        dplyr::filter(dataset(), log10_outlier == FALSE)
      } else dataset()
    })
    
    bar_data <- reactive({
      
      filtered_data() %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(
          mean_value = mean(value),
          mean_log10 = mean(log10_value),
          median_value = median(value),
          median_log10 = median(log10_value),
          se = sd(value)/(sqrt(dplyr::n())),
          se_log10 = sd(log10_value)/(sqrt(dplyr::n())),
          iqr_low = fivenum(value)[2],
          iqr_high = fivenum(value)[4],
          iqr_log10_low = fivenum(log10_value)[2],
          iqr_log10_high = fivenum(log10_value)[4]
        ) 
    })
    
    y_val <- reactive({
      
      if(input$bar_log_transform==TRUE){
        switch(input$bar_median_mean, "mean" = "mean_log10", "median" = "median_log10")
      } else{
        switch(input$bar_median_mean, "mean" = "mean_value", "median" = "median_value")
      }
    })
    
    se_error_val <- reactive({
      dplyr::if_else(input$bar_log_transform, "se_log10", "se")
    })

    iqr_low <- reactive({
      dplyr::if_else(input$bar_log_transform, "iqr_log10_low", "iqr_low")
    })

    iqr_high <- reactive({
      dplyr::if_else(input$bar_log_transform, "iqr_log10_high", "iqr_high")
    })
    
    barplot_base <- reactive({

     # y_limit <- max((bar_data()[[y_val()]]+bar_data()[[y_val()]])*1.1)
      
      p <-  bar_data() %>%
        ggplot(aes(x=name, y=.data[[y_val()]])) +
          geom_col(fill="#3C6997", color="#F57200")

      if(input$bar_show_errorbars) {
        if(input$bar_median_mean == "mean"){
          p <- p + geom_errorbar(
            aes(
              ymin=.data[[y_val()]]-.data[[se_error_val()]], 
              ymax=.data[[y_val()]]+.data[[se_error_val()]]
            ), 
            width=0.2, 
            size=1
          )
        } else {
          p <- p + geom_errorbar(
            aes(ymin=.data[[iqr_low()]], ymax=.data[[iqr_high()]]), 
            width=0.2, 
            size=1
          )
        }
      }

      p + xlab("")#+ ylim(c(0,y_limit))
      
    })
    
    barplot_obj <- reactive({
      
      if(input$bar_show_points == FALSE) return (barplot_base())
      
      y_values <- dplyr::if_else(input$bar_log_transform, "log10_value", "value")
     
      barplot_base() +
        geom_point(
          position = position_jitter(seed = 1, height = 0, width = 0.3),
          data=filtered_data(), 
          aes(x=name, y=.data[[y_values]]), 
          colour = "#F57200",
          size = 3,
          alpha = 0.7)
    })
    
    output$barplot <- renderPlot(barplot_obj())
    
    # simple boxplot with no options
    # # this code should be simplified - we don't need to summarise this each time
    output$barplot_no_menu <- renderPlot({
      dataset() %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(
          mean_value = mean(value),
          se = sd(value)/(sqrt(dplyr::n())),
        ) %>%
        dplyr::ungroup() %>%
        dplyr::right_join(dataset()) %>%
        ggplot(aes(x=name, y=mean_value)) +
          stat_summary(geom="col", fun = mean, fill="#3C6997", color="#F57200") +
          geom_errorbar(aes(ymin=mean_value-se, ymax=mean_value+se), width=0.2, size=1) +
          xlab("")
    })
    
  })
}





