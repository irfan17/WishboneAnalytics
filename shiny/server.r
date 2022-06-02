server <- function(input, output) {
  set.seed(123)
    
  output$plot1 <- renderPlot({
  ideology_plot(input$issue1, input$issue2)
  })

}