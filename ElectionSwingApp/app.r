library(shiny)
library(shinydashboard)

#setwd("..//..//..//..//..//app")
#setwd('app')
#print(getwd())
source('ElectionSwingApp.r')

ui <- dashboardPage(
  dashboardHeader(title = 'Presidential Election Swings'),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 350), width = 9),
      
      box(
        title = "First Year",
        selectInput("year1", "Select Year", choices = c(2000,2004,2008,2012,2016,2020)),width = 3
      ),
      box(
        title = "Second Year",
        selectInput("year2", "Select Year", choices = c(2000,2004,2008,2012,2016,2020)), width = 3
      ),
        selectInput(
		inputId = 'States',
		label = 'States',
		choices = unique(comb$state),
		multiple = TRUE
		)  
    )  
  )
)


server <- function(input, output) {
  set.seed(123)

  output$plot1 <- renderPlot({
  swing_chart(input$year1, input$year2, input$States)
  })

}

#Run the application 
shinyApp(ui = ui, server = server)