ui <- dashboardPage(
  dashboardHeader(title = 'U.S. Congress Ideology'),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
           
      box(
        title = "Issue 1", width = 6,
        selectInput("issue1", "Select First Issue", choices = unique(bills$policy))
      ),
      box(
        title = "Issue 2", width = 6,
        selectInput("issue2", "Select Second Issue", choices = unique(bills$policy))
      ),
	  box(plotOutput("plot1", height = 500), width = 12)
    )  
  )
)