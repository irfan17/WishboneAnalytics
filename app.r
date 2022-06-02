#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
bills <- read.csv('samplebills.csv')

#print(getwd())
#setwd("..//..//..//..//..//app")
#setwd('app')
#print(getwd())
source('HelperFunctions.r')
source('billcounter.r')
source('Ideology.r')

shiny::runApp('shiny',launch.browser = TRUE)