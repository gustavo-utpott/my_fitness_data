# script o run the app

library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(shinydashboardPlus)
library(shinyEffects)

# scripts for data wrangling, ui and server

source("data_wrangling.R", encoding = "UTF-8")
source("my_ui.R", encoding = "UTF-8")
source("my_server.R", encoding = "UTF-8")


##############################################################################################
# Aplicativo
##############################################################################################

shinyApp(
  ui = ui, 
  server = server
)





