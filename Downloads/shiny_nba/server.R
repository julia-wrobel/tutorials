#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
library(tidyverse)
library(plotly)

# load data and source functions to make plot of basketball court
source("helpers.R")
load("nba_shots.RData")

# define plot of court
gg_court = make_court()

################################################################################
# Define server logic 
shinyServer(function(input, output) {
   
  # set range of seasons based on player choice
  output$season_choice <- renderUI({
    seasons = nba_shots %>% filter(player_name == input$player_choice) %>% 
      distinct(season) %>% pull()
    
    selectizeInput("season_choice", label = "Select season", choices = seasons,
                selected = seasons[1], multiple = TRUE)
  })
  
  
})
