#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# data management
players = nba_shots %>% distinct(player_name) %>% pull()
made = nba_shots %>% distinct(shot_made_flag ) %>% pull()

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("NBA Shot Attempts"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # drop down menu for player
      selectInput("player_choice", label = h3("Select player"),
                  choices = players, selected = "LeBron James"),
      
      # drop down menu for season based on a certain player
      uiOutput("season_choice")
    
    ),
    
    # Show output based on user selections
    mainPanel(
      # spatial plot of shots made
      plotOutput("court_shots")
    )
  )
))
