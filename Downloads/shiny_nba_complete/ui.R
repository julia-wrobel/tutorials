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
library(plotly)

# data management
load("nba_shots.RData")
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
      uiOutput("season_choice"),
      
      radioButtons("shots_made", label = h3("Shot status"), choices = list("all", "made", "missed"))
      
    
    ),
    
    # Show output based on user selections
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Spatial display", plotOutput("court_shots")),
                  tabPanel("Basket distance",  plotlyOutput("shot_distances")),
                  tabPanel("Court position", plotOutput("court_position")),
                  tabPanel("Time remaining", 
                           plotlyOutput("coupled1"),
                           plotOutput("coupled2")
                           ))
      
    )
  )
))
