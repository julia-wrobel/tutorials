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
   
  ## set range of seasons based on player choice
  output$season_choice <- renderUI({
    seasons = nba_shots %>% filter(player_name == input$player_choice) %>% 
      distinct(season) %>% pull()
    
    selectizeInput("season_choice", label = "Select season", choices = seasons,
                selected = seasons[1], multiple = TRUE)
  })
  
  ## subset data by selected player using reactive expression
  player_data = reactive({
    filter(nba_shots, player_name == input$player_choice)
  })
  
  ## first tab plot
  output$court_shots <- renderPlot({
    gg_court + geom_point(data = filter(player_data(), season %in% input$season_choice),
                          alpha = 0.75, size = 2.5,
                          aes(loc_x, loc_y, color = shot_made_flag, shape = season)) +
      scale_color_manual("", values = c(made = "blue", missed = "orange"))
  })
  
  ## second tab plot
  output$shot_distances <- renderPlotly({
    nba_shots %>%
      filter(if(input$shots_made != 'all')  (shot_made_flag == input$shots_made) else TRUE) %>%
      plot_ly(y = ~shot_distance, color = ~player_name, type = "box") %>%
      layout(showlegend = FALSE)
  })
  

  ## third tab plot
  output$court_position <- renderPlot({
    # subset data by selected player and season(s)
    nba_subset = player_data() %>%
      select(player_name, season, shot_zone_area, shot_made_flag, shot_attempted_flag, shot_type, action_type) %>%
      filter(shot_zone_area != "Back Court(BC)") %>%
      group_by(player_name, season) %>%
      mutate(zone = ifelse(shot_zone_area %in% c("Right Side(R)", "Right Side Center(RC)"), "Right Side",
                           ifelse(shot_zone_area %in% c("Center(C)") , "Center", "Left Side")),
             total_season_shots = n()) %>%
      ungroup() %>%
      group_by(player_name, season, zone) %>%
      summarize(total_season_shots = first(total_season_shots), attempts_percent = n()/total_season_shots) %>%
      ungroup() 
    
      ggplot(nba_subset , aes(season, attempts_percent, group = zone, color = zone)) + 
        theme_bw() + geom_line(lwd = 2) + ylab("percent of shot attempts") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              legend.position = "bottom")
  })
  
  ## fourth tab plot 1
  output$coupled1 <- renderPlotly({
    player_data() %>%
      filter(period < 5) %>% # exclude overtime play
      filter(if(input$shots_made != 'all')  (shot_made_flag == input$shots_made) else TRUE) %>%
      # add plot
      plot_ly(mode = "markers", type = "scatter", showlegend = FALSE,
              source = "time_plot", key = ~shot_id) %>% 
      layout(
        xaxis = list(title = "distance from basket (feet)"),
        yaxis = list(title = "time remaining (minutes)"),
        dragmode = "select"
      ) %>%
      add_trace(x = ~shot_distance, y = ~ time_remaining,  
                marker = list(
                  color = 'rgba(245, 137, 15, 0.1)',
                  size = 10,
                  line = list(
                    color = 'rgb(4, 2, 0)',
                    width = 0.5
                  ))) %>%
      add_segments(x = 0, xend = 100, y = 12, yend = 12) %>%
      add_segments(x = 0, xend = 100, y = 24, yend = 24) %>%
      add_segments(x = 0, xend = 100, y = 36, yend = 36) %>%
      add_segments(x = 0, xend = 100, y = 48, yend = 48) %>%
      add_annotations(x = c(80, 80, 80, 80),
                      y = c(12, 24, 36, 48),
                      text = c("quarter 4 start", "quarter 3 start", "quarter 2 start", "game start"))
    
  })
  
  
  output$coupled2 <- renderPlot({
    selected <- event_data("plotly_selected", source = "time_plot")
    
    player_data_subset = filter(player_data(), shot_id %in% selected$key)
    
    gg_court + geom_point(data = player_data_subset,
                          alpha = 0.75, size = 2.5,
                          aes(loc_x, loc_y, color = shot_made_flag)) +
      scale_color_manual("", values = c(made = "blue", missed = "orange"))
    
    
  })
  
})
