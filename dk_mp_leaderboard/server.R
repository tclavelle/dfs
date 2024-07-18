#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(glue)
library(purrr)
library(DT)
library(shiny)

# Define server logic for Draft Kings scoreboard app ----
server <- function(input, output) {
  
  # Reactive expression to load the weekly results
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  weekly_results <- reactive({
    
    season <- input$season
    
    # Get list of weekly result files
    weekly_result_files <- list.files(glue::glue('standings/{season}'), full.names = T)
    
    # function to process list of weekly results
    weekly_file_df <- function(week_result){
      df <- read_csv(week_result) %>% 
        select(rank = Rank, 
               team = EntryName, 
               points = Points) %>% 
        filter(!is.na(team)) %>% 
        mutate(week = as.numeric(str_extract(week_result, glue::glue("(?<=_| )[^_ ]+(?=\\_{season}.csv)")))) 
      return(df)
    }
    
    # Process all weekly files
    weekly_result_df <- map_dfr(weekly_result_files, weekly_file_df)
    
    return(weekly_result_df)
    
  })
  
  # Reactive expression to return full season summary standings
  season_summary <- reactive({
    
    weekly_result_df <- weekly_results()
    
    # Summarize season totals by player
    season_summary_df <- weekly_result_df %>% 
      group_by(team) %>% 
      summarize(weeks_played = n_distinct(week),
                total_points = round(sum(points, na.rm = T), digits = 2),
                avg_points = round(mean(points, na.rm = T), digits = 2),
                wins = sum(ifelse(rank == 1, 1, 0)),
                second = sum(ifelse(rank == 2 & week != 1, 1, 0)),
                avg_rank = mean(rank, na.rm = T),
                high_score = max(points, na.rm = T),
                min_score = min(points, na.rm = T)) %>% 
      arrange(desc(total_points))
    
    # Top 14 scores per player
    top_14 <- weekly_result_df %>% 
      group_by(team) %>% 
      mutate(score_rank = dense_rank(desc(points))) %>% 
      filter(score_rank <= 14) %>% 
      summarize(top_14 = sum(points)) %>% 
      arrange(desc(top_14))
    
    season_standings <- season_summary_df %>% 
      left_join(top_14)
    
    return(season_standings)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$week_plot <- renderPlot({
    
    week_totals <- weekly_results() %>% 
      filter(week == input$week)
    
    dk_cols <- c('#9ac434','#f46c22',rep.int("grey50", max(week_totals$rank) - 2))
    
    ggplot(data = week_totals,
           aes(x = fct_reorder(team, rank, desc), 
               y = points)) +
      geom_col(aes(fill = as.character(rank))) +
      geom_label(aes(label = points),
                 hjust = 1.2) +
      scale_fill_manual(
        values = dk_cols,
        breaks = as.character(c(1:max(week_totals$rank))),
        guide = FALSE
        ) +
      coord_flip() +
      labs(x = NULL,
           y = 'Score') +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text = element_text(color = '#f46c22'),
            axis.title.x = element_text(color = '#f46c22'))
  })
  
  output$season_plot <- renderPlot({
    
    season_df <- season_summary()
    
    dk_cols <- c('#9ac434','#f46c22',rep.int("grey50", nrow(season_df) - 2))
    
    ggplot(data = season_df,
           aes(x = fct_reorder(team, top_14), 
               y = total_points)) +
      geom_col(aes(fill = as.character(total_points))) +
      geom_label(aes(label = total_points),
                 hjust = 1.2) +
      scale_fill_manual(
        values = dk_cols,
        breaks = as.character(c(1:nrow(season_df))),
        guide = FALSE
      ) +
      coord_flip() +
      labs(x = NULL,
           y = 'Total score') +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.text = element_text(color = '#f46c22'),
            axis.title.x = element_text(color = '#f46c22'))
    
  })
  
  
}