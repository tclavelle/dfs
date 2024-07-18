#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Meatpoles DraftKings Leaderboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select the random distribution type ----
      selectInput('season',
                  'Select NFL season:',
                  choices = c(2022:2023),
                  selected = 2023),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Select the random distribution type ----
      selectInput('week',
                  'Select week:',
                  choices = c(1:8),
                  selected = 8),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Week Results", plotOutput("week_plot")),
                  tabPanel("Season Standings", plotOutput("season_plot"))
                  # tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)
