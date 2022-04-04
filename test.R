library(plotly)
library(tidyverse)
library(shiny)
library(shinydashboard)

## Read in Data
players_df <- read_csv("Data/skaters.csv")
goalies_df <- read_csv("Data/goalies.csv")
teams_df <- read_csv("Data/teams.csv")


#######################################################################

  # Modifications

players_df <- players_df %>%
  mutate(minutes_icetime = icetime/60,
         ice_per_game = minutes_icetime/games_played)


#######################################################################

var_choice <- names(players_df)
var_choice_g <- names(goalies_df)
var_choice_t <- names(teams_df)

#######################################################################

    # APP

ui <- dashboardPage(
  dashboardHeader(title = "Hockey"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Players", tabName = "players", icon = icon("ice-skate")),
      menuItem("Goalies", tabName = "goalies", icon = icon("hockey-mask")),
      menuItem("Teams", tabName = "teams", icon = icon("hockey-sticks"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("players",
              box(selectizeInput("stat",
                                 label = "Choose a Statistic",
                                 choices = var_choice,
                                 selected = "ice_per_game"), 
                  height = 82
              ),
              box(radioButtons("situation",
                               label = "Situation",
                               choices = levels(factor(players_df$situation)),
                               selected = "all",
                               inline = TRUE
              )
              ),
              box(plotlyOutput(outputId = "plot1"), width = 12),
      ),
      tabItem("goalies",
              box(selectizeInput("goaliestat",
                                 label = "Choose a Statistic",
                                 choices = var_choice,
                                 selected = "games_played"),
                  height = 82
              ),
              box(radioButtons("sitgoalie",
                               label = "Situation",
                               choices = levels(factor(goalies_df$situation)),
                               selected = "all",
                               inline = TRUE
              )
              ),
              box(plotlyOutput(outputId = "plot2"), width = 12)
      ),
      tabItem("teams",
              box(selectizeInput("teamstat",
                                 label = "Choose a Statistic",
                                 choices = var_choice_t,
                                 selected = "goalsFor"),
                  height = 82
              ),
              box(radioButtons("sitteam",
                               label = "Situation",
                               choices = levels(factor(teams_df$situation)),
                               selected = "all",
                               inline = TRUE)
              ),
              box(plotlyOutput(outputId = "plot3"), width = 12)
      )
    )
  )
)

server <- function(input, output, session) {
  
  ## Players
  
  top_10_react <- reactive({
    players_df %>% 
      filter(situation == input$situation) %>%
      arrange(desc(.data[[input$stat]])) %>%
      slice(1:10) %>%
      mutate(name = fct_reorder(name, .data[[input$stat]]))
  })
  
  output$plot1 <- renderPlotly({
    p1 <- ggplot(data = top_10_react(), aes(y = name, 
                                            x = .data[[input$stat]],
                                            text = .data[[input$stat]])) +
      geom_point(colour = "blue",
                 size = 2.5) +
      labs(y = "")
    
    ggplotly(p1,
             tooltip = "text",
             displayModeBar = FALSE)
  })
  
  ## Goalies
  
  top_10_goalies <- reactive({
    goalies_df %>% 
      filter(situation == input$sitgoalie) %>%
      arrange(desc(.data[[input$goaliestat]])) %>%
      slice(1:10) %>%
      mutate(name = fct_reorder(name, .data[[input$goaliestat]]))
  })
  
  output$plot2 <- renderPlotly({
    p2 <- ggplot(data = top_10_goalies(), aes(y = name, 
                                              x = .data[[input$goaliestat]],
                                              text = .data[[input$goaliestat]])) +
      geom_point(colour = "blue",
                 size = 2.5) +
      labs(y = "")
    
    ggplotly(p2,
             tooltip = "text",
             displayModeBar = FALSE)
  })
  
  ## Teams
  
  top_10_teams <- reactive({
    teams_df %>% 
      filter(situation == input$sitteam) %>%
      arrange(desc(.data[[input$teamstat]])) %>%
      slice(1:10) %>%
      mutate(name = fct_reorder(name, .data[[input$teamstat]]))
  })
  
  output$plot3 <- renderPlotly({
    p3 <- ggplot(data = top_10_teams(), aes(y = name, 
                                            x = .data[[input$teamstat]],
                                            text = .data[[input$teamstat]])) +
      geom_point(colour = "blue",
                 size = 2.5) +
      labs(y = "")
    
    ggplotly(p3,
             tooltip = "text",
             displayModeBar = FALSE)
  })
  
}

shinyApp(ui, server)