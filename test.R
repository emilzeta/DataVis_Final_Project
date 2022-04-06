library(plotly)
library(tidyverse)
library(shiny)
library(shinydashboard)

## Read in Data
players_df <- read_csv("Data/skaters.csv")
goalies_df <- read_csv("Data/goalies.csv")
teams_df <- read_csv("Data/teams.csv")
teamstats17_df <- read_csv("Data/teamstatstotals/teamstatstotals16-17.csv")
teamstats18_df <- read_csv("Data/teamstatstotals/teamstatstotals17-18.csv")
teamstats19_df <- read_csv("Data/teamstatstotals/teamstatstotals18-19.csv")
teamstats20_df <- read_csv("Data/teamstatstotals/teamstatstotals19-20.csv")
teamstats21_df <- read_csv("Data/teamstatstotals/teamstatstotals20-21.csv")
teamstats22_df <- read_csv("Data/teamstatstotals/teamstatstotals21-22.csv")

#######################################################################

  # Modifications

teamstats17_df <- teamstats17_df %>% mutate(season = '16/17')
teamstats18_df <- teamstats18_df %>% mutate(season = '17/18') 
teamstats19_df <- teamstats19_df %>% mutate(season = '18/19')
teamstats20_df <- teamstats20_df %>% mutate(season = '19/20')
teamstats21_df <- teamstats21_df %>% mutate(season = '20/21')
teamstats22_df <- teamstats22_df %>% mutate(season = '21/22')

teams_full <- rbind(teamstats17_df,
                    teamstats18_df,
                    teamstats19_df,
                    teamstats20_df, 
                    teamstats21_df,
                    teamstats22_df) %>%
  select(season, everything())

teams_full <- teams_full %>%
  group_by(season) %>%
  mutate(ff_rate = FF/TOI,
         cf_rate = CF/TOI,
         cf_total = sum(CF),
         total_TOI = sum(TOI),
         TOI_pg = (TOI/GP)*60,
         n_teams = n())

nhl_cf_rate <- teams_full %>%
  group_by(season) %>%
  summarize(cf_rate = cf_total/total_TOI) %>%
  ungroup() %>%
  group_by(season, cf_rate)

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
      menuItem("Players", tabName = "players", icon = icon("ice-skate"),
               menuSubItem("Top 10", tabName = "player10")),
      menuItem("Goalies", tabName = "goalies", icon = icon("hockey-mask"),
               menuSubItem("Top 10", tabName = "goalie10")),
      menuItem("Teams", tabName = "teams", icon = icon("hockey-sticks"),
               menuSubItem("Top 10", tabName = "team10")),
      menuItem("Empty Netters", tabName = "emptynet", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("player10",
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
      tabItem("goalie10",
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
      tabItem("team10",
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
              ),
      tabItem("emptynet",
              box(plotlyOutput(outputId = "figure1"), width = 12)
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
  
  ## Empty Net

  fig1 <- teams_full %>%
    highlight_key(., ~Team) %>%
    ggplot(aes(x = season, 
               y = cf_rate)) +
    geom_line(aes(group = Team), colour = "grey") +
    labs(x = "Season",
         y = "CF per minute") +
    geom_line(data = nhl_cf_rate, aes(x = season,
                                      y = cf_rate), 
              group = 1,
              colour = "yellow", 
              size = 1.1)
  
  s <- attrs_selected(
    line = list(color = "blue",
                width = 3))
  
  output$figure1 <- renderPlotly(
    ggplotly(fig1) %>%
    highlight(on = "plotly_hover",
              opacityDim = 1,
              selected = s)
  )
}

shinyApp(ui, server)