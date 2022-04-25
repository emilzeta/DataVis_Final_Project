library(plotly)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(teamcolors)
library(ggpubr)

## Read in Data
players_22 <- read_csv("Data/skaters21-22.csv")
players_21 <- read_csv("Data/skaters20-21.csv")
players_20 <- read_csv("Data/skaters19-20.csv")
players_19 <- read_csv("Data/skaters18-19.csv")
players_18 <- read_csv("Data/skaters17-18.csv")

players_df <- rbind(players_22,
                    players_21,
                    players_20,
                    players_19,
                    players_18)

goalies_22 <- read_csv("Data/goalies21-22.csv")
goalies_21 <- read_csv("Data/goalies20-21.csv")
goalies_20 <- read_csv("Data/goalies19-20.csv")
goalies_19 <- read_csv("Data/goalies18-19.csv")
goalies_18 <- read_csv("Data/goalies17-18.csv")

goalies_df <- rbind(goalies_18,
                    goalies_19,
                    goalies_20,
                    goalies_21)

teams_17 <- read_csv("Data/teams19-20.csv")
teams_18 <- read_csv("Data/teams18-19.csv")
teams_19 <- read_csv("Data/teams19-20.csv")
teams_20 <- read_csv("Data/teams20-21.csv")
teams_21 <- read_csv("Data/teams21-22.csv")

teams_df <- rbind(teams_19,
                  teams_20,
                  teams_21)

teamstats17_df <- read_csv("Data/teamstatstotals/teamstatstotals16-17.csv")
teamstats18_df <- read_csv("Data/teamstatstotals/teamstatstotals17-18.csv")
teamstats19_df <- read_csv("Data/teamstatstotals/teamstatstotals18-19.csv")
teamstats20_df <- read_csv("Data/teamstatstotals/teamstatstotals19-20.csv")
teamstats21_df <- read_csv("Data/teamstatstotals/teamstatstotals20-21.csv")
teamstats22_df <- read_csv("Data/teamstatstotals/teamstatstotals21-22.csv")

#######################################################################

  # Modifications

players_22 <- players_22 %>% mutate(season = as.character(season))
players_21 <- players_21 %>% mutate(season = as.character(season))
players_20 <- players_20 %>% mutate(season = as.character(season))
players_19 <- players_19 %>% mutate(season = as.character(season))
players_18 <- players_18 %>% mutate(season = as.character(season))

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
         n_teams = n(),
         ppg = Points/GP)

nhl_cf_rate <- teams_full %>%
  group_by(season) %>%
  summarize(cf_rate = cf_total/total_TOI) %>%
  ungroup() %>%
  group_by(season, cf_rate)

players_df <- players_df %>%
  mutate(minutes_icetime = icetime/60,
         ice_per_game = minutes_icetime/games_played,
         bench_time = timeOnBench/60,
         bench_time_per_game = bench_time/games_played)


#######################################################################
player_varselect <- players_df %>% select_if(~is.numeric(.)) %>% select(-starts_with(c("playerID", "season", "icetime", "iceTimeRank", "minutes_icetime", "games_played", "timeOnBench"))) %>%
  select(-ends_with("AfterShifts"))
goalie_varselect <- goalies_df %>% select_if(~is.numeric(.)) %>% select(-starts_with(c("playerID", "season")))
team_varselect <- teams_df %>% select_if(~is.numeric(.)) %>% select(-starts_with(c("season", "games_played", "iceTime")))

var_choice_p <- names(player_varselect)
var_choice_g <- names(goalie_varselect)
var_choice_t <- names(team_varselect)

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
              fluidRow(
              box(selectizeInput("stat",
                                 label = "Choose a Statistic",
                                 choices = var_choice_p,
                                 selected = "ice_per_game"),
                  radioButtons("situation",
                               label = "Situation",
                               choices = levels(factor(players_df$situation)),
                               selected = "all",
                               inline = TRUE), height = 183),
              box(radioButtons("player_season",
                              label = "Select Season:",
                              choices = levels(factor(players_df$season)),
                              selected = '2018',
                              inline = TRUE),
                  sliderInput("players_gp", 
                              label = "Games Played",
                              min = 1,
                              max = 82,
                              value = 30)),
              box(plotlyOutput(outputId = "plot1"), width = 12),
              )),
      tabItem("goalie10",
              fluidRow(
              box(selectizeInput("goaliestat",
                                 label = "Choose a Statistic",
                                 choices = var_choice_g,
                                 selected = "highDangerGoals"),
                  radioButtons("sitgoalie",
                               label = "Situation",
                               choices = levels(factor(goalies_df$situation)),
                               selected = "all",
                               inline = TRUE), height = 183),
              box(radioButtons("goalies_season",
                               label = "Select Season:",
                               choices = levels(factor(goalies_df$season)),
                               selected = '2018',
                               inline = TRUE),
                  sliderInput("goalies_gp", 
                              label = "Games Played",
                              min = 1,
                              max = 67,
                              value = 30)),
              box(plotlyOutput(outputId = "plot2"), width = 12)
              )),
      tabItem("team10",
              fluidRow(
              box(selectizeInput("teamstat",
                                 label = "Choose a Statistic",
                                 choices = var_choice_t,
                                 selected = "goalsFor"), height = 82),
              box(radioButtons("sitteam",
                               label = "Situation",
                               choices = levels(factor(teams_df$situation)),
                               selected = "all",
                               inline = TRUE)),
              box(radioButtons("teams_season",
                               label = "Select Season:",
                               choices = levels(factor(teams_df$season)),
                               selected = '2019',
                               inline = TRUE)),
              box(plotlyOutput(outputId = "plot3"), width = 12),
              )),
      tabItem("emptynet",
              fluidRow(
              box(title = "Analysing and Visualizing Empty Net Shot Rates in the NHL",
                  br(),
                  "After watching NHL games throughout the 2021-2022 season, we hypothesized that teams more often than previous seasons were shooting for the empty net, seemingly not caring whether or not they missed for an icing. The question then became whether there has been a recent shift in how teams defend in a 6-on-5 situation. For estimating a teams amount of attempts we used Corsi for per minute against an open net (Corsi being defined as all blocked, missed and on target shots). All data was taken from naturalstattrick.com",
                  br(), br(), br(),
                  solidHeader = TRUE,
                  status = "primary",
                  plotlyOutput(outputId = "figure1"), width = 12,
                  br(), br(),
                  "Looking at both team and league averages and how they changed throughout the years, there was no significant overall difference in the last 5 years. The one main thing that was very visible was that teams that do good a particular year generally have a more attempts at the open net than those teams who do worse. This is most likely do to the fact that they spend time being up a goal or two at the end of the 3rd period. What is interesting is that there are a few teams that don't follow this general trend and there are teams who have a big shift in shot attempts at the open net from one particular year to the following. We think that coaching style may play a big part in this.",
                  br(), br()),
              box(title = "Single Season Visualizations",
                  br(),
                  "Below we can see two plots visualizing the difference between seasons. In the lolipop chart to the left we can see that the teams that shoot the most at the open net change from season to season and that the teams on top are usually very strong that season. To the right we see the realtionship between points and shot attempts per minute. Noticable is that R keeps staying positive throughout all of the seasons, suggesting that there is a positive relationship between shot attempts per minute and a team's points per game.",
                  br(), br(),
                  solidHeader = TRUE,
                  status = "primary",
                  radioButtons("en_season",
                               label = "Select Season:",
                               choices = levels(factor(teams_full$season)),
                               selected = '20/21',
                               inline = TRUE), width = 12),
              box(plotOutput(outputId = "en_hist")),
              box(plotOutput(outputId = "en_point"))
              ))
    )
  )
)

server <- function(input, output, session) {
  
  ## Players
  
  top_10_react <- reactive({
    players_df %>% 
      filter(situation == input$situation,
             season == input$player_season,
             games_played <= input$players_gp) %>%
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
      filter(situation == input$sitgoalie,
             season == input$goalies_season,
             games_played <= input$goalies_gp) %>%
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
      filter(situation == input$sitteam,
             season == input$teams_season) %>%
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
         y = "Shot Attempts per Minute") +
    geom_line(data = nhl_cf_rate, aes(x = season,
                                      y = cf_rate), 
              group = 1,
              colour = "black", 
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
  
  en_react <- reactive({
    teams_full %>%
      filter(season == input$en_season) %>%
      mutate(Team = fct_reorder(.f = Team, .x = cf_rate))
  })
  
  output$en_hist <- renderPlot({
    ggplot(data = en_react(), aes(x = Team,
                                  y = cf_rate)) +
      geom_point() +
      geom_segment(aes(x = Team, xend = Team, y = 0, yend = cf_rate)) +
      labs(x = "", y = "Shot Attempts on the Empty Net per Minute") +
      coord_flip()
  })
  
  output$en_point <- renderPlot({
    teams_full %>% 
      filter(season == input$en_season) %>%
      ggplot(aes(x = cf_rate, y = ppg)) +
      geom_point() +
      geom_smooth(method = lm) +
      labs(x = "Shot Attempts on the Empty Net per Minute", y = "Team Points per Game") +
      stat_cor(method = "pearson", aes(label = ..r.label..))
  })
}

shinyApp(ui, server)