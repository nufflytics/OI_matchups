#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(stringr)
library(future)
plan(multiprocess)

library(nufflytics)
key <- readRDS("data/api.key")

pretty_skills <- function(skill) {
  str_replace_all(skill, c(
    "IncreaseStrength" = "+ST",
    "IncreaseAgility" = "+AG",
    "IncreaseMovement" = "+MA",
    "IncreaseArmour" = "+AV",
    "([a-z])([A-Z])" = "\\1 \\2"
  )
  )
}


imgify <- function(skill_vec) {
  if(!is.null(skill_vec) & all(skill_vec != "BH")) {
    return(
      glue::glue_data(
        list(skill = skill_vec), 
        "<img class = 'skillimg' src='img/skills/{skill}.png' title='{pretty_skills(skill)}' />"
      ) %>% 
        glue::glue_collapse() %>% 
        as.character()
    )
  }
  ""
}

convert_player_to_row <- function(player) {
  p = list(
    #id = player$id,
    num = player$number,
    Name = player$name,
    Position = str_replace_all(player$type, c(".*_" = "", "([a-z])([A-Z])"="\\1 \\2")),
    Level = player$level,
    SPP = player$xp,
    TV = player$value,
    Injuries = glue::collapse(player$casualties_state %>% map_chr(state_to_casualty) %>% map_chr(imgify)),
    Skills = glue::collapse(player$skills %>% map_chr(imgify))
  )
  
  if(is_empty(p$Injuries)) p$Injuries <- ""
  if(is_empty(p$Skills)) p$Skills <- ""
  
  if(player$suspended_next_match) {p$Injuries <- paste0(p$Injuries, "<img class = 'skillimg' src='img/skills/MNG.png' title='MNG' />")}
  
  as_data_frame(p)
}

#home_teams <- readRDS("data/saved_home_teams.rds")
#away_teams <- readRDS("data/saved_away_teams.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    #tags$script(src="https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/ladder.css"),
    tags$link(rel="stylesheet", type="text/css", href = "https://cdn.jsdelivr.net/npm/animate.css@3.5.2/animate.min.css"),
    tags$style("
               @import url('https://fonts.googleapis.com/css?family=Alegreya+SC:800|Open+Sans:600');
               ")
    ),
  
  #textOutput("home_team"),
  uiOutput("splash")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  matchups <- read_csv("data/r2_matches.csv", trim_ws = F)
  
  team_index = reactiveVal(0)
  
  h <- eventReactive(team_index(), {
    validate(need(team_index(), message = F))
    
    #future({
      api_team(key, matchups$Team[team_index()])
      #home_teams[[team_index()]]
    #})
  })
  
  a <-  eventReactive(team_index(), {
    validate(need(team_index(), message = F))
    
    #future({
      api_team(key, matchups$Team2[team_index()])
      #away_teams[[team_index()]]
    #})
  })
  
  new_team <- reactiveTimer(30000)
  
  observe({
    new_team()
    
    if (isolate(team_index() <= nrow(matchups))) {
      team_index(isolate(team_index())+1)
    } 
    
    })
  
  output$splash <- renderUI({
    h <- h()
    a <- a()
    
    tagList(
      # div(class = "identifier",
      #     fluidRow(
      #       column(12,
      #              img(class = "vs",src="http://nufflytics.com/img/main/REBBL.png", width = 250),
      #              p(class = "text-center division", glue::glue("Match {team_index()}"))
      #       )
      #     )
      # ),
      div(class = "matchup",
          fluidRow(column(12, p(class = "division", glue::glue("Match #{team_index()}")))),
          fluidRow(
            column(class = "home_team", 5,img( class = "pull-right", height = "150px", src=glue::glue("http://images.bb2.cyanide-studio.com/logos/Logo_{h$team$logo}.png"))),
            column(2),
            column(class = "away_team",5, img(height = "150px", src=glue::glue("http://images.bb2.cyanide-studio.com/logos/Logo_{a$team$logo}.png")))
          ),
          fluidRow(
            column(class = "home_team", 5, div(class = "text-right", p(class="team", h$team$name), p(class="coach", h$coach$name), p(class = "coach", id_to_race(h$team$idraces)), p(class="motto",h$team$leitmotiv))),
            column(2,  img(class = "central_col", src="img/BigIconVS.png")),
            column(class = "away_team", 5, div(p(class = "team", a$team$name),  p(class = "coach", a$coach$name), p(class = "coach", id_to_race(a$team$idraces)), p(class = "motto", a$team$leitmotiv)))
          ),
          column(class = "home_team", 5, tableOutput("home_roster")),
          column(2),
          column(class = "away_team", 5, tableOutput("away_roster"))
      )
    )

  })
  
  output$home_roster <- renderTable(h()$roster %>% map_df(convert_player_to_row) %>% arrange(num) %>% select(-num), sanitize.text.function = function(x) x)
  output$away_roster <- renderTable(a()$roster %>% map_df(convert_player_to_row) %>% arrange(num) %>% select(-num), sanitize.text.function = function(x) x)  
}

# Run the application 
shinyApp(ui = ui, server = server)

