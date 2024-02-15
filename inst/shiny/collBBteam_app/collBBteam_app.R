library(shiny)
library(dplyr)
library(stringr)

cbbga24 <- read.fwf("http://kenpom.com/cbbga24.txt",
                    widths = c(10, 24, 3, 24, 3, 2, 20),
                    strip.white=TRUE) |> 
  as_tibble() |>
  rename('Date' = V1,
         'Home_Team' = V2,
         'Home_Score' = V3,
         'Visiting_Team' = V4,
         'Visiting_Score' = V5,
         'Site_Type' = V6,
         'Game_Site' = V7) |>
  mutate(Margin = Home_Score - Visiting_Score ) |> 
  arrange(Home_Team) |> 
  select(!c(Site_Type, Game_Site)) 

all_teams_df <- tibble(Team = unique(c(cbbga24$Home_Team, cbbga24$Visiting_Team)))

## College Team App build
ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      selectInput('team_select', 'Select Home or Visiting Team:', choices = c('Home_Team', 'Visiting_Team')),
      uiOutput("team_search_ui")
    ),
    
    mainPanel(
    )
  )
)

server <- function(input, output, session) {
  
  output$team_search_ui <- renderUI({
    selectizeInput('team_search', "Search Team by Typing:", choices = NULL)
  })
  
  observe({
    team_column <- switch(input$team_select,
                          "Home_Team" = cbbga24$Home_Team,
                          "Visiting_Team" = cbbga24$Visiting_Team)
    
    updateSelectizeInput(session, 'team_search', choices = unique(team_column))
  })
  
}

shinyApp(server = server, ui = ui)
