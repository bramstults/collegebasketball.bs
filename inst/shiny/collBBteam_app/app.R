library(shiny)
library(dplyr)
library(stringr)
library(collegeBasketball.bs)

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
      #selectInput('team_select', 'Select Home or Visiting Team:', choices = c('Home_Team', 'Visiting_Team')),
      
      uiOutput("team_search_ui"),
      actionButton("confirm_selection", "Please Click to Confirm")
      
    ),
    
    mainPanel(
      tableOutput('teamRecord'),
      plotOutput('thePlot'),
      
      tags$head(
        tags$style(
          '.top-text {
            position: absolute;
            top: 10%; 
            left: 50%;
            transform: translateX(-50%);
          }'
        )
      ),
      div(class = "top-text", h3(textOutput('plot_Unmatched'))
      )
      
      
    )
  
  )
)

server <- function(input, output, session) {
  
  output$team_search_ui <- renderUI({
    selectizeInput('team_search', "Search Team by Typing:", choices = NULL)
  })
  
  observe({
    updateSelectizeInput(session, 'team_search', choices = all_teams_df$Team)
  })
  
  
  output$teamRecord <- function(){ 
    if (input$team_search %in% all_teams_df[['Team']]) {
    req(input$confirm_selection)
    collegeBasketball.bs::team_Record(cbbga24, input$team_search) |> 
      knitr::kable(format = "html") |>
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
    }
  }
  
  
  output$thePlot <- renderPlot({
    
    if (input$team_search %in% all_teams_df[['Team']]) {
      req(input$confirm_selection)
      
      # wins and losses
      result <- collegeBasketball.bs::team_Record(cbbga24, input$team_search)
      wins <- result$Wins
      losses <- result$Losses
      
      # bar plot
      ggplot(data = data.frame(Result = c("Wins", "Losses"), Count = c(wins, losses)), 
             aes(x = Result, y = Count)) +
        geom_bar(stat = "identity", fill = c("slategray3", "slategray")) +
        labs(x = "Outcome", y = "Count", title = "Wins vs Losses")+
        theme(axis.title = element_text(size = 14),
              legend.title = element_text(size = 14),
              plot.title = element_text(size = 18, face = "bold"),
              panel.background = element_rect(fill = 'ivory', 
                                              color = 'navajowhite3', size=2,
                                              linetype = "solid")) 
    } else {
      
        output$plot_Unmatched <- renderText({
          if ( !(input$team_search %in% all_teams_df[['Team']]) ) {
            paste("You have not typed an available team's name, 
              please type a few letters of the teams name and select from list")
          }
        })
      
      }
  })
  
  
}

shinyApp(server = server, ui = ui)

