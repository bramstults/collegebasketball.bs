#' Extract a team's win/loss record and winning percentage for the games recorded in tibble
#'
#' This will take in a tibble and the string name of a given team and return a 
#' tibble with the win/loss record of that given team
#'
#' @param data This is the tibble of data from kenpom.com
#' @param team The string name of the team, i.e.:  team = 'Indiana State'
#' 
#' 
#'
#' @return This function returns a tibble summarizing wins, losses, and a win percentage
#'
#' @examples
#' ## filter game data 
#' a tibble with four columns
#' data <- tibble(
#'  team1 = rep(c("A", "B", "C", "D"), each = 4), 
#'  team2 = rep(c("A", "B", "C", "D"), times = 4),
#'  Margin = runif(16, min = -75, max = 75))
#'  
#'  team_Record(data, 'A')
#'  
#'
#' @import
#' tibble
#'
#' @export

team_Record <- function(data, team) {
  
  temp_Tibble <- data |> 
    mutate(Result = case_when(
      Home_Team == team & Margin > 0 ~ 1,
      Home_Team == team & Margin < 0 ~ 0,
      Visiting_Team == team & Margin < 0 ~ 1,
      Visiting_Team == team & Margin > 0 ~ 0)) |>
    mutate(WinLoss = case_when(
      Result == 1 ~ 'Win',
      Result == 0 ~ 'Loss'))
  
  #creating summary tibble
  result_Tibble <-  temp_Tibble |> 
    filter(Home_Team == team | Visiting_Team == team) |> 
    summarize(
      Team = team,
      Wins = sum(WinLoss == 'Win'),
      Losses = sum(WinLoss == 'Loss'),
      Percent_Win = round(Wins/(Wins+Losses)*100,2) %>% paste0("%"))
  
  return(result_Tibble)
}

