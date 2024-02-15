#' Extract all teams' win/loss records and winning percentages for the games recorded in tibble
#'
#' This will take in a tibble and return a tibble with the win/loss record of 
#' all teams in the tibble.  This is a nested function requiring team_record.R.
#'
#' @param data This is the tibble of data from kenpom.com
#'
#'
#'
#' @return This function returns a tibble summarizing wins, losses, and a 
#' win percentage for all teams in a tibble 
#'
#' @examples
#' ## filter game data 
#' a tibble with four columns
#' data <- tibble(
#'  team1 = rep(c("A", "B", "C", "D"), each = 4), 
#'  team2 = rep(c("A", "B", "C", "D"), times = 4),
#'  Margin = runif(16, min = -75, max = 75))
#'  
#'  all_team_Record(data)
#'  
#'
#' @import
#' tibble
#'
#' @export
#' 
#' 
#' 
all_team_Record <- function(data) {
  
  
  unique_teams <- unique(c(data$Team_1, data$Team_2)) # vector of unique teams
  result_Tibble <- tibble() # tibble to store results
  
  for (team in unique_teams) {
    new_row <- team_Record(data, team)
    result_Tibble <- rbind(result_Tibble, new_row) #add_row() doesn't work here, IDKW
  }
  
  return(result_Tibble)
}

