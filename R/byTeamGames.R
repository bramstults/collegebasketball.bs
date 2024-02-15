#' Identify all games where a given team is a player (whether home team or visiting)
#'
#' This will take in a tibble and the string name of a given team and return a 
#' tibble with all games played by that team.
#'
#' @param data This is the tibble of data from kenpom.com
#' @param team The string name of the team, i.e.:  team = 'Indiana State'
#' @param ungroup If TRUE will undo some other previous grouping 
#' 
#' 
#'
#' @return This function returns tibble containing rows of all home or visiting 
#' games for a given team.
#'
#' @examples
#' ## filter game data 
#' a tibble with four columns
#' data <- tibble(
#'  team1 = rep(c("A", "B", "C", "D"), each = 4), 
#'  score1 = runif(16, min = 0, max = 100),        
#'  team2 = rep(c("A", "B", "C", "D"), times = 4),
#'  score2 = runif(16, min = 0, max = 100))
#'  
#'  byTeamGames(data, 'A')
#'  
#'
#' @import
#' tibble
#'
#' @export

byTeamGames <- function(data, team, ungroup){
  newTibble <- data |> 
    rowwise() |> 
    filter(any(c(Team_1, Team_2) == team)) |>
    ungroup() #if there is some other (non-lubridate parsed) formatting
  return(newTibble)
}


