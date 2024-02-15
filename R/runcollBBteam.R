#' College BB Team App
#' 
#' This function allows the college team shiny app to run.  This app offers users
#' a team-by-team summary of statistics, based on their entry, presented in a 
#' table with a supplementary plot.
#' 
#' @export

runcollBBTeam <- function() {
  appDir <- system.file("shiny", "collBBteam_app", package = "collegebasketball.bs")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `collegebasketball.bs`.", 
         call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}