#' Make a histogram of victory margins (win/loss)
#'
#' This will take in a tibble and return a tibble a colored histogram of the 
#' victory margins of games played.
#'
#' @param data This is the tibble of data from kenpom.com
#' @param months This allows user to specify the numeric int months in the order 
#' they wish them to appear in the box plot:  a vector argument, default is 
#' c(11,12,1,2), the months of the basketball season.
#'
#'
#' @return This function returns a boxplot showing the ranges of margins over the
#' months of the basketball season.
#' 
#' @examples
#' ## make a boxplot
#' a tibble with four columns
#' data <- tibble(
#'  team1 = rep(c("A", "B", "C", "D"), each = 4), 
#'  team2 = rep(c("A", "B", "C", "D"), times = 4),
#'  Margin = runif(16, min = -75, max = 75))
#'  
#'  plot <- marginMonthlyRange(data)
#'  plot
#'  
#'  
#'
#' @import
#' viridis
#' ggplot2
#'
#' @export
#' 
#' 

marginMonthlyRange <- function(data, months = c(11,12,1,2) ){
data <- data |>
mutate(Date = mdy(Date)) |>
  mutate(Month = month(Date)) |>   #only run once, thank you...
  mutate(Month = factor(Month, levels = months))
         
plot <- ggplot(data, aes(x = Month, y = Margin, fill = Month)) +
  geom_boxplot() +
  labs(title = "HOME TEAMS: Monthly Margin of Victory Distribution",
       x = "Month",
       y = "Margin of Win/Loss") +
  theme(axis.title=element_text(size = 14),
        plot.title =element_text(size = 14, face = "bold"))
return(plot)
}
