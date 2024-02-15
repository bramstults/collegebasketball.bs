#' Make a histogram of victory margins (win/loss)
#'
#' This will take in a tibble and return a tibble a colored histogram of the 
#' victory margins of games played.
#'
#' @param data This is the tibble of data from kenpom.com
#' @param divideBinsBy This allows user to reduce default number of bins, 
#' default argument is no division (1).
#'
#'
#' @return This function returns a tibble summarizing wins, losses, and a 
#' win percentage for all teams in a tibble 
#'
#' @examples
#' ## make a plotted histgram: 
#' a tibble with four columns
#' data <- tibble(
#'  team1 = rep(c("A", "B", "C", "D"), each = 4), 
#'  team2 = rep(c("A", "B", "C", "D"), times = 4),
#'  Margin = runif(16, min = -75, max = 75))
#'  
#'  plot <- marginHist(data)
#'  plot
#'  
#'  
#'
#' @import
#' viridis
#'
#' @export
#' 
#' 
#' 
marginHist <- function(data, divideBinsBy=1) {
  num_bins <- length(unique(cbbga24$Margin)) / divideBinsBy
  
  marginHist <- ggplot(data, aes(x = Margin, fill = factor(after_stat(count)))) +
    geom_histogram(bins = num_bins, color = 'grey25') +
    labs(x = "Margins", y = "Frequency", 
         title = 'Distribution of Victory Margins (win/loss)')+
    theme(axis.title=element_text(size = 14),
          plot.title =element_text(size = 14, face = "bold"))+
    scale_fill_viridis_d(direction = -1, option = "cividis")+
    guides(fill = 'none')
}
