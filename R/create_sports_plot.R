#' Create Sports Plot
#'
#' This function creates a bar plot displaying the win ratios of sports teams.
#'
#' @param data A data.frame containing at least the columns wins, losses, and team.
#' @return A ggplot object representing the win ratio plot.
#' @export
create_sports_plot <- function(data) {
  # Check if win_ratio column exists, and if not, create it
  if (!"win_ratio" %in% names(data)) {
    data$win_ratio <- with(data, ifelse(wins + losses == 0, NA, wins / (wins + losses)))
  }

  # Create the plot using fully qualified calls and .data pronoun
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$team, y = .data$win_ratio, fill = .data$team)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Win Ratio of Sports Teams",
      x = "Team",
      y = "Win Ratio"
    ) +
    ggplot2::scale_fill_brewer(palette = "Pastel1") +
    ggplot2::theme(legend.position = "none")  # Remove legend for aesthetics

  return(p)
}
