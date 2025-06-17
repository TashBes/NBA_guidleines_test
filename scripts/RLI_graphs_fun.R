#####################################################################################
## Script name: RLI_graphs.R
##
## Purpose of script: To create the  Red List Index buffered line graphs
##
## Author: Shae-Lynn Hendricks
##
## Date Created: 2024-07-24
## Date updated: 2025-06-17
##
## Notes: Dataframe requires the Red List Index (RLI), year, minimum and
##        maximum values for each year and potentially the group for categorising the plot.
##
#####################################################################################
### Load libraries

library(dplyr)
library(ggplot2)
library(rlang)

RLI_plot <- function(DF, YEAR, RLI, min, max, GROUP = NULL, summarise_by_year = TRUE) {
  # Convert column names to symbols
  YEAR <- enquo(YEAR)
  RLI <- enquo(RLI)
  min <- enquo(min)
  max <- enquo(max)
  GROUP <- enquo(GROUP)

  # Optional summarisation
  if (summarise_by_year && rlang::quo_is_null(GROUP)) {
    DF <- DF %>%
      group_by(!!YEAR) %>%
      summarise(
        !!min := mean(!!min, na.rm = TRUE),
        !!max := mean(!!max, na.rm = TRUE),
        !!RLI := mean(!!RLI, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Plotting
  p <- ggplot(DF, aes(x = !!YEAR, y = !!RLI))

  if (rlang::quo_is_null(GROUP)) {
    p <- p +
      geom_line() +
      geom_ribbon(aes(ymin = !!min, ymax = !!max), alpha = 0.3, colour = NA)
  } else {
    p <- p +
      geom_line(aes(group = !!GROUP, color = !!GROUP), linetype = "dashed") +
      geom_ribbon(aes(ymin = !!min, ymax = !!max, group = !!GROUP), fill = "grey", alpha = 0.2, colour = NA)
  }

  p + theme_classic() + ylim(0.7, 1)
}


