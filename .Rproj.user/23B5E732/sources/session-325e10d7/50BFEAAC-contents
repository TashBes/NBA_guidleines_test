#####################################################################################
##
## Script name: graphs
##
## Purpose of script:Make functions to create all graphs needed for the NBA
##
## Author: Natasha Besseling, Lauryn Bull, Maphale
##
## Date Created: 2024-05-14
##
##
## Notes:
##
##
#####################################################################################
### packages & functions

require(tidyverse)
require(sf)


#####################################################################################
###
threat_status_plot <-function(DAT, X, Y, FILL )

{

  ggplot(DAT, aes(y = Y, x = X, fill = FILL)) +
    geom_bar(stat = "identity", width = 0.5) + ## change width of bars
    geom_text(aes(label = count), position = position_stack(vjust = 0.5), ## add count labels to the stacked bars and adjust the "vjust" value to place text at the beginning, centre or end of bars
              size = 3, color = "black", show.legend = FALSE) + ## adjust size of labels with no legend being shown
    scale_fill_manual(values = c("#b1d798", "#eeeea3", "#fff02a", "#f97835", "#e9302c")) +  ## order the colours of the bars in the reversed order
    ylab("Percent protected (%)") +
    xlab("") + ## remove the heading for the y-axis
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(fill = "Threat Status") + ## change the legend title here
    theme_minimal() + ## create a black bounding box around the plot
    theme(panel.grid = element_blank(),
          panel.border = element_rect(fill= NA),
          panel.grid.major.y = element_line(colour = "grey", size = 0.005))+
    theme(legend.position = "bottom") + ## position the legend to beneath the plot
    coord_flip()  ## flip the orientation of the chart
  thr_plot


}
#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)
