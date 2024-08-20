#####################################################################################
##
## Script name: Andrew graphs
##
## Purpose of script:make functions to create the graphs for andrews data he sent us.
##
## Author: Natasha Besseling
##
## Date Created: 2024-08-19
##
##
## Notes:
##
##
#####################################################################################
### packages & functions

require(tidyverse)
library(readxl)
library(NBA.package)

# source("functions/packages.R")       # loads up all the packages we need
#devtools::install_github("TashBes/NBA.package")
#####################################################################################
### functions

test <-function(DF, X, Y, FILL, COUNT){

  cols <- c("#e9302c", "#f97835", "#fff02a", "#eeeea3", "#b1d798")
  breaks <- c( "Critically Endangered", "Endangered","Vulnerable","Near Threatened","Least Concern")


  dat <- DF %>%
    mutate(FILL = factor({{FILL}}, levels = breaks))

  ggplot2::ggplot(dat, aes(y = {{Y}}, x = {{X}}, fill = FILL)) +
    ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
    ggplot2::geom_text(aes(label = {{COUNT}}), position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                       size = 3, color = "black", show.legend = FALSE) + # adjust size of labels with no legend being shown
    ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
    ggplot2::ylab("Percentage of ecosystem functional types") +
    ggplot2::xlab("") + ## remove the heading for the y-axis
    ggplot2::guides(fill = guide_legend(reverse = F, nrow = 1, size = 0.5)) +  # display legend in 2 rows
    ggplot2::labs(fill = "") + ## change the legend title here
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", # position legend to the bottom
                   panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                   axis.line = element_blank(), # remove all x-axis grid lines
                   panel.grid.major.y = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                   legend.text = element_text(size = 8), # change legend text size
                   plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                   plot.margin = margin(10, 10, 10, 10)) +   # extend plot margins to accommodate the border)
    ggplot2::coord_flip()  # flip the orientation of the chart
}

test.1 <-function(DF, X, Y, FILL, COUNT) {

  cols <- c("#466a31","#80a952","#d5dec3","#a4a3a3")
  breaks <- c("Well Protected","Moderately Protected","Poorly Protected","No Protection")

  dat <- DF %>%
    mutate(FILL = factor({{FILL}}, levels = breaks))

  ggplot2::ggplot(dat, aes(y = {{Y}}, x = {{X}}, fill = FILL)) +
    ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + ## change width of bars
    ggplot2::geom_text(aes(label = {{COUNT}}), position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                       size = 3, color = "black", show.legend = FALSE) + # adjust size of labels with no legend being shown
    ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
    ggplot2::ylab("Percentage of ecosystem functional types") +
    ggplot2::xlab("") +
    ggplot2::guides(fill = guide_legend(reverse = F, nrow = 1, size = 0.5)) +  # display legend in 2 rows
    ggplot2::labs(fill = "") + # change the legend title
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0, 50, 100)) + # set the y-axis to show 0%, 50%, and 100%
    ggplot2::theme_minimal() + # create a black bounding box around the plot
    ggplot2::theme(legend.position = "bottom", # position legend to the bottom
                   panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                   axis.line = element_blank(), # remove all x-axis grid lines from
                   panel.grid.major.y = element_blank(), # include the horizontal grid line on 1st , 3rd and 5 ... x-axis
                   legend.text = element_text(size = 8),
                   plot.background = element_rect(color = "black", fill = NA),  # add black border around the entire plot
                   plot.margin = margin(10, 10, 10, 10)) +   # extend plot margins to accommodate the border)
    ggplot2::coord_flip()
}

#####################################################################################
###
### fig 1.a

Fig1a_graph <- read_excel(
      dir("data",
          "Fig1a_graph.xlsx",
          full.names = T,
          recursive = T))%>%
  pivot_longer(2:5, names_to = "threat_status", values_to = "num_ecos")%>%
  mutate(threat_precentage = (num_ecos/TOT)*100) %>%
  mutate(across(num_ecos, ~na_if(., 0)))

test(Fig1a_graph, `OVERALL types`,threat_precentage, threat_status, num_ecos)


###
### fig 1.b

Fig1b_graph <- read_excel(
  dir("data",
      "Fig1b_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  pivot_longer(2:9, names_to = "OVERALL_types", values_to = "num_spp") %>%
  na.omit() %>%
  mutate(TOT = sum(num_spp), .by = OVERALL_types )%>%
  mutate(threat_precentage = (num_spp/TOT)*100)

test(Fig1b_graph, OVERALL_types, threat_precentage, `Red List Category`, num_spp)

###
### fig 1.c

Fig1c_graph <- read_excel(
  dir("data",
      "Fig1c_graph updated.xlsx",
      full.names = T,
      recursive = T),
  sheet ="Sheet1")%>%
  pivot_longer(2:5, names_to = "pro_level", values_to = "num_ecos")%>%
  mutate(pro_precentage = (num_ecos/TOT)*100) %>%
  mutate(across(num_ecos, ~na_if(., 0)))


test.1(Fig1c_graph, `OVERALL types`, pro_precentage, pro_level, num_ecos)




#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)
