#####################################################################################
## Script name: thr_status_efg.R
##
## Purpose of script: To create the  protection level stacked bar diagram for the ecosystem functional groups
##
## Author: Lauryn Bull
##
## Date Created: 2024-04-09
##
## Notes:
##
#####################################################################################
### Load libraries
library(tidyverse)
library(sf)

#####################################################################################
### Load the data
ecoAttr_2023_upd <- read_csv("data/tbl_ecoAttr_2023_updated.csv")
names(ecoAttr_2023_upd)

#####################################################################################
### Prepare the data

## subset the data
ecoAttr_2023_sub <- ecoAttr_2023_upd %>%
  select(Ecosystem_functional_type, Percent_protected, Threat_status_2023)

## round values in 'percent protected' column to 2 decimal places
ecoAttr_2023_new <- ecoAttr_2023_sub %>% mutate(across(c('Percent_protected'), round, 2))

## order the data from lowest protection to highest threat status levels
ecoAttr_2023_new$Threat_status_2023 <- factor(ecoAttr_2023_new$Threat_status_2023,
                                              levels = c("Least Concern", "Near Threatened",
                                                         "Vulnerable", "Endangered", "Critically Endangered"))

#####################################################################################
### Summarize the data to calculate counts and percentages for each combination of categories

## summarise the data
summary_data <- ecoAttr_2023_new %>%
  group_by(Ecosystem_functional_type, Threat_status_2023) %>%
  summarize(count = n(), ## calculate frequency totals
            percent = sum(Percent_protected)) %>% ## calculate the total percentages
  mutate(percent = percent / sum(percent) * 100)  ## calculate percentage

#####################################################################################
### Plot the stacked bar chart with counts as text labels on the bars and percentages on the x-axis

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
### save the plot to the outputs folder
ggsave("outputs/threat_status_efg.png", plot = thr_plot, device = "png")

#####################################################################################
