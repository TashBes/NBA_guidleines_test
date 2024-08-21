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
    ggplot2::geom_text(aes(label = {{COUNT}}),
                       position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                       size = 3,
                       color = "black",
                       show.legend = FALSE) + # adjust size of labels with no legend being shown
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


test.2 <- function(DF,YEAR, RLI, min, max){
  ggplot2::ggplot(DF, aes(x = {{YEAR}}, y = {{RLI}})) +
    ggplot2::geom_line(aes(y = {{RLI}})) +
    ggplot2::geom_ribbon(aes(ymin = {{min}}, ymax = {{max}}),alpha = .3, colour = NA)+
    ggplot2::theme_classic()+
    ggplot2::ylim(0.7,1)

}
#####################################################################################
###
### fig 1.a

Fig1a <- read_excel(
      dir("data",
          "Fig1a_graph.xlsx",
          full.names = T,
          recursive = T))%>%
  slice_head(n =8) %>%
  mutate(across(2:6, as.numeric))%>%
  pivot_longer(2:5, names_to = "threat_status", values_to = "num_ecos")%>%
  mutate(threat_precentage = (num_ecos/TOT)*100)%>%
  mutate(across(num_ecos, ~na_if(., 0)))

test(Fig1a, `OVERALL types`,threat_precentage, threat_status, num_ecos)


###
### fig 1.b

Fig1b <- read_excel(
  dir("data",
      "Fig1b_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  pivot_longer(2:9, names_to = "OVERALL_types", values_to = "num_spp") %>%
  na.omit() %>%
  mutate(TOT = sum(num_spp), .by = OVERALL_types )%>%
  mutate(threat_precentage = (num_spp/TOT)*100)

test(Fig1b, OVERALL_types, threat_precentage, `Red List Category`, num_spp)

###
### fig 1.c

Fig1c <- read_excel(
  dir("data",
      "Fig1c_graph updated.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8) %>%
  mutate(across(2:5, as.numeric))%>%
  pivot_longer(2:5, names_to = "pro_level", values_to = "num_ecos")%>%
  mutate(pro_precentage = (num_ecos/...6)*100) %>%
  mutate(across(num_ecos, ~na_if(., 0)))


test.1(Fig1c, `OVERALL types`, pro_precentage, pro_level, num_ecos)


###
### fig 1.d

Fig1d <- read_excel(
  dir("data",
      "Fig1d_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  pivot_longer(2:8, names_to = "OVERALL_types", values_to = "num_spp")%>%
  mutate(TOT = sum(num_spp), .by = OVERALL_types )%>%
  mutate(pro_precentage = (num_spp/TOT)*100)


test.1(Fig1d, OVERALL_types, pro_precentage, `...1`, num_spp)


###
### Fig4a

Fig4a <- read_excel(
  dir("data",
      "Fig4a_graph.xlsx",
      full.names = T,
      recursive = T)) %>%
  slice_head(n =8) %>%
  mutate(across(2:6, as.numeric))%>%
  pivot_longer(2:5, names_to = "threat_status", values_to = "num_ecos")%>%
  mutate(thr_precentage = (num_ecos/TOT)*100)%>%
  mutate(across(num_ecos, ~na_if(., 0)))


p <- test(Fig4a, `OVERALL types`, thr_precentage, threat_status, num_ecos)

p +
  annotate("rect", xmin =1.5, xmax = 2.5, ymin = -1, ymax = 86.5,alpha = 0, color= "black",linewidth = 1.5)+
  annotate("rect", xmin =7.5, xmax = 8.5, ymin = -1, ymax = 79,alpha = 0, color= "black",linewidth = 1.5)


###
### Fig4b

Fig4b <- read_excel(
  dir("data",
      "Fig4b_graph updated.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8) %>%
  mutate(across(2:6, as.numeric))%>%
  pivot_longer(2:5, names_to = "pro_level", values_to = "num_ecos")%>%
  mutate(pro_precentage = (num_ecos/`...6`)*100)%>%
  mutate(across(num_ecos, ~na_if(., 0)))


p <- test.1(Fig4b, `OVERALL types`, pro_precentage, pro_level, num_ecos)

p +
  annotate("rect", xmin =1.5, xmax = 2.5, ymin = -1, ymax = 19,alpha = 0, color= "black",linewidth = 1.5)+
  annotate("rect", xmin =7.5, xmax = 8.5, ymin = -1, ymax = 6,alpha = 0, color= "black",linewidth = 1.5)


###
### Fig6

Fig6 <- read_excel(
  dir("data",
      "Fig6_graph_part.xlsx",
      full.names = T,
      recursive = T))%>%
  select(-c(5:6))

test.2(Fig6, Years, RLI, min, max)


###
### Fig21

## no idea how to do this or what it is

###
### Fig23abc

Fig23abc <- read_excel(
  dir("data",
      "Fig23abc_graph.xlsx",
      full.names = T,
      recursive = T))


a <- Fig23abc %>%
  slice(1:5) %>%
  pivot_longer(2:9, names_to = "year") %>%
  mutate(year = as.numeric(year))%>%
  filter(`MAINLAND EEZ`%in% c("MPAs as a proprtion of the mainland EEZ","Overall prop. PA contributing to targets")) %>%
  mutate(value = value*100)%>%
  pivot_wider(names_from =`MAINLAND EEZ` ) %>%
  ggplot(aes(x = year))+
  geom_line(aes(y = `MPAs as a proprtion of the mainland EEZ`),
            show.legend =F,
            linewidth = 1,
            colour = "lightblue")+
  geom_line(aes(y = `Overall prop. PA contributing to targets`),
            show.legend =F,
            linewidth = 1,
            colour = "black",
            linetype = "dashed")+
  labs(y = "", x = "")+
  geom_hline(yintercept=10,
             linetype="dashed",
             color = "green",
             linewidth = 1.5)+
  annotate("text", x=2005, y=12, label="Aichi 10% target")+
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"),
                              limits = c(0,40),
                              expand = c(0, 0))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(colour = "grey"),
        panel.grid.major.y = element_line(colour = "grey"))
a


b <- Fig23abc %>%
  slice(17:19) %>%
  pivot_longer(2:9, names_to = "year") %>%
  mutate(year = as.numeric(year))%>%
  filter(`MAINLAND EEZ`%in% c("PAs as a proprtion of the mainland","Overall prop. PA contributing to targets")) %>%
  mutate(value = value*100)%>%
  pivot_wider(names_from =`MAINLAND EEZ` )%>%
  ggplot(aes(x = year))+
  geom_line(aes(y = `PAs as a proprtion of the mainland`),
            show.legend =F,
            linewidth = 1,
            colour = "darkblue")+
  geom_line(aes(y = `Overall prop. PA contributing to targets`),
            show.legend =F,
            linewidth = 1,
            colour = "black",
            linetype = "dashed")+
  labs(y = "", x = "")+
  geom_hline(yintercept=17,
             linetype="dashed",
             color = "lightblue",
             linewidth = 1.5)+
  annotate("text", x=2005, y=12, label="Aichi 17% target")+
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"),
                              limits = c(0,40),
                              expand = c(0, 0))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(colour = "grey"),
        panel.grid.major.y = element_line(colour = "grey"))
b


c <- Fig23abc %>%
  slice(10:12) %>%
  pivot_longer(2:9, names_to = "year") %>%
  mutate(year = as.numeric(year))%>%
  filter(`MAINLAND EEZ`%in% c("PEI MPA as proportion of PEI EEZ",
                              "PEI prop. PA contributing to targets")) %>%
  mutate(value = value*100)%>%
  pivot_wider(names_from =`MAINLAND EEZ` )%>%
  ggplot(aes(x = year))+
  geom_line(aes(y = `PEI MPA as proportion of PEI EEZ`),
            show.legend =F,
            linewidth = 1,
            colour = "darkgrey")+
  geom_line(aes(y = `PEI prop. PA contributing to targets`),
            show.legend =F,
            linewidth = 1,
            colour = "black",
            linetype = "dashed")+
  labs(y = "", x = "")+
  geom_hline(yintercept=10,
             linetype="dashed",
             color = "black",
             linewidth = 1.5)+
  annotate("text", x=2005, y=12, label="Aichi 10% target")+
  ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"),
                              limits = c(0,40),
                              expand = c(0, 0))+
  theme(panel.background = element_blank(),
        axis.line.x = element_line(colour = "grey"),
        panel.grid.major.y = element_line(colour = "grey"))
c

library("cowplot")
plot_grid(b, a, c,
          labels = c("(a)", "(b)", "(c)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2, nrow = 2)

#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)
