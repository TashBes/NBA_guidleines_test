#####################################################################################
##
## Script name: Andrew graphs
##
## Purpose of script:make functions to create the graphs for Andrews data he sent us.
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
library("cowplot")

#devtools::install_github("TashBes/NBA.package")


#####################################################################################
###
### fig 1.a

Fig1a <- read_excel(
      dir("data",
          "Fig1a_graph.xlsx",
          full.names = T,
          recursive = T))%>%
  slice_head(n =8) %>%
  mutate(across(2:6, as.numeric))

NBA_plot(Fig1a,
     `OVERALL types`,
     2:5,
     CHRT = "bar",
     NUM = TRUE,
     LAB = "Percentage of ecosystem types",
     SAVE = "Fig1a")


###
### fig 1.b

Fig1b <- read_excel(
  dir("data",
      "Fig1b_graph.xlsx",
      full.names = T,
      recursive = T)) %>%
  slice_head(n =4) %>%
  pivot_longer(2:9, names_to = "OVERALL_types") %>%
  pivot_wider(names_from = `Red List Category`)


NBA_plot(Fig1b,
     OVERALL_types,
     2:5,
     CHRT = "bar",
     LAB = "Percentage of taxon types",
     SAVE = "Fig1b")

###
### fig 1.c

Fig1c <- read_excel(
  dir("data",
      "Fig1c_graph updated.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8) %>%
  mutate(across(2:5, as.numeric))


NBA_plot(Fig1c,
     `OVERALL types`,
     2:5,
     CHRT = "bar",
     NUM = T,
     LAB = "Percentage of ecosystem types",
     SAVE = "Fig1c")


###
### fig 1.d

Fig1d <- read_excel(
  dir("data",
      "Fig1d_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  pivot_longer(2:8, names_to = "OVERALL_types") %>%
  pivot_wider(names_from = `...1`)



NBA_plot(Fig1d,
     OVERALL_types,
     2:5,
     CHRT = "bar",
     LAB = "Percentage of taxon types",
     SAVE = "Fig1d")


###
### Fig4a

Fig4a <- read_excel(
  dir("data",
      "Fig4a_graph.xlsx",
      full.names = T,
      recursive = T)) %>%
  slice_head(n =8) %>%
  mutate(across(2:6, as.numeric))



p <- NBA_plot(Fig4a,
          `OVERALL types`,
          2:5,
          NUM = T,
          CHRT = "bar",
          LAB = "Percentage of ecosystem types",
          SAVE = "Fig4a")

#add_rec <- function(GRAPH, CAT1, ..., X%)

p +
  annotate("rect", xmin =2.5, xmax = 3.5, ymin = -1, ymax = 86.5,alpha = 0, color= "black",linewidth = 1.5)+
  annotate("rect", xmin =4.5, xmax = 5.5, ymin = -1, ymax = 79,alpha = 0, color= "black",linewidth = 1.5)


###
### Fig4b

Fig4b <- read_excel(
  dir("data",
      "Fig4b_graph updated.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8) %>%
  mutate(across(2:6, as.numeric))


p <- NBA_plot(Fig4b,
          `OVERALL types`,
          2:5,
          NUM = T,
          CHRT = "bar",
          LAB = "Percentage of ecosystem types",
          SAVE = "Fig4b")

p +
  annotate("rect", xmin =2.5, xmax = 3.5, ymin = -1, ymax = 18.5,alpha = 0, color= "black",linewidth = 1.5)+
  annotate("rect", xmin =4.5, xmax = 5.5, ymin = -1, ymax = 6,alpha = 0, color= "black",linewidth = 1.5)


###
### Fig6

Fig6 <- read_excel(
  dir("data",
      "Fig6_graph_part.xlsx",
      full.names = T,
      recursive = T))%>%
  select(-c(5:6))

RLI_plot(Fig6, Years, RLI, min, max)


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
  annotate("text", x=2005, y=19, label="Aichi 17% target")+
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


plot_grid(b, a, c,
          labels = c("(a)", "(b)", "(c)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2, nrow = 2)


###
### Fig27

Fig27 <- read_excel(
  dir("data",
      "Fig27_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =7)%>%
  mutate(across(2:5, as.numeric))


NBA_plot(Fig27,
         `...1`,
     2:4,
     CHRT = "bar",
     LAB = "Percentage of ecosystem extent",
     SAVE = "Fig27")


###
### Fig28ab

Fig28ab <- read_excel(
  dir("data",
      "Fig28ab_graph.xlsx",
      full.names = T,
      recursive = T))


FG <- Fig28ab%>%
  slice_head(n =8)%>%
  mutate(across(2:6, as.numeric))

EXT <- Fig28ab%>%
  slice(11:18)%>%
  mutate(across(2:6, as.numeric))

a <- NBA_plot(FG,
          `OVERALL types`,
          2:5,
          NUM = T,
          CHRT = "bar",
          LAB = "Percentage of ecosystem types",
          SAVE = "Fig27a")
b <- NBA_plot(EXT,
          `OVERALL types`,
          2:5,
          CHRT = "bar",
          LAB = "Percentage of ecosystem extent",
          SAVE = "Fig27b")

plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)




###
### Fig30

Fig30 <- read_excel(
  dir("data",
      "Fig30_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  rename(Extinct = Extinct...2,
         `Critically Endangered` = `Critically Endangered...3`,
         Endangered = Endangered...4,
         Vulnerable = Vulnerable...5,
         `Near Threatened` = `Near Threatened...6`,
         `Data Deficient` = `Data Deficient...7`,
         `Rare` = `Rare...8`,
         `Least Concern` = `Least Concern...9`) %>%
  select(1:10)

NBA_plot(Fig30,
         Realm,
     2:9,
     CHRT = "bar",
     LAB = "Percentage of taxon types",
     SAVE = "Fig27b")


###
### Fig33a

Fig33a <- read_excel(
  dir("data",
      "Fig33a_graph.xlsx",
      full.names = T,
      recursive = T))

RLI_plot(Fig33a,Years, RLI, min, max)

###
### Fig33b

Fig33b <- read_excel(
  dir("data",
      "Fig33b_graph.xlsx",
      full.names = T,
      recursive = T))

RLI_plot(Fig33b,Years, RLI, min, max)

###
### Fig34ab

Fig34ab <- read_excel(
  dir("data",
      "Fig34ab_graph updated.xlsx",
      full.names = T,
      recursive = T))

FG <- Fig34ab%>%
  slice_head(n =8)%>%
  mutate(across(2:6, as.numeric))

EXT <- Fig34ab%>%
  slice(11:18)%>%
  mutate(across(2:6, as.numeric))

a <- NBA_plot(FG,
          `OVERALL types`,
          2:5,
          NUM = T,
          CHRT = "bar",
          LAB = "Percentage of ecosystem types",
          SAVE = "Fig34a")
b <- NBA_plot(EXT,
          `OVERALL types`,
          2:5,
          CHRT = "bar",
          LAB = "Percentage of ecosystem extent",
          SAVE = "Fig34b")
plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)

##
###Fig36ab_graph 8wx7h

## just has diff x axis label, maybe should make that adjustable?


###
### Fig40ab

Fig40ab <- read_excel(
  dir("data",
      "Fig40ab_graph.xlsx",
      full.names = T,
      recursive = T))

FG <- Fig40ab%>%
  slice_head(n =11)%>%
  mutate(across(2:6, as.numeric))

EXT <- Fig40ab%>%
  slice(14:24)%>%
  mutate(across(2:6, as.numeric))

a <- NBA_plot(FG,
          `TERR types`,
          2:5,
          NUM = T,
          CHRT = "bar",
          LAB = "Percentage of ecosystem types",
          SAVE = "Fig40a")
b <- NBA_plot(EXT,
          `TERR types`,
          2:5,
          CHRT = "bar",
          LAB = "Percentage of ecosystem extent",
          SAVE = "Fig40b")

plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)


###
### Fig42

Fig42 <- read_excel(
  dir("data",
      "Fig42_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =11)

NBA_plot(Fig42,
     `TERR types`,
     2:5,
     NUM = T,
     CHRT = "bar",
     LAB = "Percentage of ecosystem types",
     SAVE = "Fig40b")



###
### Fig44ab

## need to look at x labs (endemic taxa)

###
### Fig48ab

## not sure...

Fig48ab <- read_excel(
  dir("data",
      "Fig48ab_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  select(-`...7`) %>%
  slice_head(n =11)


# NBA_plot(Fig48ab,
#      `TERR types`,
#      2:5,
#      NUM = T,
#      CHRT = "bar",
#      LAB = "Percentage of ecosystem types",
#      SAVE = "Fig48ab")


###
### Fig51



Fig51 <- read_excel(
  dir("data",
      "Fig51_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =3)%>%
  pivot_longer(2:4, names_to = "OVERALL_types") %>%
  pivot_wider(names_from = `River Condition % length`)


NBA_plot(Fig51,
     OVERALL_types,
     2:4,
     CHRT = "bar",
     LAB = "Percentage of ecosystem extent",
           SAVE = "Fig51")

###
### Fig52



Fig52 <- read_excel(
  dir("data",
      "Fig52_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =3)%>%
  pivot_longer(2:6, names_to = "OVERALL_types") %>%
  pivot_wider(names_from = `Wetland Cond`)%>%
  mutate(across(2:4, as.numeric))


NBA_plot(Fig52,
     OVERALL_types,
     2:4,
     CHRT = "bar",
     LAB = "Percentage of ecosystem extent",
     SAVE = "Fig52")


###
### Fig53




Fig53 <- read_excel(
  dir("data",
      "Fig53mapinset_graph .xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  mutate(across(2:5, as.numeric)) %>%
  select(1:5)


NBA_plot(Fig53,
     `OVERALL types`,
     2:5,
     NUM = TRUE,
     LAB = "Threat status",
     GRP = F,
     CHRT = "donut",
     SAVE = "Fig53")


###
### Fig54ab



Fig54ab <- read_excel(
  dir("data",
      "Fig54ab_graph.xlsx",
      full.names = T,
      recursive = T))


FG <- Fig54ab%>%
  slice_head(n =5)%>%
  mutate(across(2:6, as.numeric))

EXT <- Fig54ab%>%
  slice(8:12)%>%
  mutate(across(2:6, as.numeric))


a <- NBA_plot(FG,
          `RIVER types`,
          2:5,
          NUM = T,
          CHRT = "bar",
          LAB = "Percentage of ecosystem types",
          SAVE = "Fig54a")
b <- NBA_plot(EXT,
          `RIVER types`,
          2:5,
          CHRT = "bar",
          LAB = "Percentage of ecosystem extent",
          SAVE = "Fig54b")

plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)


###
### Fig55mapinset



Fig55mapinset <- read_excel(
  dir("data",
      "Fig55mapinset_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  mutate(across(2:5, as.numeric)) %>%
  select(1:5)


NBA_plot(Fig55mapinset,
         `OVERALL types`,
         COLS = 2:5,
         NUM = T,
         GRP = T,
         CHRT = "donut",
         LAB = "Threat status",
         SAVE = "Fig55mapinset")



###
### Fig56

Fig56 <- read_excel(
  dir("data",
      "Fig56_graph.xlsx",
      full.names = T,
      recursive = T))


FG <- Fig56%>%
  slice_head(n =5)%>%
  mutate(across(2:6, as.numeric))

EXT <- Fig56%>%
  slice(8:12)%>%
  mutate(across(2:6, as.numeric))


a <- NBA_plot(FG,
          `WETLAND types`,
          2:5,
          NUM = T,
          CHRT = "bar",
          LAB = "Percentage of ecosystem types",
          SAVE = "Fig56")
b <- NBA_plot(EXT,
          `WETLAND types`,
          2:5,
          CHRT = "bar",
          LAB = "Percentage of ecosystem extent",
          SAVE = "Fig56")

plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)


###
### Fig58mapinset



Fig58mapinset <- read_excel(
  dir("data",
      "Fig58mapinset_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  mutate(across(2:5, as.numeric)) %>%
  select(1:5)


NBA_plot(Fig58mapinset,
     `OVERALL types`,
     COLS = 2:5,
     NUM = T,
     GRP = T,
     CHRT = "donut",
     LAB = "Protection level",
     SAVE = "Fig58mapinset")


###
### Fig59ab

Fig59ab <- read_excel(
  dir("data",
      "Fig59ab_graph.xlsx",
      full.names = T,
      recursive = T))


FG <- Fig59ab%>%
  slice_head(n =5)%>%
  mutate(across(2:5, as.numeric))

EXT <- Fig59ab%>%
  slice(8:12)%>%
  mutate(across(2:5, as.numeric))


a <- NBA_plot(FG,
          `RIVER types`,
          2:5,
          NUM = T,
          CHRT = "bar",
          LAB = "Percentage of ecosystem types",
          SAVE = "Fig59a")
b <- NBA_plot(EXT,
          `RIVER types`,
          2:5,
          CHRT = "bar",
          LAB = "Percentage of ecosystem extent",
          SAVE = "Fig59b")

plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)


###
### Fig61mapinset



Fig61mapinset <- read_excel(
  dir("data",
      "Fig61mapinset_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  mutate(across(2:5, as.numeric)) %>%
  select(1:5)


NBA_plot(Fig61mapinset,
         `OVERALL types`,
         COLS = 2:5,
         NUM = T,
         GRP = T,
         CHRT = "donut",
         LAB = "Protection level",
         SAVE = "Fig61mapinset")


###
### Fig64ab

Fig64ab<- read_excel(
  dir("data",
      "Fig64ab_graph.xlsx",
      full.names = T,
      recursive = T))


TAXA <- Fig64ab%>%
  select(1:8) %>%
  janitor::row_to_names(row_number = 1)%>%
  mutate(across(2:8, as.numeric))%>%
  pivot_longer(2:8, names_to = "OVERALL_types")%>%
  pivot_wider(names_from = `All species`)

END <- Fig64ab%>%
  select(10:17) %>%
  janitor::row_to_names(row_number = 1)%>%
  mutate(across(2:8, as.numeric))%>%
  pivot_longer(2:8, names_to = "OVERALL_types")%>%
  pivot_wider(names_from = `All species`)

a <- NBA_plot(TAXA,
              OVERALL_types,
              2:9,
              NUM = T,
              CHRT = "bar",
              LAB = "Percentage of taxa",
              SAVE = "Fig59a")
b <- NBA_plot(END,
              OVERALL_types,
              2:9,
              CHRT = "bar",
              LAB = "Percentage of endemic taxa",
              SAVE = "Fig59b")


plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)


###
### Fig67ab

Fig67ab <- read_excel(
  dir("data",
      "Fig67ab_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  janitor::row_to_names(row_number = 1)


TAXA <- Fig67ab%>%
  slice_head(n =5)%>%
  mutate(across(2:5, as.numeric))

END <- Fig67ab%>%
  slice(9:13)%>%
  mutate(across(2:5, as.numeric))

a <- NBA_plot(TAXA,
              all,
              2:5,
              NUM = T,
              CHRT = "bar",
              LAB = "Percentage of taxa",
              SAVE = "Fig67a")
b <- NBA_plot(END,
              all,
              2:5,
              CHRT = "bar",
              LAB = "Percentage of endemic taxa",
              SAVE = "Fig67b")

plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)

###
##Fig68_graph fixed20190829

## still working on

###
### Fig69ab

Fig69ab <- read_excel(
  dir("data",
      "Fig69ab_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  janitor::row_to_names(row_number = 1)


# FG <- Fig69ab%>%
#   slice_head(n =5)%>%
#   mutate(across(2:6, as.numeric))
#
# EXT <- Fig69ab%>%
#   slice(7:11)%>%
#   mutate(across(2:6, as.numeric))
#
#
# a <- NBA_plot(TAXA,
#               `Biogeographical region`,
#               2:6,
#               NUM = T,
#               CHRT = "bar",
#               LAB = "Percentage of ecosystem types",
#               SAVE = "Fig69a")
# b <- NBA_plot(END,
#               `Biogeographical region`,
#               2:6,
#               CHRT = "bar",
#               LAB = "Percentage of ecosystem extent",
#               SAVE = "Fig69b")
#
# plot_grid(a,b,
#           labels = c("(a)", "(b)"),
#           label_size = 8,
#           label_fontface = "plain",
#           ncol = 2)



###
### Fig71ab

Fig71ab <- read_excel(
  dir("data",
      "Fig71ab_graph.xlsx",
      full.names = T,
      recursive = T)) %>%
  select(-1)



FG <- Fig71ab%>%
  slice_head(n =5)%>%
  mutate(across(2:5, as.numeric))

EXT <- Fig71ab%>%
  slice(7:11)%>%
  mutate(across(2:5, as.numeric))

a <- NBA_plot(FG,
              `Biogeographical region`,
              2:5,
              NUM = T,
              CHRT = "bar",
              LAB = "Percentage of ecosystem types",
              SAVE = "Fig71a")
b <- NBA_plot(EXT,
              `Biogeographical region`,
              2:5,
              CHRT = "bar",
              LAB = "Percentage of ecosystem extent",
              SAVE = "Fig71b")


plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)

###
### Fig73ab

Fig73ab <- read_excel(
  dir("data",
      "Fig73ab_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  janitor::row_to_names(row_number = 1)



FG <- Fig73ab%>%
  slice_head(n =5)%>%
  mutate(across(2:5, as.numeric))

EXT <- Fig73ab%>%
  slice(8:12)%>%
  mutate(across(2:5, as.numeric))

a <- NBA_plot(FG,
              `Biogeographical region`,
              2:5,
              NUM = T,
              CHRT = "bar",
              LAB = "Percentage of ecosystem types",
              SAVE = "Fig73a")
b <- NBA_plot(EXT,
              `Biogeographical region`,
              2:5,
              CHRT = "bar",
              LAB = "Percentage of ecosystem extent",
              SAVE = "Fig73b")


plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)

###
### Fig74

Fig74 <- read_excel(
  dir("data",
      "Fig74_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  pivot_longer(2:5, names_to = "OVERALL_types")%>%
  pivot_wider(names_from = `Est RLS`) %>%
  select(-2)%>%
  mutate(across(2:8, as.numeric))


NBA_plot(Fig74,
         OVERALL_types,
         2:8,
         NUM = T,
         CHRT = "bar",
         LAB = "Percentage of ecosystem types",
         SAVE = "Fig73b")


###
### Fig84

Fig84 <- read_excel(
  dir("data",
      "Fig84_graph.xlsx",
      full.names = T,
      recursive = T))


NBA_plot(Fig84,
         `2019 MPAs`,
         2:5,
         NUM = T,
         CHRT = "bar",
         LAB = "Percentage of ecosystem types",
         SAVE = "Fig73b")



### Fig90



Fig90 <- read_excel(
  dir("data",
      "Fig90_graph.xlsx",
      full.names = T,
      recursive = T)) %>%
  select(1:7)%>%
  mutate(`...1` = if_else(is.na(`...1`), "OVERALL_types", `...1`)) %>%
  janitor::row_to_names(1)%>%
  slice_head(n =2)%>%
  mutate(across(2:7, as.numeric))


NBA_plot(Fig90,
         OVERALL_types,
         COLS = 2:7,
         CHRT = "bar",
         LAB = "Percentage of ecosystem land use",
         SAVE = "Fig90")


###
### Fig91ab

Fig91ab <- read_excel(
  dir("data",
      "Fig91ab_graph.xlsx",
      full.names = T,
      recursive = T))


FG <- Fig91ab%>%
  slice_head(n =2)%>%
  mutate(across(2:4, as.numeric))

EXT <- Fig91ab%>%
  slice(4:7)%>%
  mutate(across(2:4, as.numeric))

a <- NBA_plot(FG,
              `...1`,
              num =T,
              2:4,
              CHRT = "bar",
              LAB = "Percentage of ecosystem types",
              SAVE = "Fig91a")
b <- NBA_plot(EXT, `...1`,
              2:4,
              CHRT = "bar",
              LAB = "Percentage of ecosystem extent",
              SAVE = "Fig91b")

plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          nrow = 2)


###
### Fig92abcd

Fig92abcd <- read_excel(
  dir("data",
      "Fig92abcd_graph updated.xlsx",
      full.names = T,
      recursive = T))


FG1 <- Fig92abcd%>%
  select(1:5) %>%
  rename(`Critically Endangered` = `CR...2`,
         Endangered = `EN...3`,
         Vulnerable = `VU...4`,
         `Least Concern` = `LC...5`)

EXT1 <- Fig92abcd%>%
  select(7:11)%>%
  rename(`Critically Endangered` = `CR...8`,
         Endangered = `EN...9`,
         Vulnerable = `VU...10`,
         `Least Concern` = `LC...11`)

FG2 <- Fig92abcd%>%
  select(13:17) %>%
  slice_head(n=2) %>%
  rename(`Critically Endangered` = `CR...14`,
         Endangered = `EN...15`,
         Vulnerable = `VU...16`,
         `Least Concern` = `LC...17`)

EXT2 <- Fig92abcd%>%
  select(19:23)%>%
  slice_head(n=2) %>%
  rename(`Critically Endangered` = `CR...20`,
         Endangered = `EN...21`,
         Vulnerable = `VU...22`,
         `Least Concern` = `LC...23`)

a <- NBA_plot(FG1,
              `Coast RLE types...1`,
              2:4,
              NUM = T,
              CHRT = "bar",
              LAB = "Percentage of ecosystem types",
              SAVE = "Fig92a")
a
b <- NBA_plot(EXT1,
              `Coast RLE extent`,
              2:4,
              CHRT = "bar",
              LAB = "Percentage of ecosystem extent",
              SAVE = "Fig92b")
b
c <- NBA_plot(FG2,
              `Coast RLE types...13`,
              2:4,
              NUM = T,
              CHRT = "bar",
              LAB = "Percentage of ecosystem types",
              SAVE = "Fig92c")
c
d <- NBA_plot(EXT2,
              `...19`,
              2:4,
              CHRT = "bar",
              LAB = "Percentage of ecosystem extent",
              SAVE = "Fig92d")
d

plot_grid(a,b,c,d,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          label_size = 8,
          label_fontface = "plain",
          nrow = 2, ncol = 2)




###
### Fig93abcd

Fig93abcd <- read_excel(
  dir("data",
      "Fig93abcd_graph updated.xlsx",
      full.names = T,
      recursive = T))


FG1 <- Fig93abcd%>%
  select(1:5) %>%
  rename(`Well Protected` = `WP...2`,
         `Moderately Protected` = `MP...3`,
         `Poorly Protected` = `PP...4`,
         `No Protection` = `NP...5`)

EXT1 <- Fig93abcd%>%
  select(7:11)%>%
  rename(`Well Protected` = `WP...8`,
         `Moderately Protected` = `MP...9`,
         `Poorly Protected` = `PP...10`,
         `No Protection` = `NP...11`)

FG2 <- Fig93abcd%>%
  select(13:17) %>%
  slice_head(n=2) %>%
  rename(`Well Protected` = `WP...14`,
         `Moderately Protected` = `MP...15`,
         `Poorly Protected` = `PP...16`,
         `No Protection` = `NP...17`)

EXT2 <- Fig93abcd%>%
  select(19:23)%>%
  slice_head(n=2) %>%
  rename(`Well Protected` = `WP...20`,
         `Moderately Protected` = `MP...21`,
         `Poorly Protected` = `PP...22`,
         `No Protection` = `NP...23`)

a <- NBA_plot(FG1,
              `...1`,
              2:4,
              NUM = T,
              CHRT = "bar",
              LAB = "Percentage of ecosystem types",
              SAVE = "Fig93a")
a
b <- NBA_plot(EXT1,
              `...7`,
              2:4,
              CHRT = "bar",
              LAB = "Percentage of ecosystem extent",
              SAVE = "Fig93b")
b
c <- NBA_plot(FG2,
              `...13`,
              2:4,
              NUM = T,
              CHRT = "bar",
              LAB = "Percentage of ecosystem types",
              SAVE = "Fig93c")
c
d <- NBA_plot(EXT2,
              `...19`,
              2:4,
              CHRT = "bar",
              LAB = "Percentage of ecosystem extent",
              SAVE = "Fig93d")
d


plot_grid(a,b,c,d,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          label_size = 8,
          label_fontface = "plain",
          nrow = 2, ncol = 2)



###
### Fig98mapinset


Fig98mapinset <- read_excel(
  dir("data",
      "Fig98mapinset_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  mutate(across(2:5, as.numeric)) %>%
  select(1:5)


NBA_plot(Fig98mapinset,
         `OVERALL types`,
         COLS = 2:5,
         NUM = T,
         GRP = T,
         CHRT = "donut",
         LAB = "Threat status",
         SAVE = "Fig98mapinset")




###
### Fig99mapinset


Fig99mapinset <- read_excel(
  dir("data",
      "Fig99mapinset_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  mutate(across(2:5, as.numeric)) %>%
  select(1:5)


NBA_plot(Fig99mapinset,
         `OVERALL types`,
         COLS = 2:5,
         NUM = T,
         GRP = T,
         CHRT = "donut",
         LAB = "Protection level",
         SAVE = "Fig99mapinset")





###
### Table3

Table3 <- read_excel(
  dir("data",
      "Table 3 SpeciesSummaryData_NBA_2019_ALS.xlsx",
      full.names = T,
      recursive = T),
  sheet = "T3 in NBA 2018 Synthesis")


basic_tbl(Table3)



#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)
