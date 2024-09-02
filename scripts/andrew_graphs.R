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
library("cowplot")

# source("functions/packages.R")       # loads up all the packages we need
#devtools::install_github("TashBes/NBA.package")
#####################################################################################
### functions

test <-function(DF, X, COLS, TYP = c("FG", "EXT", "TAXA", "END")){

  cols <- c("black","#e9302c", "#f97835", "#fff02a", "#eeeea3","brown","grey" , "#b1d798", "#B9B386","#DB7D15", "#B36611", "#808080", "#F5C592","#0071C0")
  breaks <- c("Extinct", "Critically Endangered", "Endangered","Vulnerable","Near Threatened", "Data Deficient", "Rare", "Least Concern", "Natural", "Cropland", "Plantation","Built up","Mine", "Artificial waterbody")

  ord <-   DF %>%
    dplyr::pull({{X}})

  dat <- DF %>%
    pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
    mutate(TOT = sum(COUNT, na.rm = T), .by = {{X}} )%>%
    mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
    mutate(across(COUNT, ~na_if(., 0))) %>%
    mutate(FILL = factor(FILL, levels = breaks))

  if(TYP == "FG"){



  ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
    ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
    ggplot2::geom_text(aes(label = COUNT),
                       position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                       size = 3,
                       color = "black",
                       show.legend = FALSE) + # adjust size of labels with no legend being shown
    ggplot2::scale_fill_manual(values = cols, breaks = breaks)+  # order the colours of the bars in the reversed order
    ggplot2::ylab("Percentage of ecosystem types") +
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

  else {

    if(TYP == "EXT"){

      ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
        ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
        ggplot2::ylab("Percentage of ecosystem extent") +
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

      else {

        if(TYP == "TAXA"){

        ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
          ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
          ggplot2::ylab("Percentage of taxa") +
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

        else {

          ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
            ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
            ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
            ggplot2::ylab("Percentage of endemic taxa") +
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

      }

  }
  }

test.1 <-function(DF, X, COLS, TYP = c("FG", "EXT", "TAXA")) {

  cols <- c("#466a31","#80a952","#d5dec3","#a4a3a3")
  breaks <- c("Well Protected","Moderately Protected","Poorly Protected","No Protection")
  ord <-   DF %>%
    dplyr::pull({{X}})


  dat <- DF %>%
    pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
    mutate(TOT = sum(COUNT, na.rm = T), .by = {{X}} )%>%
    mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
    mutate(across(COUNT, ~na_if(., 0))) %>%
    mutate(FILL = factor(FILL, levels = breaks))

  if(TYP == "FG"){



    ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
      ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
      ggplot2::geom_text(aes(label = COUNT),
                         position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                         size = 3,
                         color = "black",
                         show.legend = FALSE) + # adjust size of labels with no legend being shown
      ggplot2::scale_fill_manual(values = cols, breaks = breaks)+  # order the colours of the bars in the reversed order
      ggplot2::ylab("Percentage of ecosystem types") +
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

  else {

    if(TYP == "EXT"){

      ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
        ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
        ggplot2::ylab("Percentage of ecosystem extent") +
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

    else {

      if(TYP == "TAXA"){

        ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
          ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
          ggplot2::ylab("Percentage of taxa") +
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

      else {

        ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
          ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
          ggplot2::ylab("Percentage of endemic taxa") +
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

    }

  }
}

test.2 <- function(DF,YEAR, RLI, min, max){
  ggplot2::ggplot(DF, aes(x = {{YEAR}}, y = {{RLI}})) +
    ggplot2::geom_line(aes(y = {{RLI}})) +
    ggplot2::geom_ribbon(aes(ymin = {{min}}, ymax = {{max}}),alpha = .3, colour = NA)+
    ggplot2::theme_classic()+
    ggplot2::ylim(0.7,1)

}


test.3 <-function(DF, X, COLS, TYP = c("FG", "EXT", "TAXA")) {

  cols <- c("#6e9fd4","#6e9fd4", "#a5c5c7","#81aba7","#88814e","#88812e")
  breaks <- c("Natural", "Natural/near-natural", "Near-natural", "Moderately modified","Heavily modified", "Severely/critically modified")
  ord <-   DF %>%
    dplyr::pull({{X}})


  dat <- DF %>%
    pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
    mutate(TOT = sum(COUNT, na.rm = T), .by = {{X}} )%>%
    mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
    mutate(across(COUNT, ~na_if(., 0))) %>%
    mutate(FILL = factor(FILL, levels = breaks))

  if(TYP == "FG"){



    ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
      ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
      ggplot2::geom_text(aes(label = COUNT),
                         position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                         size = 3,
                         color = "black",
                         show.legend = FALSE) + # adjust size of labels with no legend being shown
      ggplot2::scale_fill_manual(values = cols, breaks = breaks)+  # order the colours of the bars in the reversed order
      ggplot2::ylab("Percentage of ecosystem types") +
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

  else {

    if(TYP == "EXT"){

      ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
        ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
        ggplot2::ylab("Percentage of ecosystem extent") +
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

    else {

      if(TYP == "TAXA"){

        ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
          ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
          ggplot2::ylab("Percentage of taxa") +
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

      else {

        ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
          ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
          ggplot2::ylab("Percentage of endemic taxa") +
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

    }

  }
}


test.4 <-function(DF, X, COLS, GRP = FALSE)
{

  ### define the order
  cols <- c("#e9302c","#f97835","#fff02a","#eeeea3","#b1d798")
  breaks <- c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")

  if(GRP == FALSE) {

    ## Prepare the data frame by arranging and setting colors
    dat <- DF %>%
      pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
      group_by(FILL) %>%
      summarise(COUNT = sum(COUNT, na.rm = T))  %>%
      mutate(FILL = factor(FILL, levels = breaks))%>%
      dplyr::mutate(ymax = cumsum(COUNT)) %>%
      dplyr::mutate(ymin = ymax -COUNT)


    ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
      ggplot2::geom_rect() +
      ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3) +  ## Add this line to include count values
      ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
      ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
      ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
      ggplot2::labs(fill = "Threat Status") +
      ggplot2::theme_void() + ## removes the lines around chart and grey background
      ggplot2::theme(
        panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
        plot.background = element_rect(fill = "white", color = NA)  ## set plot background to white
      )

  }

  else {


    ## Prepare the data frame by arranging and setting colors
    dat <- DF %>%
      pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
      mutate(TOT = sum(COUNT, na.rm = T), .by = {{X}} )%>%
      mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
      mutate(FILL = factor(FILL, levels = breaks))%>%
      dplyr::mutate(ymax = cumsum(PERCENTAGE), .by = {{X}}) %>%
      dplyr::mutate(ymin = ymax -PERCENTAGE)

    ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
      ggplot2::geom_rect() +
      facet_wrap(vars({{X}}))+
      ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 1) +  ## Add this line to include count values
      ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
      ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
      ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
      ggplot2::labs(fill = "Threat Status") +
      ggplot2::theme_void() + ## removes the lines around chart and grey background
      ggplot2::theme(
        panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
        plot.background = element_rect(fill = "white", color = NA)  ## set plot background to white
      )
  }
}



test.5 <-function(DF, X, COLS, GRP = FALSE)
{

  ### define the order
  cols <- c("#466a31","#80a952","#d5dec3","#a4a3a3")
  breaks <- c("Well Protected","Moderately Protected","Poorly Protected","No Protection")

  if(GRP == FALSE) {

    ## Prepare the data frame by arranging and setting colors
    dat <- DF %>%
      pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
      group_by(FILL) %>%
      summarise(COUNT = sum(COUNT, na.rm = T))  %>%
      mutate(FILL = factor(FILL, levels = breaks))%>%
      dplyr::mutate(ymax = cumsum(COUNT)) %>%
      dplyr::mutate(ymin = ymax -COUNT)


    ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
      ggplot2::geom_rect() +
      ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3) +  ## Add this line to include count values
      ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
      ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
      ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
      ggplot2::labs(fill = "Threat Status") +
      ggplot2::theme_void() + ## removes the lines around chart and grey background
      ggplot2::theme(
        panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
        plot.background = element_rect(fill = "white", color = NA)  ## set plot background to white
      )

  }

  else {


    ## Prepare the data frame by arranging and setting colors
    dat <- DF %>%
      pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
      mutate(TOT = sum(COUNT, na.rm = T), .by = {{X}} )%>%
      mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
      mutate(FILL = factor(FILL, levels = breaks))%>%
      dplyr::mutate(ymax = cumsum(PERCENTAGE), .by = {{X}}) %>%
      dplyr::mutate(ymin = ymax -PERCENTAGE)

    ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
      ggplot2::geom_rect() +
      facet_wrap(vars({{X}}))+
      ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 1) +  ## Add this line to include count values
      ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
      ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
      ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
      ggplot2::labs(fill = "Threat Status") +
      ggplot2::theme_void() + ## removes the lines around chart and grey background
      ggplot2::theme(
        panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
        plot.background = element_rect(fill = "white", color = NA)  ## set plot background to white
      )
  }
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
  mutate(across(2:6, as.numeric))

test(Fig1a, `OVERALL types`, 2:5,  TYP = "FG")


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


test(Fig1b, OVERALL_types, 2:5,  TYP = "SPP")

###
### fig 1.c

Fig1c <- read_excel(
  dir("data",
      "Fig1c_graph updated.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8) %>%
  mutate(across(2:5, as.numeric))


test.1(Fig1c, `OVERALL types`, 2:5,  TYP = "FG")


###
### fig 1.d

Fig1d <- read_excel(
  dir("data",
      "Fig1d_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  pivot_longer(2:8, names_to = "OVERALL_types") %>%
  pivot_wider(names_from = `...1`)



test.1(Fig1d, OVERALL_types, 2:5, TYP = "SPP")


###
### Fig4a

Fig4a <- read_excel(
  dir("data",
      "Fig4a_graph.xlsx",
      full.names = T,
      recursive = T)) %>%
  slice_head(n =8) %>%
  mutate(across(2:6, as.numeric))



p <- test(Fig4a, `OVERALL types`,2:5, TYP = "FG")

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


p <- test.1(Fig4b, `OVERALL types`, 2:5, TYP = "FG")

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


#test.3 <- function(DF, ROWS, YR_COLS, NM_COL,  )


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


test.3(Fig27,`...1`, 2:4, TYP = "EXT")


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

a <- test(FG,`OVERALL types`, 2:5, TYP = "FG" )
b <- test(EXT,`OVERALL types`, 2:5, TYP = "EXT" )

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

test(Fig30,Realm, 2:9, TYP = "SPP")


###
### Fig33a

Fig33a <- read_excel(
  dir("data",
      "Fig33a_graph.xlsx",
      full.names = T,
      recursive = T))

test.2(Fig33a,Years, RLI, min, max)

###
### Fig33b

Fig33b <- read_excel(
  dir("data",
      "Fig33b_graph.xlsx",
      full.names = T,
      recursive = T))

test.2(Fig33b,Years, RLI, min, max)

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

a <- test.1(FG,`OVERALL types`, 2:5, TYP = "FG" )
b <- test.1(EXT,`OVERALL types`, 2:5, TYP = "EXT" )

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

a <- test(FG,`TERR types`, 2:5, TYP = "FG" )
b <- test(EXT,`TERR types`, 2:5, TYP = "EXT" )

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

test.1(Fig42,`TERR types`, 2:5, TYP = "FG" )



###
### Fig44ab

## need to look at x labs (endemic taxa)

###
### Fig48ab



Fig48ab <- read_excel(
  dir("data",
      "Fig48ab_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  select(-`...7`) %>%
  slice_head(n =11)


test.1(Fig42,`TERR types`, 2:5, TYP = "FG" )


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


test.3(Fig51,OVERALL_types , 2:4, TYP = "EXT" )


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


test.3(Fig52,OVERALL_types , 2:4, TYP = "EXT" )


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


test.4(Fig53, `OVERALL types`, 2:5, GRP = T)




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

a <- test(FG,`RIVER types`, 2:5, TYP = "FG" )
b <- test(EXT,`RIVER types`, 2:5, TYP = "EXT" )

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


test.4(Fig55mapinset, `OVERALL types`, COLS = 2:5, GRP = T)



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

a <- test(FG,`WETLAND types`, 2:5, TYP = "FG" )
b <- test(EXT,`WETLAND types`, 2:5, TYP = "EXT" )

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


test.5(Fig58mapinset, `OVERALL types`, COLS = 2:5, GRP = T)


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

a <- test.1(FG,`RIVER types`, 2:5, TYP = "FG" )
b <- test.1(EXT,`RIVER types`, 2:5, TYP = "EXT" )

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


test.5(Fig61mapinset, `OVERALL types`, COLS = 2:5, GRP = T)


###
### Fig64ab

Fig64ab <- read_excel(
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

a <- test(TAXA,OVERALL_types, 2:9, TYP = "TAXA" )
b <- test(END,OVERALL_types, 2:9, TYP = "END" )

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

a <- test.1(TAXA,all, 2:5, TYP = "TAXA" )
b <- test.1(END,all, 2:5, TYP = "END" )

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


FG <- Fig69ab%>%
  slice_head(n =5)%>%
  mutate(across(2:6, as.numeric))

EXT <- Fig69ab%>%
  slice(7:11)%>%
  mutate(across(2:6, as.numeric))

a <- test.3(FG, `Biogeographical region`, 2:6, TYP = "FG" )
b <- test.3(EXT, `Biogeographical region`, 2:6, TYP = "EXT" )

plot_grid(a,b,
          labels = c("(a)", "(b)"),
          label_size = 8,
          label_fontface = "plain",
          ncol = 2)



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

a <- test(FG, `Biogeographical region`, 2:5, TYP = "FG" )
b <- test(EXT, `Biogeographical region`, 2:5, TYP = "EXT" )

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

a <- test.1(FG, `Biogeographical region`, 2:5, TYP = "FG" )
b <- test.1(EXT, `Biogeographical region`, 2:5, TYP = "EXT" )

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


test(Fig74, OVERALL_types, 2:8, TYP = "FG" )


###
### Fig84

Fig84 <- read_excel(
  dir("data",
      "Fig84_graph.xlsx",
      full.names = T,
      recursive = T))


test.1(Fig84, `2019 MPAs`, 2:5, TYP = "FG" )



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


test.6(Fig90, OVERALL_types, COLS = 2:7)




test.6 <-function(DF, X, COLS){

  cols <- c("#B9B386","#DB7D15", "#B36611", "#808080", "#F5C592","#0071C0")
  breaks <- c("Natural", "Cropland", "Plantation","Built up","Mine", "Artificial waterbody")
  ord <-   DF %>%
    dplyr::pull({{X}})

  dat <- DF %>%
    pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
    mutate(TOT = sum(COUNT, na.rm = T), .by = {{X}} )%>%
    mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
    mutate(across(COUNT, ~na_if(., 0))) %>%
    mutate(FILL = factor(FILL, levels = breaks))


  ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{X}}, level = ord), fill = FILL)) +
    ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
    ggplot2::scale_fill_manual(values = cols, breaks = breaks)+  # order the colours of the bars in the reversed order
    ggplot2::ylab("Percentage extent in each land cover category") +
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



#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)
