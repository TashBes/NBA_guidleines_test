#####################################################################################
##
## Script name:functions
##
## Purpose of script:
##
## Author:
##
## Date Created: 202y-mm-dd
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

#####################################################################################
#####################################################################################
### functions

test <-function(DF, GROUPS, COLS, CHRT = c("bar", "donut"), NUM = FALSE, LAB, GRP = TRUE, SAVE){

  cols <- c("#6e9fd4",
            "#6e9fd4",
            "#a5c5c7",
            "#81aba7",
            "#88814e",
            "#88812e",
            "#466a31",
            "#80a952",
            "#d5dec3",
            "#a4a3a3",
            "#a4a3a3",
            "black",
            "#e9302c",
            "#f97835",
            "#fff02a",
            "#eeeea3",
            "brown",
            "grey" ,
            "#b1d798",
            "#DB7D15",
            "#B36611",
            "#808080",
            "#F5C592",
            "#0071C0")

  breaks <- c("Natural",
              "Natural/near-natural",
              "Near-natural",
              "Moderately modified",
              "Heavily modified",
              "Severely/critically modified",
              "Well Protected",
              "Moderately Protected",
              "Poorly Protected",
              "No Protection",
              "Not Protected",
              "Extinct",
              "Critically Endangered",
              "Endangered",
              "Vulnerable",
              "Near Threatened",
              "Data Deficient",
              "Rare",
              "Least Concern",
              "Cropland",
              "Plantation",
              "Built up",
              "Mine",
              "Artificial waterbody")

  if(CHRT == "donut"){

    if(GRP == FALSE) {

      ## Prepare the data frame by arranging and setting colors
      dat <- DF %>%
        pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
        group_by(FILL) %>%
        summarise(COUNT = sum(COUNT, na.rm = T))  %>%
        mutate(FILL = factor(FILL, levels = breaks))%>%
        dplyr::mutate(ymax = cumsum(COUNT)) %>%
        dplyr::mutate(ymin = ymax -COUNT) %>%
        ungroup()


      plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
        ggplot2::geom_rect() +
        ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 5) +  ## Add this line to include count values
        ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
        ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
        ggplot2::labs(fill = LAB) +
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
        mutate(TOT = sum(COUNT, na.rm = T), .by = {{GROUPS}} )%>%
        mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
        mutate(FILL = factor(FILL, levels = breaks))%>%
        dplyr::mutate(ymax = cumsum(PERCENTAGE), .by = {{GROUPS}}) %>%
        dplyr::mutate(ymin = ymax -PERCENTAGE)

      plot <-ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
        ggplot2::geom_rect() +
        facet_wrap(vars({{GROUPS}}))+
        ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3) +  ## Add this line to include count values
        ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
        ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
        ggplot2::labs(fill = LAB) +
        ggplot2::theme_void() + ## removes the lines around chart and grey background
        ggplot2::theme(
          panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
          plot.background = element_rect(fill = "white", color = NA)  ## set plot background to white
        )
    }
  }


  ## if chart is bar:
  else {

    ord <-   DF %>%
      dplyr::pull({{GROUPS}})

    dat <- DF %>%
      pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
      mutate(TOT = sum(COUNT, na.rm = T), .by = {{GROUPS}} )%>%
      mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
      mutate(across(COUNT, ~na_if(., 0))) %>%
      mutate(FILL = factor(FILL, levels = breaks))

    if(NUM == TRUE){



      plot <-ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{GROUPS}}, level = ord), fill = FILL)) +
        ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
        ggplot2::geom_text(aes(label = COUNT),
                           position = position_stack(vjust = 0.5, reverse = TRUE), # add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
                           size = 3,
                           color = "black",
                           show.legend = FALSE) + # adjust size of labels with no legend being shown
        ggplot2::scale_fill_manual(values = cols, breaks = breaks)+  # order the colours of the bars in the reversed order
        ggplot2::ylab({{LAB}}) +
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

    ## if NUM == FALSE
    else {

      plot <- ggplot2::ggplot(dat, aes(y = PERCENTAGE, x = factor({{GROUPS}}, level = ord), fill = FILL)) +
        ggplot2::geom_bar(stat = "identity", position =  position_stack(reverse = TRUE), width = 0.5) + # change width of bars
        ggplot2::scale_fill_manual(values = cols, breaks = breaks) +  # order the colours of the bars in the reversed order
        ggplot2::ylab({{LAB}}) +
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

  ggsave(paste0("outputs/", SAVE, ".png"), height = 10, width = 16, units = 'cm')

  plot

}



test.2 <- function(DF,YEAR, RLI, min, max, GRP = FALSE){

  if(GRP == TRUE){

    ggplot2::ggplot(DF, aes(x = {{YEAR}}, y = {{RLI}}, group = {{GROUP}}, color = {{GROUP}})) +
      ggplot2::geom_line(linetype="dashed") +
      ggplot2::geom_ribbon(aes(ymin = {{min}}, ymax = {{max}}), fill = "grey", alpha = .2, colour = NA)+
      ggplot2::theme_classic()+
      ggplot2::ylim(0.7,1)

  }
  else {

    ggplot2::ggplot(DF, aes(x = {{YEAR}}, y = {{RLI}})) +
      ggplot2::geom_line(aes(y = {{RLI}})) +
      ggplot2::geom_ribbon(aes(ymin = {{min}}, ymax = {{max}}),alpha = .3, colour = NA)+
      ggplot2::theme_classic()+
      ggplot2::ylim(0.7,1)


  }

  ggsave(paste0("outputs/", "IRL", ".png", height = 10, width = 16, units = 'cm'))
}

test.3 <- function(DF){

  table <- kableExtra::kable(DF, "html", escape = FALSE) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 1) %>%
    kableExtra::row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black") %>% # purple header with black text and black borders
    kableExtra::column_spec(1:ncol(DF), border_left = TRUE, border_right = TRUE, background = "white") %>% # black border around columns
    kableExtra::add_header_above(c(" " = ncol(DF)), line = TRUE, line_sep = 3, color = "black") # black border around header
  table
}


#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)
