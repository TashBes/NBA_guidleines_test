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

test <- function(DF, GROUPS, COLS, CHRT = c("bar", "donut"), NUM = FALSE, LAB, GRP = TRUE, SAVE = NULL){


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
            "#b1d798",
            "brown",
            "grey" ,
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
              "Least Concern",
              "Data Deficient",
              "Rare",
              "Cropland",
              "Plantation",
              "Built up",
              "Mine",
              "Artificial waterbody")

  if(CHRT == "donut"){

    if(GRP == FALSE) {

      ## Prepare the data frame by arranging and setting colors
      dat <- DF %>%
        tidyr::pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
        dplyr::group_by(FILL) %>%
        dplyr::summarise(COUNT = sum(COUNT, na.rm = T), .by = FILL)  %>%
        dplyr::mutate(FILL = factor(FILL, levels = breaks))%>%
        dplyr::mutate(ymax = cumsum(COUNT)) %>%
        dplyr::mutate(ymin = ymax -COUNT) %>%
        dplyr::ungroup()


      if(NUM == FALSE){


        plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
          ggplot2::geom_rect() +
          #ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 5) +  ## Add this line to include count values
          ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
          ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
          #ggplot2::ggtitle(LAB)+
          ggplot2::labs(fill = "", title = LAB) + #this is the legend label
          ggplot2::theme_void() + ## removes the lines around chart and grey background
          ggplot2::theme(
            panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
            plot.background = element_rect(fill = "white", color = NA),
            title = element_text(size = 10),
            strip.text = element_blank()## set plot background to white
          )

      }

      #if NUm is true
      else{

        plot <- ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
          ggplot2::geom_rect() +
          ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 5) +  ## Add this line to include count values
          ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
          ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
          ggplot2::labs(fill = "", title = LAB)+
          #ggplot2::xlab(LAB)+
          ggplot2::theme_void() + ## removes the lines around chart and grey background
          ggplot2::theme(
            panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
            plot.background = element_rect(fill = "white", color = NA),
            title = element_text(size = 10),
            strip.text = element_blank()  ## set plot background to white
          )

      }
    }

    #if grp is true
    else {

      ## Prepare the data frame by arranging and setting colors
      dat <- DF %>%
        tidyr::pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
        dplyr::mutate(TOT = sum(COUNT, na.rm = T), .by = {{GROUPS}} )%>%
        dplyr::mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
        dplyr::mutate(FILL = factor(FILL, levels = breaks))%>%
        dplyr::mutate(ymax = cumsum(PERCENTAGE), .by = {{GROUPS}}) %>%
        dplyr::mutate(ymin = ymax -PERCENTAGE)

      if(NUM == FALSE){


        plot <-ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
          ggplot2::geom_rect() +
          ggplot2::facet_wrap(vars({{GROUPS}}))+
          #ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3) +  ## Add this line to include count values
          ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
          ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
          ggplot2::labs(fill = "", title = LAB)+
          #ggplot2::xlab(LAB)+
          ggplot2::theme_void() + ## removes the lines around chart and grey background
          ggplot2::theme(
            panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
            plot.background = element_rect(fill = "white", color = NA),
            title = element_text(size = 10),
            strip.text = element_blank()  ## set plot background to white
          )
      }

      #if Num is true
      else{

        plot <-ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
          ggplot2::geom_rect() +
          ggplot2::facet_wrap(vars({{GROUPS}}))+
          ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 3) +  ## Add this line to include count values
          ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
          ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
          ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
          ggplot2::labs(fill = "", title = LAB)+
          #ggplot2::xlab(LAB)+
          ggplot2::theme_void() + ## removes the lines around chart and grey background
          ggplot2::theme(
            panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
            plot.background = element_rect(fill = "white", color = NA),
            title = element_text(size = 10),
            strip.text = element_blank()  ## set plot background to white
          )

      }

    }
  }


  ## if chart is bar:
  else {

    ord <-   DF %>%
      dplyr::pull({{GROUPS}})

    dat <- DF %>%
      tidyr::pivot_longer({{COLS}}, names_to = "FILL", values_to = "COUNT")%>%
      dplyr::mutate(TOT = sum(COUNT, na.rm = T), .by = {{GROUPS}} )%>%
      dplyr::mutate(PERCENTAGE = (COUNT/TOT)*100)%>%
      dplyr::mutate(dplyr::across(COUNT, ~na_if(., 0))) %>%
      dplyr::mutate(FILL = factor(FILL, levels = breaks))

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

Fig99mapinset <- read_excel(
  dir("data",
      "Fig99mapinset_graph.xlsx",
      full.names = T,
      recursive = T))%>%
  slice_head(n =8)%>%
  mutate(across(2:5, as.numeric)) %>%
  select(1:5)

test(Fig99mapinset,
     `OVERALL types`,
     COLS = 2:5,
     NUM = F,
     GRP = F,
     CHRT = "donut",
     LAB = "Protection level",
     SAVE = "Fig99mapinset")

#############################################################################
##try with marine

mem <- sf::read_sf(list.files
                   (path="data",
                     pattern="Marine_Ecosystem_Map_2023_final_benthic_benthopelagic_only.gpkg",
                     recursive = T,
                     full.names = T)[1])

pro_thr <- readxl::read_xlsx(list.files
                    (path="data",
                      pattern="Marine_Ecosystem_Map_2023_MarineDescriptiveFields_LinkTable_forKerry_Avril_KS.xlsx",
                      recursive = T,
                      full.names = T)[1])

eez <- sf::read_sf(list.files(path="data",
                     pattern="^eez_mainlandRSA_buffered_1km_allEEZversions.gpkg$",
                     recursive = T,
                     full.names = T)[1]) # pattern="^eez_MEM2018.gpkg$",

mem <- mem %>%
  left_join(select(pro_thr, EcosystemType, `2023_threat_status`),
            by = c("B_EcosysType" = "EcosystemType")) %>%
  slice_tail(n = 1000)




library(tmap)

names(mem)[names(mem) == "2023_threat_status"] = "Red List of Ecosystems Category"



a <- tm_shape(mem) +
  tm_fill("Red List of Ecosystems Category",
          fill.scale = tm_scale(values= c("red","orange","yellow","#B4D79E")),
          fill.legend = tm_legend_hide())+
  # tm_shape(eez) +
  # tm_fill("white")+
  tm_shape(mem) +
  tm_borders(col='black',
             lwd = 0.7) +
  tm_title(
    "Figure 1a: Map showing the distribution of threatened ecosystem types in their historical extent",
    position = c("left", "top"),
    size = 0.9
  ) +
  tm_layout(
    frame = TRUE
  )+
  tm_grid(lwd = 0.05,
          alpha = 0.2,
          labels.show = FALSE) +
  tm_compass(type="arrow",
             position = c("right", "bottom"),
             size = 0.7) +
  tm_layout(frame = FALSE)

a


b <- tm_shape(mem) +
  tm_fill("Red List of Ecosystems Category",
          fill.scale = tm_scale(values= c("red","orange","yellow","#B4D79E"),
                                labels = c("Critically Endangered", "Endangered", "Vulnerable", "Least Concern", "Not nutaral")),
          fill.legend = tm_add_legend(),
          legend.show = TRUE)+

  # tm_shape(mem) +
  # tm_raster(style = "cont",
  #           palette = "gray97",
  #           labels = "Not Natural",
  #           title = "",
  #           colorNA = NULL) +
  #
  # tm_shape(Excl_SA) +
  # tm_fill("white") +

  tm_shape(mem) +
  tm_borders(col='black',
             lwd = 0.75) +
  tm_title(
    "Figure 1a: Map showing the distribution of threatened ecosystem types in their historical extent",
    position = c("left", "top"),
    size = 0.9
  ) +
  tm_legend(text.size = 0.7,
            title.size = 0.9)+
  tm_layout(
    frame = TRUE
  )+

  tm_grid(lwd = 0.05,
          alpha = 0.2,
          labels.show = FALSE) +
  tm_scalebar(position = c("right", "bottom"),
               width = 5,
              text.size = 5) +

  tm_layout(frame = FALSE)

b

tmap_arrange(a, b)



dat <- Fig99mapinset %>%
  tidyr::pivot_longer(2:5, names_to = "FILL", values_to = "COUNT")%>%
  dplyr::group_by(FILL) %>%
  dplyr::summarise(COUNT = sum(COUNT, na.rm = T))%>%
  dplyr::mutate(FILL = factor(FILL, levels = breaks))%>%
  dplyr::mutate(ymax = cumsum(COUNT)) %>%
  dplyr::mutate(ymin = ymax -COUNT) %>%
  dplyr::ungroup()


dat <- Fig99mapinset %>%
  tidyr::pivot_longer(2:5, names_to = "FILL", values_to = "COUNT") %>%
  dplyr::summarise(COUNT = sum(COUNT, na.rm = TRUE), .by = FILL) %>%
  # Ensure FILL is a factor with levels in the desired order, using `breaks`
  dplyr::mutate(FILL = factor(FILL, levels = breaks)) %>%
  dplyr::mutate(ymax = cumsum(COUNT)) %>%
  dplyr::mutate(ymin = ymax - COUNT) %>%
  dplyr::ungroup()

# Check the levels of FILL in dat to confirm the correct order
dat$FILL

ggplot2::ggplot(dat, aes(ymax = ymax, ymin = ymin,xmax = 4, xmin = 3,  fill = FILL)) +
  ggplot2::geom_rect()+
 ggplot2::geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = COUNT), color = "black", size = 5) +  ## Add this line to include count values
ggplot2::coord_polar(theta = "y") + ## convert to polar coordinates
ggplot2::xlim(c(2, 4)) + ## limit x-axis to create a donut chart
  ggplot2::scale_fill_manual(values = values)+
  ggplot2::labs(fill = "", title = "Protection level")+
  #ggplot2::xlab(LAB)+
  ggplot2::theme_void() + ## removes the lines around chart and grey background
  ggplot2::theme(
    panel.background = element_rect(fill = "white", color = NA),  ## set panel background to white
    plot.background = element_rect(fill = "white", color = NA),
    title = element_text(size = 10),
    strip.text = element_blank()  ## set plot background to white
  )


###########################################################
##table

NBA_colr_tbl <- function(DF, COL) {

  color_cell <- function(COL) {
    color <- dplyr::case_when(
      COL == "Well Protected" ~ "#466a31",
      COL == "Moderately Protected" ~ "#80a952",
      COL == "Poorly Protected" ~ "#d5dec3",
      COL == "No Protection" ~ "#a4a3a3",
      COL == "Natural" ~ "#6e9fd4",
      COL == "Natural/near-natural" ~ "#6e9fd4",
      COL == "Near-natural" ~ "#a5c5c7",
      COL == "Moderately modified" ~ "#81aba7",
      COL == "Heavily modified" ~ "#88814e",
      COL == "Severely/critically modified" ~ "#88812e",
      COL == "Not Protected" ~ "#a4a3a3",
      COL == "Extinct" ~ "black",
      COL == "Critically Endangered" ~ "#e9302c",
      COL == "Endangered" ~ "#f97835",
      COL == "Vulnerable" ~ "#fff02a",
      COL == "Near Threatened" ~ "#eeeea3",
      COL == "Data Deficient" ~ "brown",
      COL == "Rare" ~  "grey",
      COL == "Least Concern" ~ "#b1d798",
      COL == "Cropland" ~ "#DB7D15",
      COL == "Plantation" ~ "#B36611",
      COL == "Built up" ~ "#808080",
      COL == "Mine" ~ "#F5C592",
      COL == "Artificial waterbody" ~ "#0071C0",
      TRUE ~ "white"
    )

    html <- paste0(
      '<div style="background-color:', color, '; color: black; padding: 5px;">',
      COL, '</div>'
    )

    return(html)
  }

  # Apply the HTML function to the Status column
  DF_col <- DF %>%
    dplyr::mutate({{COL}} = sapply({{COL}}, color_cell))

  table <- kableExtra::kable(DF_col, "html", escape = FALSE) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 12
    ) %>%
    kableExtra::column_spec(1:ncol(DF_col), border_left = TRUE, border_right = TRUE, background = "white") %>%
    kableExtra::add_header_above(c(" " = ncol(DF_col)), line = TRUE, line_sep = 3, color = "black")%>%
    kableExtra::row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black")
  table

}


library(dplyr)

tbl <- NBA_example_pro_data %>%
  pivot_longer(2:5, names_to = "protection_level") %>%
  NBA_colr_tbl(COL = protection_level)

tbl

#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)
