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
library(kableExtra)


#####################################################################################
rgb()

### Create the function to create horizontal barplots for the threat status of ecosystem function groups (efgs)
thr_efg_plot <-function(DAT, X, Y, FILL )

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
}

### Create the function for the donut plots for the threat status of the ecosystem types
thr_donut_plot <-function(data, ymax, ymin, fill)

{
 ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Threat_status_2023)) +
    geom_rect() +
    geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = Frequency), color = "black", size = 5)+  ## Add this line to include values
    coord_polar(theta="y") + ## try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) + ## try to remove that to see how to make a pie chart
    scale_fill_manual(values = freq_df$Cols, breaks = freq_df$Threat_status_2023) +
    labs(fill = "Threat Status") +
    theme_void() ## removes the lines around chart and grey background

}

### Create the function to create horizontal barplots for the threat status of ecosystem function groups (efgs)
prot_donut_plt <-function(data, ymax, ymin, fill)

{

ggplot(freq_df2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Protection_level_2023)) +
  geom_rect() +
  geom_text(aes(x = 3.5, y = (ymin + ymax) / 2, label = Frequency), color = "black", size = 5)+  ## Add this line to include values
  coord_polar(theta="y") + ## try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + ## try to remove that to see how to make a pie chart
  scale_fill_manual(values = freq_df2$Cols, breaks = freq_df2$Protection_level_2023) +
  labs(fill = "Protection Levels") +
  theme_void() ## removes the lines around chart and grey background

}

### Create the function to create horizontal barplots for the protection of ecosystem function groups (efgs)
prot_efg_plot <-function(DAT, X, Y, FILL )

{
  ggplot(DAT, aes(y = Y, x = X, fill = FILL)) +
    geom_bar(stat = "identity", width = 0.5) + ## change width of bars
    geom_text(aes(label = count), position = position_stack(vjust = 0.5),## add count labels to the bars and adjust "vjust" value to place text at the beginning, centre or end of bars
              size = 3, color = "black", show.legend = FALSE) + ## adjust size of labels with no legend being shown
    scale_fill_manual(values = c("#a4a3a3", "#d5dec3", "#80a952", "#466a31")) +  ## order the colours of the bars in the reversed order
    ylab("Percent protected (%)") +
    xlab("") +
    guides(fill = guide_legend(reverse = TRUE)) + ## reverse the order of the legend to match the plots order of categories
    labs(fill = "Protection Levels") + ## change the legend title
    theme_bw() + ## create a black bounding box around the plot
    theme(legend.position = "bottom") + ## position the legend to the beneath the plot
    coord_flip()  ## flip the orientation of the chart
}




######################################################################################
####################################
## make the bubble plot

bubble <- read_csv("nba_bubble_plot_example_data.csv")
bubble

library(dplyr)
library(ggplot2)
library(RColorBrewer)

modify_facet_appearance <- function(plot = NULL,
                                    strip.background.y.fill = NULL,
                                    strip.background.y.col = NULL,
                                    strip.text.y.col = NULL){

  if(is.null(plot)){stop("A ggplot (gg class) needs to be provided!")}

  # Generate gtable object to modify the facet strips:
  g <- ggplot_gtable(ggplot_build(plot))

  # Get the locations of the right and top facets in g:
  stripy <- which(grepl('strip-r|strip-l', g$layout$name)) # account for when strip positions are switched r-l and/or t-b in facet_grid(switch = )


  # Check that the provided value arrays have the same length as strips the plot has:
  ly <- c(length(strip.background.y.fill), length(strip.background.y.col), length(strip.text.y.col))
  if(!all(ly==length(stripy) | ly==0)){stop("The provided vectors with values need to have the same length and the number of facets in the plot!")}

  # Change the strips on the y axis:
  for (i in seq_along(stripy)){ # if no strips in the right, the loop will not be executed as seq_along(stripy) will be integer(0)

    # Change strip fill and (border) colour :
    j1 <- which(grepl('strip.background.y', g$grobs[[stripy[i]]]$grobs[[1]]$childrenOrder))
    if(!is.null(strip.background.y.fill[i])){g$grobs[[stripy[i]]]$grobs[[1]]$children[[j1]]$gp$fill <- strip.background.y.fill[i]} # fill
    if(!is.null(strip.background.y.col[i])){g$grobs[[stripy[i]]]$grobs[[1]]$children[[j1]]$gp$col <- strip.background.y.col[i]} # border colour

    # Change color of text:
    j2 <- which(grepl('strip.text.y', g$grobs[[stripy[i]]]$grobs[[1]]$childrenOrder))
    if(!is.null(strip.text.y.col[i])){g$grobs[[stripy[i]]]$grobs[[1]]$children[[j2]]$children[[1]]$gp$col <- strip.text.y.col[i]}

  }


  return(g)

  # Note that it returns a gtable object. This can be ploted with plot() or grid::draw.grid().
  # patchwork can handle the addition of such gtable to a layout with other ggplot objects,
  # but be sure to use patchwork::wrap_ggplot_grob(g) for proper alignment of plots!
  # See: https://patchwork.data-imaginist.com/reference/wrap_ggplot_grob.html

}

pal.y <- brewer.pal(length(unique(bubble$pressure)), "BrBG")



p <- {ggplot(bubble, aes(taxon_group, sub_pressure, size=perc_concern_under_press, fill = pressure,
                         colour = pressure)) +
  geom_point(shape=21) +
  scale_size(range = c(.1, 17)) +
  scale_fill_brewer(palette = "BrBG")+
  scale_colour_brewer(palette = "BrBG")+
  ylab("") +
  xlab("") +
  scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) +
  facet_grid(pressure ~ ., scales = "free", space = "free",
             labeller = labeller( pressure= label_wrap_gen(width = 20))) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        #panel.border = element_rect(color = "black", fill = NA),
        panel.spacing = unit(0, "cm"),
        strip.background = element_rect(),
        strip.text.y = element_text(angle = 0),
        panel.grid.major.y = element_line(colour = "lightgrey"),
        panel.grid.major.x = element_line(colour = "lightgrey"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(vjust = grid::unit(c(-2, 0), "points")))}%>%
  modify_facet_appearance(strip.background.y.fill = pal.y,
                          strip.background.y.col = pal.y)

as.ggplot(p)

plot(p)

ggsave("outputs/bubble_plot.png")

display.brewer.all()
#####################################################################################
##as a ggplot object

#install.packages("ggh4x")
library(ggh4x)
library(RColorBrewer)

# Create a named color palette for pressures
pressures <- unique(bubble$pressure)
pressures
# Subset col_mapping using names that match the values in pressures
subset_col_mapping <- col_mapping[match(pressures, names(col_mapping))]


# Create strip background and text style lists
strip_bg <- lapply(subset_col_mapping, function(col) element_rect(fill = col, colour = col))
strip_text <- lapply(subset_col_mapping, function(col) element_text(colour = "black"))  # or custom color

# Build strip object
my_strips <- ggh4x::strip_themed(
  background_y = strip_bg,
  text_y = strip_text
)



library(ggplot2)

p <- ggplot(bubble, aes(taxon_group, sub_pressure, size = perc_concern_under_press,
                        fill = pressure, colour = pressure)) +
  geom_point(shape = 21) +
  scale_size(range = c(.1, 15)) +
  scale_fill_manual(values = col_mapping) +
  scale_colour_manual(values = col_mapping) +
  ylab("") +
  xlab("") +
  geom_text(aes(label = perc_concern_under_press),
            parse = TRUE,
            size = 2,
            colour = "black") +
  scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) +
  ggh4x::facet_grid2(
    pressure ~ ., scales = "free", space = "free",
    labeller = labeller(pressure = label_wrap_gen(width = 20)),
    strip = my_strips
  ) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.spacing = unit(0, "cm"),
    strip.text.y = element_text(angle = 0),
    panel.grid.major.y = element_line(colour = "lightgrey"),
    panel.grid.major.x = element_line(colour = "lightgrey"),
    axis.ticks.y = element_blank(),
    axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
    axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black")
  )



p


ggsave("outputs/bubble_plot_strips_ggplot.png", plot = p, width = 10, height = 8, dpi = 300)


#########################################################
##as a function

NBA_plot_bubble <- function(DF, GROUP, CAT, SUB_CAT, VALUE, SAVE){


  # Create a named color palette for pressures
  cat <- DF %>%
    dplyr::select({{CAT}}) %>%
    unique() %>%
    as.data.frame()
  cat <- cat[,1]

  pal.y <- setNames(brewer.pal(length(cat), "BrBG"), cat)


  # Create strip background and text style lists
  strip_bg <- lapply(pal.y, function(col) element_rect(fill = col, colour = col))
  strip_text <- lapply(pal.y, function(col) element_text(colour = "black"))  # or custom color

  # Build strip object
  my_strips <- ggh4x::strip_themed(
    background_y = strip_bg,
    text_y = strip_text
  )


  p <- DF %>%
    ggplot(aes({{GROUP}}, {{SUB_CAT}}, size = {{VALUE}},
                          fill = {{CAT}}, colour = {{CAT}})) +
    geom_point(shape = 21) +
    scale_size(range = c(.1, 15)) +
    scale_fill_brewer(palette = "BrBG") +
    scale_colour_brewer(palette = "BrBG") +
    ylab("") +
    xlab("") +
    geom_text(aes(label = {{VALUE}}),
              parse = TRUE,
              size = 2,
              colour = "black") +
    scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) +
    ggh4x::facet_grid2(
      pressure ~ ., scales = "free", space = "free",
      labeller = labeller(pressure = label_wrap_gen(width = 20)),
      strip = my_strips
    ) +
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      panel.spacing = unit(0, "cm"),
      strip.text.y = element_text(angle = 0),
      panel.grid.major.y = element_line(colour = "lightgrey"),
      panel.grid.major.x = element_line(colour = "lightgrey"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
      axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"))





  if(!is.null(SAVE)){

  ggsave(paste0("outputs/", SAVE, ".png"), plot = p, width = 10, height = 8, dpi = 300)


}
  p
}


test <- NBA_plot_bubble(DF = bubble,
                        GROUP = taxon_group,
                        CAT = pressure,
                        SUB_CAT = sub_pressure,
                        VALUE = perc_concern_under_press,
                        SAVE = "bubble_plot_test")

test

### unload packages

# detach("package:xxx", unload=TRUE)
