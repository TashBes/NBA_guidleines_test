## make maps


# read the sptial data
RLE.PLterr <- sf::read_sf("Data/RLE_2022.shp")
Modified <- terra::rast("Data/Notnat_2022nlc4class.tif")
LC.change <- terra::rast("Data/nlc2022sn_4class.tif")
Province <- sf::read_sf("Data/Province_New_SANeighbours_Albs.shp")
LES_SWAZ <- sf::read_sf("Data/Lesotho_eSwatini.shp")

library(tmap)

# Step 1: Use tmap function to create RLE map
RLEmap <- tm_shape(RLE.PLterr) + # dataset that will be visualized
  tm_polygons(fill = "RLEstatus", # determines the fill color of each polygon
              fill.scale = tm_scale(values = c("Critically Endangered" = "red",
                                               "Endangered" = "orange",
                                               "Vulnerable" = "yellow",
                                               "Least Concern" = "#B4D79E"),label.na = ""),
              col_alpha = 0,
              fill.legend = tm_legend(title = "IUCN RLE risk categories")) +

  tm_shape(LES_SWAZ) +
  tm_borders(col = "white", lwd = 0.2) + # applies to the interior of polygons.
  tm_fill(col = NULL, fill = "white") + # applies to the border/outline of those polygons.

  tm_shape(Province) +
  tm_borders(col='black', lwd = 0.5) +

  tm_layout(asp = 0.9, # Shrinks the tmap
            frame = FALSE,
            legend.show = FALSE)

print(RLEmap)

test_map <- NBA_map(RLE.PLterr,
                          GEOM = geometry,
                          CAP = "Figure 1. Map of threatened ecosystems (IUCN Red List Ecosystems), showing historical extent (National Vegetation Map 2024) and an inset graph of threat category percentages",
                          FILL = RLEstatus)+
  geom_sf(data = LES_SWAZ, col = "white", fill = "white", lwd = 0.5)+
  geom_sf(data = Province, col = "black", lwd = 0.5, fill = NA)+
  theme(panel.border = element_blank())

test_map

RLE.PLterr$NAME

# Step 2: Create a donut chart

RLEdonut_chart <- ggplot(RLE.stats, aes(x = 2, # x = 2 causes all the bar segments to stack on top of each other, forming a single stacked column
                                        y = RLE.count, # Defines the height of each bar segment based on count values.
                                        fill = RLE.2022)) +

  geom_bar(stat = "identity",  # ggplot to use the exact y-values you provide (not to count anything)
           width = 1.1,
           color = "white") + # adds a white border between segments for visual separation
  coord_polar(theta = "y") +  # changes the shape of your plot from a normal bar chart to a pie chart  C
  scale_fill_manual(values = c("Critically Endangered" = "red",
                               "Endangered" = "orange",
                               "Vulnerable" = "yellow",
                               "Least Concern" = "#B4D79E"))  +

  theme_void() + # Removes all default axes, gridlines, background, and labels
  theme(text = element_text(size = 9)) +
  theme(legend.key.size = unit(0.5, 'cm'),
        legend.position = c(-2, 1.76)) +
  geom_text(aes(label = paste0(round(RLE.perc, 0), "%")),
            position = position_stack(vjust = 0.5), #  Vertically centers the labels within each slice
            size = 2.8, color = "black") +
  annotate("text", x = 0, y = 0, label = "466\nEcosystem\ntypes", size = 2.7, color = "black")


# Convert to a grob (graphical object)
donut_grob <- ggplotGrob(RLEdonut_chart)

# Step 3: creates viewport where the donut chart will be placed.
vp <- grid::viewport(x = 1.03, y = 0.07, width = 0.47, height = 0.39, just = c("right", "bottom"))
grid::pushViewport(vp) # Activates the viewport, meaning all subsequent drawing operations will be placed within this viewport.
grid::grid.draw(donut_grob) # Draws the donut chart created earlier inside the viewport.
grid::popViewport()



