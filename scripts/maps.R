#####################################################################################
##
## Script name: maps
##
## Purpose of script: make mapping functions
##
## Author: Natasha Besseling and Maphale
##
## Date Created: 2025-04-15
##
##
## Notes: original code taken from Maphale
##
##
#####################################################################################
### packages & functions

# spatial data

library(tidyverse)
#library(maptools)
library(redlistr)
library(leaflet)
library(sf)
library(tmap)
library(createDB)

#####################################################################################
##map function


NBA_map <-function(DF, COLS, GEOM, CAP, FILL){



  dat <- DF %>%
    group_by(pick({{COLS}}))%>%
    summarise(geometry = st_union({{GEOM}})) %>%
    filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    ungroup()



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




  ## plot the 30% protection threshold map
  map <- ggplot() +

    geom_sf(data = dat,
            aes(fill = {{FILL}}),
            color = "grey",
            lwd = 0.1) +  # plot protection level and separate each protectipn level category in grey boundaries

    ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
    # theme_void() +
    labs(title = "",
         fill = "", ## legend title
         x = "",
         y = "",
         caption = CAP) +

    ggspatial::annotation_scale(location = "bl",            #location of the scale bar (br = bottom right)
                                width_hint = 0.1,
                                style= "bar") +         #proportion of plot that scalebar occupies

    ggspatial::annotation_north_arrow(location = "bl",                 #location of arrow (br = bottom right)
                                      which_north = "true",            #points to the north pole
                                      height = unit(0.8, "cm"),
                                      width = unit(0.8, "cm"),
                                      pad_x = unit(0.1, "in"),        #margin between arrow and map edge
                                      pad_y = unit(0.3, "in"),         #margin between arrow and map edge
                                      style = ggspatial::north_arrow_orienteering(text_size = 8)) +

    theme(legend.key.size = unit(0.5,"line"),
          legend.position = "inside",
          # legend.position.inside = c(.95, .95),
          legend.justification = c("right", "bottom"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))

}

######################################################################################

test <- NBA_map(mem,
                c(ecosystem_type, protection_level_30, threat_status, eco_poly_colour),
                geometry,
                "Figure 1. Map of the distribution of the ecosystems threat status",
                threat_status)

test

#####################################################################################
### settings

mem_3023_fl <- "Marine_Ecosystem_Map_2023_final_benthic_benthopelagic_only.gpkg"
year <- "2023"
year2 <- "2023"

##connect to database
con <- sql_con("mem_dwh")

####################################################################################
##load the 2023 ecosystem map


mem_crs <- st_read(dir("data",
                        mem_3023_fl,
                        full.names = T,
                        recursive = T)[1])




#####
## load mem
#####

##connect to database
con <- sql_con("mem_dwh")


mem <- st_read(dsn = con, query = paste0("SELECT ecosystem_type, protection_level, threat_status, eco_poly_colour, geometry FROM dim_poly LEFT JOIN fact_ecosystem ON  fk_poly_b_ecosystem_id = pk_ecosystem_id LEFT JOIN dim_protection_level ON fk_eco_protection_level_30_",year, "_id  = pk_protection_level_id LEFT JOIN dim_threat_status ON fk_eco_threat_status_id_",year, "  = pk_threat_status_id WHERE version = ", "'", year, "'"))%>%
  filter(!is.na(protection_level)) %>%
  rename(protection_level_30 = protection_level)

DBI::dbDisconnect(con)

if (length(st_is_valid(mem, reason=T)[which(!st_is_valid(mem))]) > 0){
  mem <- st_make_valid(mem) # fix them
}


####################################################################################
## combine polygons to one polygon per ecosystem

mem_2023 <- mem %>%
  group_by(ecosystem_type, protection_level_30, threat_status, eco_poly_colour)%>%
  summarise(geometry = st_union(geometry)) %>%
  filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  ungroup() %>%
  st_transform(crs = st_crs(mem_crs))

sub <- mem_2023 %>%
  dplyr::slice_head(n = 20)

######################################################################################
##map threat status

a <- tm_shape(sub) +
  tm_polygons("threat_status",
              fill.scale = tm_scale(
                values = c("red", "orange", "yellow", "beige", "#B4D79E"),
                labels = c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")
              ),
              fill.legend = tm_legend("Red List of Ecosystems Category")  ,
              col = "black",
              col_alpha = 0.5,
              legend.show = TRUE
  )+
  tm_title("Figure 1a: Map showing the distribution of threatened ecosystem types in their historical extent",
           size = 0.9,
           position = "left") +
  tm_layout(legend.text.size = 0.7,
    legend.title.size = 0.9,
    #legend.outside = FALSE,
    frame = TRUE
  ) +
  tm_grid(lwd = 0.05,
          alpha = 0.2,
          labels.show = FALSE)+
  tm_compass(type="arrow",
             position = c("right", "bottom"),
             size = 0.7)

a

b <- tm_shape(EcoMap) +
  tm_fill("Red List of Ecosystems Category",
          palette = c("red","orange","yellow","#B4D79E"),
          labels = c("Critically Endangered", "Endangered", "Vulnerable", "Least Concern", "Not nutaral"),
          legend.show = TRUE) +

  tm_shape(HM60) +
  tm_raster(style = "cont",
            palette = "gray97",
            labels = "Not Natural",
            title = "",
            colorNA = NULL) +

  tm_shape(Excl_SA) +
  tm_fill("white") +

  tm_shape(Province) +
  tm_borders(col='black',
             lwd = 0.75) +

  tm_layout(main.title = "Figure 1b: Map showing the modified natural patches",
            main.title.position = "left",
            main.title.size = 0.9,
            legend.text.size = 0.7,
            legend.title.size = 0.9,
            #legend.outside = TRUE,
            frame = TRUE) +
  tm_grid(lwd = 0.05,
          alpha = 0.2,
          labels.show = FALSE) +
  tm_scale_bar(position = c("right", "bottom"),
               width = 5,
               size = 5) +

  tm_layout(frame = FALSE)

tmap_arrange(a, b)


####################################################################################



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




## plot the 30% protection threshold map
benth_prot_map_30 <- ggplot() +

  geom_sf(data = mem_2023,
          aes(fill = protection_level_30),
          color = "grey",
          lwd = 0.1) +  # plot protection level and separate each protectipn level category in grey boundaries

  ggplot2::scale_fill_manual(values = cols, breaks = breaks) +
 # theme_void() +
  labs(title = "",
       fill = "", ## legend title
       x = "",
       y = "",
       caption = "CAP") +

  ggspatial::annotation_scale(location = "bl",            #location of the scale bar (br = bottom right)
                              width_hint = 0.1,
                              style= "bar") +         #proportion of plot that scalebar occupies

  ggspatial::annotation_north_arrow(location = "bl",                 #location of arrow (br = bottom right)
                                    which_north = "true",            #points to the north pole
                                    height = unit(0.8, "cm"),
                                    width = unit(0.8, "cm"),
                                    pad_x = unit(0.1, "in"),        #margin between arrow and map edge
                                    pad_y = unit(0.3, "in"),         #margin between arrow and map edge
                                    style = ggspatial::north_arrow_orienteering(text_size = 8)) +

  theme(legend.key.size = unit(0.5,"line"),
        legend.position = "inside",
        # legend.position.inside = c(.95, .95),
        legend.justification = c("right", "bottom"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

benth_prot_map_30


ggsave("outputs/threat_status_map_benthic.png", plot= benth_threat_map,  bg = "white")


#####################################################################################
##map rasters with polygons
##turn raster into tibble
gplot_data <- function(x, maxpixels = 50000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')

  dat <- tibble::as_tibble(data.frame(coords, dat))

  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}


##load data
rast <- raster::raster(
  dir("data",
      "^Mining Footprint.tif$",
      full.names = T,
      recursive = T))

rast <- terra::rast(
  dir("data",
      "^Mining Footprint.tif$",
      full.names = T,
      recursive = T))




poly <- NBA.package::NBA_example_map_data %>%
  sf::st_transform(crs = sf::st_crs(rast))


cum_pres <- gplot_data(rast)


#########
##plot in ggplot
########
library(ggplot2)

ggplot() +
  geom_tile(data = cum_pres,
            aes(x = x, y = y, fill = value)) +
  geom_sf(data = poly, fill = NA) +
  scale_fill_gradient("Pressure",
                      low = 'green', high = 'red',
                      na.value = NA)

ggplot() +
  tidyterra::geom_spatraster(data = rast) +
  geom_sf(data = poly, fill = NA) +
  scale_fill_gradient("Pressure",
                      low = 'green', high = 'red',
                      na.value = NA)





##############################################################################
## recreate mem style file
library(xml2)
library(dplyr)
library(purrr)
library(ggplot2)
library(sf)
library(ggpattern)


mem_2025 <- sf::st_read(
  dir("data",
      "^Marine_Ecosystem_Map_2025_interim.gpkg$",
      full.names = T,
      recursive = T))
mem_2025



qml <- read_xml(dir("data",
                    "^Marine_Ecosystem_Map_2025_benthic.qml$",
                    full.names = T,
                    recursive = T))
qml


rules <- xml_find_all(qml, ".//rule")

rule_data <- tibble(
  name = xml_attr(rules, "symbol"),  # symbol ID
  B_EcosysType = xml_attr(rules, "label")
)

# Remove "<all other values>" if it’s a placeholder
rule_data <- filter(rule_data, B_EcosysType != "<all other values>")

symbols_raw <- xml_find_all(qml, ".//symbol")
symbols_raw


# Extract relevant data from each symbol
symbol_data <- map_df(symbols_raw, function(sym) {
  type <- xml_attr(sym, "type")
  name <- xml_attr(sym, "name")

  # Extract first layer node
  layer <- xml_find_first(sym, ".//layer")
  class <- xml_attr(layer, "class")

  # Extract all style options under the layer
  options <- xml_find_all(layer, ".//Option")
  option_list <- map_chr(options, ~ paste(xml_attr(.x, "name"), xml_attr(.x, "value"), sep = "="))

  tibble(
    name = name,
    type = type,
    class = class,
    style = paste(option_list, collapse = "; ")
  )
})

symbol_styles <- left_join(rule_data, symbol_data, by = "name")
symbol_styles

# Extract hex color codes for SimpleFill types
SimpleFill <- symbol_styles %>%
  mutate(
    fill_color = str_extract(style, "color=\\d+,\\d+,\\d+,\\d+"),
    fill_color = str_remove(fill_color, "color=")
  )%>%
  filter(class == "SimpleFill" & !is.na(fill_color))%>%
  mutate(
    hex = str_split(fill_color, ",", simplify = TRUE),
    hex = rgb(
      red   = as.numeric(hex[,1])/255,
      green = as.numeric(hex[,2])/255,
      blue  = as.numeric(hex[,3])/255,
      alpha = as.numeric(hex[,4])/255
    )
  )
SimpleFill

# Extract hex color codes for SimpleLine types
SimpleLine <- symbol_styles %>%
  mutate(
    fill_color = str_extract(style, "line_color=\\d+,\\d+,\\d+,\\d+"),
    fill_color = str_remove(fill_color, "line_color=")
  )%>%
  filter(class == "LinePatternFill" & !is.na(fill_color))%>%
  mutate(
    hex = str_split(fill_color, ",", simplify = TRUE),
    hex = rgb(
      red   = as.numeric(hex[,1])/255,
      green = as.numeric(hex[,2])/255,
      blue  = as.numeric(hex[,3])/255,
      alpha = as.numeric(hex[,4])/255
    )
  )
SimpleLine

symbol_styles <- SimpleFill %>%
  bind_rows(SimpleLine)
symbol_styles

# Join your symbol styles to your spatial data
shp <- left_join(mem_2025, symbol_styles, by = "B_EcosysType")

#3. Split data by class
simple_fill <- shp %>% filter(class == "SimpleFill")
line_pattern <- shp %>% filter(class == "LinePatternFill")

# Now plot with ggplot2
ggplot(shp) +
  geom_sf(aes(fill = hex), color = NA) +
  scale_fill_identity() +
  theme_minimal()


ggplot() +
  # Simple solid fill polygons
#  geom_sf(data = simple_fill, aes(fill = hex), color = NA) +

  # Line pattern polygons
  geom_sf_pattern(
    data = line_pattern,
    aes(pattern_colour = hex),
    pattern = "stripe",
    pattern_fill = NA,        # transparent base fill
    pattern_density = 0.5,
    pattern_spacing = 0.05,
    pattern_angle = 45,
    pattern_size = 0.2,
    fill = NA,                # no base fill
    color = NA                # no border
  ) +

  # Use colors as-is
  scale_fill_identity() +
  scale_pattern_colour_identity() +

  theme_minimal()




ggsave("outputs/Marine_Ecosystem_Map_2025_interim.png",height = 10, width = 16, units = 'cm',  bg = "white")
















####################################################################################


## install if needed (do this exactly once):
## install.packages("usethis")

library(usethis)
use_git_config(user.name = "TashBes", user.email = "N.besseling@sanbi.org.za")
usethis::create_github_token()
gitcreds::gitcreds_set()

library(hexSticker)

library(ggplot2)
p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p <- p + theme_void() + theme_transparent()
p
outfile <- tempfile(fileext=".png")
test <- sticker(p, package="hexSticker", filename=outfile)
test


