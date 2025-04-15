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
library(maptools)
library(redlistr)
library(leaflet)
library(sf)
library(tmap)
library(createDB)

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
              border.col = "black",
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
st_crs(sub)

tmap_options(component.autoscale = FALSE)

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



#####################################################################################
