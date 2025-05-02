#######################################################################################
##
## Script name: benth_eco_calc.R
##
## Purpose of script:
## To create a standardised template for a table used in the NBA
##
## Author: Lauryn Bull
##
## Date Created: 2023-03-17
##
## Notes:
## This script is used to create and formualtae a specific table format for majority of tables

########################################################################################
## packages & functions

library(tidyverse)
library(sf)
library(kableExtra)

########################################################################################
### Create example data

bird_data <- data.frame(
  Species = c("African Fish Eagle", "Saddle-billed Stork", "Kori Bustard", "Secretary Bird", "Crowned Crane"),
  Habitat = c("Wetland", "Savanna", "Grassland", "Savanna", "Wetland"),
  Status = c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered")
)

land_cover <- data.frame(
  Land_cover = c("Natural", "Cropland", "Plantation", "Built-up", "Mine", "Artificial water body"),
  Non_coastal = c(7.9, 16.1, 1.4, 2.0, 0.2, 0.5),
  Coastal = c(66.7, 22, 4.5, 5.4, 1.1, 0.3)
)

########################################################################################
### Create a formatted table for basic data tables

## Test table on dataset (This one works)!!! Success!!!
table_1 <- kable(bird_data, "html", escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center",
    font_size = 1) %>%
  row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black") %>% # purple header with black text and black borders
  column_spec(1:ncol(bird_data), border_left = TRUE, border_right = TRUE, background = "white") %>% # black border around columns
  add_header_above(c(" " = ncol(bird_data)), line = TRUE, line_sep = 3, color = "black") # black border around header
table_1

NBA_colr_tbl()

#### create function
basic_tbl <- function(DF){

  table_1 <- kable(DF, "html", escape = FALSE) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 1) %>%
    row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black") %>% # purple header with black text and black borders
    column_spec(1:ncol(DF), border_left = TRUE, border_right = TRUE, background = "white") %>% # black border around columns
    add_header_above(c(" " = ncol(DF)), line = TRUE, line_sep = 3, color = "black") # black border around header
  table_1
}

basic_tbl(bird_data)

########################################################################################
### Create a formatted table for threat status tables (In progress)

bird_cat <- bird_data %>%
  mutate(Status = cell_spec(
    Status,
    background = case_when(
      Status == "Least Concern" ~ "#b1d798",
      Status == "Near Threatened" ~ "#eeeea3",
      Status == "Vulnerable" ~ "#fff02a",
      Status == "Endangered" ~ "#f97835",
      Status == "Critically Endangered" ~ "#e9302c",
      TRUE ~ "white"
    ),
    color = "black",
    bold = Status == "Critically Endangered",
    extra_css = "border: 1px solid black; display: block; width: 100%; height: 100%;",
    background_as_tile = TRUE
  ))

table_2 <- kable(bird_cat, "html", escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center",
    font_size = 12) %>%
  column_spec(1:ncol(bird_cat), border_left = TRUE, border_right = TRUE, background = "white") %>%
  kableExtra::add_header_above(c(" " = ncol(bird_cat)), line = TRUE, line_sep = 3, color = "black")%>%
  row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black")
table_2

########################################################################################
### Troubleshooting above code to get entire cell to be coloured in.

color_cell <- function(status) {
  color <- case_when(
    status == "Least Concern" ~ "#b1d798",
    status == "Near Threatened" ~ "#eeeea3",
    status == "Vulnerable" ~ "#fff02a",
    status == "Endangered" ~ "#f97835",
    status == "Critically Endangered" ~ "#e9302c",
    TRUE ~ "white"
  )

  html <- paste0(
    '<div style="background-color:', color, '; color: black; padding: 5px;">',
    status, '</div>'
  )

  return(html)
}

# Apply the HTML function to the Status column
bird_cat <- bird_data %>%
  mutate(Status = sapply(Status, color_cell))

table_2.1 <- kable(bird_cat, "html", escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center",
    font_size = 12
  ) %>%
  column_spec(1:ncol(bird_cat), border_left = TRUE, border_right = TRUE, background = "white") %>%
  kableExtra::add_header_above(c(" " = ncol(bird_cat)), line = TRUE, line_sep = 3, color = "black")%>%
  row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black")
table_2.1

thr_tbl <- function(DF, COL) {

  color_cell <- function(COL) {
    color <- case_when(
      COL == "Least Concern" ~ "#b1d798",
      COL == "Near Threatened" ~ "#eeeea3",
      COL == "Vulnerable" ~ "#fff02a",
      COL == "Endangered" ~ "#f97835",
      COL == "Critically Endangered" ~ "#e9302c",
      TRUE ~ "white"
    )

    html <- paste0(
      '<div style="background-color:', color, '; color: black; padding: 5px;">',
      COL, '</div>'
    )

    return(html)
  }

  # Apply the HTML function to the Status column
  bird_cat <- DF %>%
    mutate(Status = sapply({{COL}}, color_cell))

  table_2.1 <- kable(bird_cat, "html", escape = FALSE) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 12
    ) %>%
    column_spec(1:ncol(bird_cat), border_left = TRUE, border_right = TRUE, background = "white") %>%
    kableExtra::add_header_above(c(" " = ncol(bird_cat)), line = TRUE, line_sep = 3, color = "black")%>%
    row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black")
  table_2.1

}

pro_tbl(bird_data, Status)

########################################################################################
### Create a formatted table for protection level tables
library(dplyr)
library(kableExtra)
library(htmltools)

# Define the color_cell function
color_cell <- function(status) {
  color <- case_when(
    status == "Least Concern" ~ "#b1d798",
    status == "Near Threatened" ~ "#eeeea3",
    status == "Vulnerable" ~ "#fff02a",
    status == "Endangered" ~ "#f97835",
    status == "Critically Endangered" ~ "#e9302c",
    TRUE ~ "white"
  )

  # Return HTML with inline style
  sprintf('<div style="background-color:%s; color: black; padding: 5px; height: 100%%;">%s</div>', color, status)
}

# Apply the function to the Status column
bird_cat <- bird_data %>%
  mutate(Status = sapply(Status, color_cell))

# Create the table
table_2.2 <- kable(bird_cat, "html", escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center",
    font_size = 12
  ) %>%
  column_spec(1:ncol(bird_cat), border_left = TRUE, border_right = TRUE, background = "white") %>%
  kableExtra::add_header_above(c(" " = ncol(bird_cat)), line = TRUE, line_sep = 3, color = "black")%>%
  row_spec(0, background = "#899be1", color = "black", bold = TRUE, extra_css = "border: 1px solid black")
table_2.2

########################################################################################
