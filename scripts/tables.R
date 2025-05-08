#######################################################################################
##
## Script name: benth_eco_calc.R
##
## Purpose of script:
## To create a standardised template for a table used in the NBA
##
## Author: Lauryn Bull and Natasha Besseling
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

###make a table of protection level of threatened ecosystems
### define the levels and color of threat status and protection level categories

## define color mapping for threat statuses for use in the table display
threat_color_mapping <- c("Critically Endangered" = "#e9302c",
                          "Endangered" = "#f97835",
                          "Vulnerable" = "#fff02a",
                          "Near Threatened" = "#eeeea3",
                          "Least Concern" = "#b1d798")

## define color mapping for protection levels for table column backgrounds
protection_color_mapping <- c("Well Protected" = "#466a31",
                              "Moderately Protected" = "#80a952",
                              "Poorly Protected" = "#d5dec3",
                              "Not Protected" = "#a4a3a3")

## specify the order for threat statuses and protection levels for consistent ordering
threat_order <- c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")
protection_order <- c("Not Protected", "Poorly Protected", "Moderately Protected", "Well Protected")



######################################################################################
### prepare the data by descending and ascending order
library(magrittr)
## create a summary table grouped by threat status and protection level
mem <- NBA.package::NBA_example_map_data %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(threat_status = dplyr::case_when(
    P_EcosysType== "Agulhas Retroflection and South Ocean Transitional Water" ~ "Critically Endangered",
    P_EcosysType== "Cold Southeast Atlantic Water" ~ "Endangered",
    P_EcosysType== "Indian Ocean Frontal Margin Water" ~ "Vulnerable",
    P_EcosysType== "Indian Ocean Frontal Water" ~ "Near Threatened",
    P_EcosysType== "South Atlantic Productive Margin Water" ~ "Least Concern",
    P_EcosysType== "South Atlantic-Benguela Transitional Waters" ~ "Vulnerable",
    P_EcosysType== "Stable Agulhas Current Water" ~ "Critically Endangered",
    P_EcosysType== "Stable Indian Ocean Water" ~ "Critically Endangered",
    P_EcosysType== "Stable Southeast Atlantic Water" ~ "Endangered",
    P_EcosysType== "Upwelled Agulhas Current Margin Water" ~ "Vulnerable",
    P_EcosysType== "Variable Agulhas current core" ~ "Least Concern",
    P_EcosysType== "Variable Indo-Atlantic Water" ~ "Least Concern",
    P_EcosysType== "Warm Stable Indian Ocean Water" ~ "Near Threatened"

  ))



summary_table <- mem %>%
  dplyr::distinct(P_EcosysType, threat_status, protection_level) %>%
  dplyr::mutate(
    ## set factor levels for threat status based on the defined order
    threat_status = factor(threat_status, levels = threat_order)
  )%>%
  ## Group by threat status and protection level
  dplyr::group_by(threat_status, protection_level) %>%
  ## count the occurrences within each group
  dplyr::summarise(Count = dplyr::n(), .groups = 'drop') %>%
  ## convert from long to wide format, filling missing values with zero
  tidyr::pivot_wider(names_from = protection_level, values_from = Count, values_fill = list(Count = 0)) %>%
  ## calculate a total count for each row
  dplyr::mutate(Total = rowSums(dplyr::select(., -threat_status), na.rm = TRUE)) %>%
  dplyr::arrange(threat_status)

## calculate total count for each column and add a total row
total_row <- summary_table %>%
  dplyr::summarise(across(-threat_status, \(x) sum(x, na.rm = TRUE))) %>%
  dplyr::mutate(threat_status = "Total (n)")

##calculate number of ecosystems
eco_num <- mem %>%
  dplyr::distinct(P_EcosysType) %>%
  dplyr::count() %>%
  as.data.frame()

eco_num <- eco_num[,1]

## calculate the percentage
percentage_row <- total_row %>%
  dplyr::mutate(across(-threat_status, ~ (.x/eco_num) * 100))%>%  ## calculate percentage for each column
  dplyr::mutate(threat_status = "Percentage (%)")  ## set the row name as "Percentage %"

## combine summary, total, and percentage rows
final_table <- dplyr::bind_rows(summary_table, total_row, percentage_row)

## identify columns to round (exclude total and threat status columns)
count_columns <- setdiff(names(final_table), c("threat_status", "Total"))

## round counts to 0 decimal places for cleaner display
final_table <- final_table %>%
  dplyr::mutate(across(all_of(count_columns), round, 0))

## reorder protection level columns based on predefined order
final_table <- final_table %>%
  dplyr::select(threat_status, all_of(protection_order), Total)

######################################################################################
### use the function to produce the correct table

## create and format the table using kableExtra
tbl_final <- kable(final_table, col.names = c("", colnames(final_table)[-1]), format = "html", escape = FALSE) %>%
  ## apply table styling
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    full_width = FALSE,
    position = "center",
    font_size = 16
  ) %>%
  ## style the header row
  row_spec(0, background = "lightgrey", color = "black",
           extra_css = "border-top: 2px solid black; border-bottom: 2px solid black; text-align: left; font-weight: normal;") %>%
  ## set general column styling (no borders, white background)
  column_spec(1:ncol(final_table), border_left = FALSE, border_right = FALSE, background = "white") %>%
  ## grey out and add borders for the Total row
  row_spec(nrow(final_table) - 1, background = "lightgrey", color = "black",
           extra_css = "border-top: 2px solid black; border-bottom: 2px solid black") %>%
  ## grey out and add borders for the Percentage row
  row_spec(nrow(final_table), background = "lightgrey", color = "black",
           extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;")

## rotate the header text to fit and enhance readability
tbl_final <- tbl_final %>%
  row_spec(0, extra_css = "text-align: left; writing-mode: vertical-rl; transform: rotate(180deg); white-space: nowrap; height: 170px;")

## apply background colors for threat status rows
for (i in 1:(nrow(final_table) - 1)) {
  threat_status <- final_table$threat_status[i]
  if (!is.na(threat_status) && threat_status %in% names(threat_color_mapping)) {
    tbl_final <- tbl_final %>%
      row_spec(i, background = threat_color_mapping[threat_status])
  }
}


## apply background colors for protection level columns
for (j in 1:length(protection_order)) {
  protection_column <-   colnames(final_table)[[j + 1]] ## access each protection level column
  tbl_final <- tbl_final %>%
    column_spec(j+1 ,
                background = ifelse(protection_column %in% names(protection_color_mapping),
                                    protection_color_mapping[as.character(protection_column)],
                                    "white"),
                include_thead = TRUE,
                bold = FALSE)
}

## ensure the Total column has no background color
tbl_final <- tbl_final %>%
  column_spec(2:ncol(final_table), background = "white")

## apply background color to the percentage row
tbl_final <- tbl_final %>%
  row_spec(nrow(final_table), background = "lightgrey")  ## change color for the total row as needed

## apply background color to the total row
tbl_final <- tbl_final %>%
  row_spec(nrow(final_table) - 1, background = "lightgrey")

## display the final table
tbl_final ## render the final formatted table

######################################################################################
### save the final table as an image

## save the table as HTML first
html_file <- "outputs/protection_threat_table.html"
save_kable(tbl_final, file = html_file, self_contained = TRUE)  ## ensure styles are embedded

## use "viewport" and adjust padding/margin in CSS if necessary
webshot2::webshot(html_file,
                  file = "tables/protection_threat_table_30.png",
                  vwidth = 390,
                  vheight = 465,
                  cliprect = "viewport")


###########################
##make into a function

NBA_tbl_comb <- function(DF, GROUP, THR, PRO){

  ## define color mapping for threat statuses for use in the table display
  threat_color_mapping <- c("Critically Endangered" = "#e9302c",
                            "Endangered" = "#f97835",
                            "Vulnerable" = "#fff02a",
                            "Near Threatened" = "#eeeea3",
                            "Least Concern" = "#b1d798")

  ## define color mapping for protection levels for table column backgrounds
  protection_color_mapping <- c("Well Protected" = "#466a31",
                                "Moderately Protected" = "#80a952",
                                "Poorly Protected" = "#d5dec3",
                                "Not Protected" = "#a4a3a3")

  ## specify the order for threat statuses and protection levels for consistent ordering
  threat_order <- c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")
  protection_order <- c("Not Protected", "Poorly Protected", "Moderately Protected", "Well Protected")




  summary_table <- DF %>%
    dplyr::distinct({{GROUP}}, {{THR}}, {{PRO}}) %>%
    dplyr::mutate(
      ## set factor levels for threat status based on the defined order
      {{THR}} := factor({{THR}}, levels = threat_order)
    )%>%
    ## Group by threat status and protection level
    dplyr::group_by({{THR}}, {{PRO}}) %>%
    ## count the occurrences within each group
    dplyr::summarise(Count = dplyr::n(), .groups = 'drop') %>%
    ## convert from long to wide format, filling missing values with zero
    tidyr::pivot_wider(names_from = {{PRO}}, values_from = Count, values_fill = list(Count = 0)) %>%
    ## calculate a total count for each row
    dplyr::mutate(Total = rowSums(dplyr::select(., -c({{THR}})), na.rm = TRUE)) %>%
    dplyr::arrange({{THR}})


  ## calculate total count for each column and add a total row
  total_row <- summary_table %>%
    dplyr::summarise(across(-c({{THR}}), \(x) sum(x, na.rm = TRUE))) %>%
    dplyr::mutate({{THR}} := "Total (n)")

  ##calculate number of ecosystems
  eco_num <- mem %>%
    dplyr::distinct({{GROUP}}) %>%
    dplyr::count() %>%
    as.data.frame()

  eco_num <- eco_num[,1]

  ## calculate the percentage
  percentage_row <- total_row %>%
    dplyr::mutate(across(-{{THR}}, ~ (.x/eco_num) * 100))%>%  ## calculate percentage for each column
    dplyr::mutate({{THR}} := "Percentage (%)")  ## set the row name as "Percentage %"

  ## combine summary, total, and percentage rows
  final_table <- dplyr::bind_rows(summary_table, total_row, percentage_row)

  var <- deparse(substitute(THR))

  ## identify columns to round (exclude total and threat status columns)
  count_columns <- setdiff(names(final_table), c(var, "Total"))

  ## round counts to 0 decimal places for cleaner display
  final_table <- final_table %>%
    dplyr::mutate(across(all_of(count_columns), round, 0))

  ## reorder protection level columns based on predefined order
  final_table <- final_table %>%
    dplyr::select({{THR}}, all_of(protection_order), Total)

  ######################################################################################
  ### use the function to produce the correct table

  ## create and format the table using kableExtra
  tbl_final <- kable(final_table, col.names = c("", colnames(final_table)[-1]), format = "html", escape = FALSE) %>%
    ## apply table styling
    kable_styling(
      bootstrap_options = c("striped", "hover"),
      full_width = FALSE,
      position = "center",
      font_size = 16
    ) %>%
    ## style the header row
    row_spec(0, background = "lightgrey", color = "black",
             extra_css = "border-top: 2px solid black; border-bottom: 2px solid black; text-align: left; font-weight: normal;") %>%
    ## set general column styling (no borders, white background)
    column_spec(1:ncol(final_table), border_left = FALSE, border_right = FALSE, background = "white") %>%
    ## grey out and add borders for the Total row
    row_spec(nrow(final_table) - 1, background = "lightgrey", color = "black",
             extra_css = "border-top: 2px solid black; border-bottom: 2px solid black") %>%
    ## grey out and add borders for the Percentage row
    row_spec(nrow(final_table), background = "lightgrey", color = "black",
             extra_css = "border-top: 2px solid black; border-bottom: 2px solid black;")

  ## rotate the header text to fit and enhance readability
  tbl_final <- tbl_final %>%
    row_spec(0, extra_css = "text-align: left; writing-mode: vertical-rl; transform: rotate(180deg); white-space: nowrap; height: 170px;")


  ## apply background colors for threat status rows
  for (i in 1:(nrow(final_table) - 1)) {
    thr_name <- final_table %>%
      select({{THR}}) %>%
      slice(i) %>%
      as.data.frame()
    thr_name <- thr_name[,1]
    if (!is.na(thr_name) && thr_name %in% names(threat_color_mapping)) {
      tbl_final <- tbl_final %>%
        row_spec(i, background = threat_color_mapping[thr_name])
    }
  }


  ## apply background colors for protection level columns
  for (j in 1:length(protection_order)) {
    protection_column <-   colnames(final_table)[[j + 1]] ## access each protection level column
    tbl_final <- tbl_final %>%
      column_spec(j+1 ,
                  background = ifelse(protection_column %in% names(protection_color_mapping),
                                      protection_color_mapping[as.character(protection_column)],
                                      "white"),
                  include_thead = TRUE,
                  bold = FALSE)
  }

  ## ensure the Total column has no background color
  tbl_final <- tbl_final %>%
    column_spec(2:ncol(final_table), background = "white")

  ## apply background color to the percentage row
  tbl_final <- tbl_final %>%
    row_spec(nrow(final_table), background = "lightgrey")  ## change color for the total row as needed

  ## apply background color to the total row
  tbl_final <- tbl_final %>%
    row_spec(nrow(final_table) - 1, background = "lightgrey")

  ## display the final table
  tbl_final ## render the final formatted table


}

test <- NBA_tbl_comb(mem, P_EcosysType, threat_status, protection_level)
test

################################################################################
##recreate condition table
cond_tbl <- read.csv(
  dir("data",
      "condition_table.csv",
      full.names = T,
      recursive = T))

cond_tbl


test <- NBA.package::NBA_tbl_colr(cond_tbl, COL = c("A", "B", "C", "D", "E", "F"))


library(grid)

png("mask.png")
grid.polygon(c(-0.06, 0.06, 0.06, 0.15, 0, -0.15, -0.06),
             c(-5, -5, 2.5, 2, 5, 2, 2.5), gp=gpar(fill="black"),
             def="native",
             vp=viewport(xs=c(-0.15, 0.15), ys=c(-5, 5)))
dev.off()

library(png)
m <- readPNG("mask.png", native=FALSE)
mask <- matrix(rgb(m[,,1],m[,,2],m[,,3]),
               nrow=nrow(m))

rmat <- matrix(rgb(colorRamp(c("blue","white","red"))(seq(0,1,length=nrow(m))), maxColorValue=255),
               nrow=nrow(m), ncol=ncol(m))
rmat[mask == "#FFFFFF"] <- NA
grid.newpage()
grid.raster(rmat)

