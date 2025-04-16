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
                              "No Protection" = "#a4a3a3")

## specify the order for threat statuses and protection levels for consistent ordering
threat_order <- c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern")
protection_order <- c("No Protection", "Poorly Protected", "Moderately Protected", "Well Protected")


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


######################################################################################
### prepare the data by descending and ascending order

## create a summary table grouped by threat status and protection level
summary_table <- mem %>%
  st_drop_geometry() %>%
  distinct(ecosystem_type, threat_status, protection_level_30) %>%
  mutate(
    ## set factor levels for threat status based on the defined order
    threat_status = factor(threat_status, levels = breaks)
  ) %>%
  ## Group by threat status and protection level
  group_by(threat_status, protection_level_30) %>%
  ## count the occurrences within each group
  summarise(Count = n(), .groups = 'drop') %>%
  ## convert from long to wide format, filling missing values with zero
  tidyr::pivot_wider(names_from = protection_level_30, values_from = Count, values_fill = list(Count = 0)) %>%
  ## calculate a total count for each row
  mutate(Total = rowSums(dplyr::select(., -threat_status), na.rm = TRUE)) %>%
  arrange(threat_status)

## calculate total count for each column and add a total row
total_row <- summary_table %>%
  summarise(across(-threat_status, \(x) sum(x, na.rm = TRUE))) %>%
  mutate(threat_status = "Total (n)")

##calculate number of ecosystems
eco_num <- mem %>%
  st_drop_geometry() %>%
  distinct(ecosystem_type) %>%
  count()

eco_num <- eco_num[,1]

## calculate the percentage based on a hard-coded total value (e.g., 163)
percentage_row <- total_row %>%
  mutate(across(-threat_status, ~ . / eco_num * 100)) %>%  ## calculate percentage for each column
  mutate(threat_status = "Percentage (%)")  ## set the row name as "Percentage %"

## combine summary, total, and percentage rows
final_table <- bind_rows(summary_table, total_row, percentage_row)

## identify columns to round (exclude total and threat status columns)
count_columns <- setdiff(names(final_table), c("threat_status", "Total"))

## round counts to 0 decimal places for cleaner display
final_table <- final_table %>%
  mutate(across(all_of(count_columns), round, 0))

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
                bold = FALSE)
}

## ensure the Total column has no background color
tbl_final <- tbl_final %>%
  column_spec(ncol(final_table), background = "white")

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
html_file <- "tables/protection_threat_table_30.html"
save_kable(tbl_final, file = html_file, self_contained = TRUE)  ## ensure styles are embedded

## use "viewport" and adjust padding/margin in CSS if necessary
webshot2::webshot(html_file,
                  file = "tables/protection_threat_table_30.png",
                  vwidth = 390,
                  vheight = 465,
                  cliprect = "viewport")










#####################################################################################
### unload packages

# detach("package:xxx", unload=TRUE)
