#####################################################################################
## Script name: RLI_graphs.R
##
## Purpose of script: To create the  Red List Index buffered line graphs
##
## Author: Shae-Lynn Hendricks
##
## Date Created: 2024-07-24
##
## Notes: Dataframe only requires the Red List Index (RLI), year, minimum and
##        maximum values for each year. I am not sure what the raw data looks like,
##        I just used the tables from the excel figures Andrew shared.
##        This script creates graphs for fig 6, fig 33a, fig 33b, and fig 32
##        DF: excel fig tables
##        YEAR: The "Year" column in the datatable populates the x-axis summarising the results according to year recorded
##        RLI: The "RLI" column in the datatable that contains the Red List Index value for each species group for each year
##        min: The "min" column in the datatable that contains the minimum RLI value for each species group for each year
##        max: The "max" column in the datatable that contains the minimum RLI value for each species group for each year
##        GROUP: The "Group" column in the datatable that contains the list of species
#####################################################################################
### Load libraries

library(ggplot2)
#### covers fig 6, 33a & 33b ###
RLI_graph <- function(DF,YEAR, RLI, min, max){
  ggplot2::ggplot(DF, aes(x = {{YEAR}}, y = {{RLI}})) +
    ggplot2::geom_line(aes(y = {{RLI}})) +
    ggplot2::geom_ribbon(aes(ymin = {{min}}, ymax = {{max}}),alpha = .3, colour = NA)+
    ggplot2::theme_classic()+
    ggplot2::ylim(0.7,1)

}


### covers fig 32 ###
RLI_graph_2 <- function(DF,YEAR, RLI, min, max, GROUP){
  ggplot2::ggplot(DF, aes(x = {{YEAR}}, y = {{RLI}}, group = {{GROUP}}, color = {{GROUP}})) +
    ggplot2::geom_line(linetype="dashed") +
    ggplot2::geom_ribbon(aes(ymin = {{min}}, ymax = {{max}}), fill = "grey", alpha = .2, colour = NA)+
    ggplot2::theme_classic()+
    ggplot2::ylim(0.7,1)
}
