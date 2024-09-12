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
##
#####################################################################################
### Load libraries

library(ggplot2)

RLI_graph <- function(DF,YEAR, RLI, min, max, GRP = FALSE){

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
}


