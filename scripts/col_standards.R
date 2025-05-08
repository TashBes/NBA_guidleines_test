#####################################################################################
##
## Script name: col standards
##
## Purpose of script: make the standard cols to be used in the NBA
##
## Author: Natasha Besseling
##
## Date Created: 2025-05-07
##
##
## Notes:
##
##
#####################################################################################
### packages & functions

require(tidyverse)

#####################################################################################

##Threat status
CR <- rgb(216, 30, 5, maxColorValue = 255)
EN <- rgb(252, 127, 63, maxColorValue = 255)
VU <- rgb(249, 232, 20, maxColorValue = 255)
NT <- rgb(0, 0, 0, maxColorValue = 255)
LC <- rgb(180, 215, 158, maxColorValue = 255)
DD <- rgb(0, 0, 0, maxColorValue = 255)
R <- rgb(0, 0, 0, maxColorValue = 255)
EX <- rgb(0, 0, 0, maxColorValue = 255)

#Protection level
NP <- rgb(166, 166, 166, maxColorValue = 255)
PP <- rgb(213, 222, 196, maxColorValue = 255)
MP <- rgb(132, 171, 92, maxColorValue = 255)
WP <- rgb(75, 110, 0, maxColorValue = 255)

#Pressures
low <- rgb(223, 220, 199, maxColorValue = 255)
med <- rgb(175, 168, 117, maxColorValue = 255)
high <- rgb(122, 116, 70, maxColorValue = 255)
vhigh <- rgb(88, 82, 50, maxColorValue = 255)


no_threats <- rgb(48, 30, 6, maxColorValue = 255)
poll <- rgb(97, 65, 56, maxColorValue = 255)
transp <- rgb(99, 76, 39, maxColorValue = 255)
agri <- rgb(133, 76, 13, maxColorValue = 255)
geo <- rgb(153, 102, 0, maxColorValue = 255)
bio_use <- rgb(180, 121, 42, maxColorValue = 255)
other <- rgb(231, 160, 54, maxColorValue = 255)
human <- rgb(159, 134, 9, maxColorValue = 255)
climate <- rgb(178, 149, 78, maxColorValue = 255)
enrgy <- rgb(122, 116, 70, maxColorValue = 255)
natrl_mod <- rgb(88, 82, 50, maxColorValue = 255)
invasiv <- rgb(61, 69, 64, maxColorValue = 255)
residential <- rgb(128, 128, 128, maxColorValue = 255)


# Condition
natural <- rgb(110, 159, 212, maxColorValue = 255)
mod_modified <- rgb(165, 197, 199, maxColorValue = 255)
heav_modified <- rgb(129, 171, 167, maxColorValue = 255)
perm_modified <- rgb(136, 129, 78, maxColorValue = 255)


# Responses
no_response <- rgb(91, 66, 114, maxColorValue = 255)
some_response <- rgb(100, 103, 130, maxColorValue = 255)
gazetted <- rgb(117, 164, 179, maxColorValue = 255)


# Priority areas
LPA <- rgb(0, 60, 0, maxColorValue = 255)
MPA <- rgb(0, 38, 115, maxColorValue = 255)
CBA <- rgb(67, 128, 0, maxColorValue = 255)
ESA <- rgb(168, 168, 0, maxColorValue = 255)


##colour mapping
col_mapping <- c(

  ##Threat status
  "Critically Endangered" = rgb(216, 30, 5, maxColorValue = 255),
  "Endangered" = rgb(252, 127, 63, maxColorValue = 255),
  "Vulnerable" = rgb(249, 232, 20, maxColorValue = 255),
  "Near Threatened" = rgb(0, 0, 0, maxColorValue = 255),
  "Least Concern" = rgb(180, 215, 158, maxColorValue = 255),
  "Data Deficient" = rgb(0, 0, 0, maxColorValue = 255),
  "Rare" = rgb(0, 0, 0, maxColorValue = 255),
  "Extinct" = rgb(0, 0, 0, maxColorValue = 255),
  "Extinct in the Wild" = rgb(0, 0, 0, maxColorValue = 255),

  #Protection level
  "Not Protected" = rgb(166, 166, 166, maxColorValue = 255),
  "Poorly Protected" = rgb(213, 222, 196, maxColorValue = 255),
  "Moderately Protected" = rgb(132, 171, 92, maxColorValue = 255),
  "Well Protected" = rgb(75, 110, 0, maxColorValue = 255),

  #Pressures
  "Low" = rgb(223, 220, 199, maxColorValue = 255),
  "Medium" = rgb(175, 168, 117, maxColorValue = 255),
  "High" = rgb(122, 116, 70, maxColorValue = 255),
  "Very high" = rgb(88, 82, 50, maxColorValue = 255),


  "No threats" = rgb(48, 30, 6, maxColorValue = 255),
  "Pollution" = rgb(97, 65, 56, maxColorValue = 255),
  "Transportation & service corridors" = rgb(99, 76, 39, maxColorValue = 255),
  "Agriculture" = rgb(133, 76, 13, maxColorValue = 255),
  "Agriculture and aquaculture" = rgb(133, 76, 13, maxColorValue = 255),
  "Geological events" = rgb(153, 102, 0, maxColorValue = 255),
  "Biological resource use" = rgb(180, 121, 42, maxColorValue = 255),
  "Other threats" = rgb(231, 160, 54, maxColorValue = 255),
  "Human intrusions & disturbance" = rgb(159, 134, 9, maxColorValue = 255),
  "Human intrusions and disturbance" = rgb(159, 134, 9, maxColorValue = 255),
  "Climate change" = rgb(178, 149, 78, maxColorValue = 255),
  "Climate change & severe weather" = rgb(178, 149, 78, maxColorValue = 255),
  "Energy production & mining" = rgb(122, 116, 70, maxColorValue = 255),
  "Energy production and mining" = rgb(122, 116, 70, maxColorValue = 255),
  "Natural system modifications" = rgb(88, 82, 50, maxColorValue = 255),
  "Invasive and other problematic species, genes & diseases" = rgb(61, 69, 64, maxColorValue = 255),
  "Invasive and other problamatic species, genes and diseases" = rgb(61, 69, 64, maxColorValue = 255),
  "Residential & commercial development" = rgb(128, 128, 128, maxColorValue = 255),


  # Condition
  "Natural" = rgb(110, 159, 212, maxColorValue = 255),
  "Natural / near natural" = rgb(110, 159, 212, maxColorValue = 255),
  "Near natural" = rgb(110, 159, 212, maxColorValue = 255),
  "Moderately modified" = rgb(165, 197, 199, maxColorValue = 255),
  "Heavily / intensively modified" = rgb(129, 171, 167, maxColorValue = 255),
  "Permanently / irreversibly modified" = rgb(136, 129, 78, maxColorValue = 255),


  # Responses
  "No response" = rgb(91, 66, 114, maxColorValue = 255),
  "Some kind of response" = rgb(100, 103, 130, maxColorValue = 255),
  "Gazetted" = rgb(117, 164, 179, maxColorValue = 255),
  "Signed off" = rgb(117, 164, 179, maxColorValue = 255),


  # Priority areas
  "Land-based Protected Areas" = rgb(0, 60, 0, maxColorValue = 255),
  "Marine Protected Areas" = rgb(0, 38, 115, maxColorValue = 255),
  "Critical Biodiversity Areas" = rgb(67, 128, 0, maxColorValue = 255),
  "Ecologically Sensitive Areas" = rgb(168, 168, 0, maxColorValue = 255),

  # Built up areas
  "Cropland"= rgb(0, 0, 0, maxColorValue = 255),
  "Plantation"= rgb(0, 0, 0, maxColorValue = 255),
  "Built up"= rgb(0, 0, 0, maxColorValue = 255),
  "Mine"= rgb(0, 0, 0, maxColorValue = 255),
  "Artificial waterbody" = rgb(0, 0, 0, maxColorValue = 255)

)







##colours
cols<- c(
  ##Threat status
  CR ,
  EN ,
  VU ,
  NT ,
  LC,
  DD ,
  R ,
  EX,

  #Protection level
  NP ,
  PP ,
  MP ,
  WP ,

  #Pressures
  low ,
  med ,
  high ,
  vhigh ,


  no_threats ,
  poll ,
  transp ,
  agri ,
  geo,
  bio_use ,
  other ,
  human ,
  climate ,
  enrgy ,
  natrl_mod,
  invasiv ,
  residential ,


  # Condition
  natural,
  natural,
  natural,
  mod_modified ,
  heav_modified ,
  perm_modified ,


  # Responses
  no_response ,
  some_response ,
  gazetted ,


  # Priority areas
  LPA ,
  MPA ,
  CBA ,
  ESA )


## breaks
breaks <- c(
  ##Threat status
  CR ,
  EN ,
  VU ,
  NT ,
  LC,
  DD ,
  R ,
  EX,

  #Protection level
  NP ,
  PP ,
  MP ,
  WP ,

  #Pressures
  low ,
  med ,
  high ,
  vhigh ,


  no_threats ,
  poll ,
  transp ,
  agri ,
  geo,
  bio_use ,
  other ,
  human ,
  climate ,
  enrgy ,
  natrl_mod,
  invasiv ,
  residential ,


  # Condition
  "Natural",
  "Natural/near-natural",
  "Near-natural",
  mod_modified ,
  heav_modified ,
  perm_modified ,


  # Responses
  no_response ,
  some_response ,
  gazetted ,


  # Priority areas
  LPA ,
  MPA ,
  CBA ,
  ESA

  "Natural",
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



