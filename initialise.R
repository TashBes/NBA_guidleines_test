#####################################################################################
##
## Script name: initialise.R
##
## Purpose of script:
## create the template files and folder structure typically needed by the SANBI marine team
##
## Author: Jock Currie
##
## Date Created: 2024-01-05
##
##
## Notes:
##   This script copies across most of this project folder to directory specified in the
##   target_path.
##   NB Note that it also copies the .gitignore file, which includes the data
##   subfolder, so any files in 'data' will NOT be version controlled or shared on the
##   repository. This default has been chosen to avoid mistakenly sharing our (or most
##   often other people's) data publicly.
##
#####################################################################################
### packages & functions

# require(tidyverse)
# require(sf)
# require(terra)

# source("functions/packages.R")       # loads up all the packages we need

#####################################################################################
### settings

### filenames & directories

## set the folder path to your newly created project folder Warning! the README.md and any other files with duplicated names will be over-written!
target_path <- "C:\\Users\\N.Besseling\\Documents\\repos\\NBA_guidleines_test" # I've specified a relative path as all my R projects are in one folder, but it may be easier to use an absolute path copied from your file browser

# options(scipen = 6, digits = 4) # if prefer non-scientific notation

#####################################################################################
### check if directories exist, if not create them

## note, this will not work if the project folder specified in target_path does not yet exist.

## create data subfolder
ifelse(!dir.exists(file.path(target_path, "data")), dir.create(file.path(target_path, "data")), FALSE)

## create scripts subfolder
ifelse(!dir.exists(file.path(target_path, "scripts")), dir.create(file.path(target_path, "scripts")), FALSE)

## create outputs subfolder
ifelse(!dir.exists(file.path(target_path, "outputs")), dir.create(file.path(target_path, "outputs")), FALSE)

## create plots subfolder
ifelse(!dir.exists(file.path(target_path, "plots")), dir.create(file.path(target_path, "plots")), FALSE)

#####################################################################################
### copy across relevant template files

## copy across the .gitignore; WARNING: this will over-write the .gitignore file in the target_path folder.
file.copy(from = ".gitignore", to = file.path(target_path), overwrite = T)

## copy across all files in the scripts folder to the newproject scripts folder
for (i in list.files(path = "scripts", all.files = F, full.names = T)) {
  file.copy(from = i, to = file.path(target_path, i), overwrite = T)
}

#####################################################################################
