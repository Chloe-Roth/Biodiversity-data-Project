###############################################################################
# PACKAGES (in alphabetical order) needed for the project
###############################################################################

library(appeears)
library(dplyr)         # table manipulation
library(elevatr)   # download elevation data
library(ggplot2)       # graphics
library(luna)
library(MODIStsp)
library(raster)        # spatial extent management
library(Rchelsa)
library(rgbif)         # access to GBIF data
library(rinat)         # access to iNaturalist data
library(rnaturalearth) # country maps 
library(sf)            # modern spatial objects
library(terra)

# Disable s2 geometry engine (can avoid issues in some spatial operations)
sf_use_s2(FALSE)
