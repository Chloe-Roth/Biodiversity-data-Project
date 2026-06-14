###############################################################################
# PACKAGES (in alphabetical order) needed for the project
###############################################################################

library(appeears)
library(caret)         # train/test split and confusion matrix (for Random Forest)
library(cowplot)       # for final figure (summary panel)
library(dplyr)         # table manipulation
library(elevatr)       # download elevation data
library(fmsb)          # plot
library(ggnewscale)    # for final figure (summary panel)
library(ggplot2)       # graphics
library(gridGraphics)  # for final figure (summary panel)
library(leaflet)       # map
library(luna)
library(MODIStsp)
library(plotly)        # interactive plot
library(randomForest)  # ML algorithm
library(raster)        # spatial extent management
library(rayshader)     # 2D/3D map
library(Rchelsa)
library(rgbif)         # access to GBIF data
library(rinat)         # access to iNaturalist data
library(rnaturalearth) # country maps 
library(sf)            # modern spatial objects
library(tidyr)         
library(terra)
library(viridis)       # nice colors for plots

# Disable s2 geometry engine (can avoid issues in some spatial operations)
sf_use_s2(FALSE)

