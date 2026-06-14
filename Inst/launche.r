
## Intermediate Project ## 

source("./SRC/Intermediate Project/matrix_full.r")

source("./SRC/Intermediate Project/ecosystems.r") 

source("./SRC/Intermediate Project/elevation.r")

source("./SRC/Intermediate Project/climate.r")

source("./SRC/Intermediate Project/sat_manual.r")



## Final Project ##

# 1) Load the final environmental matrix & description of my project (with research questions)
source("./SRC/Final Project/step1_read_matrix.R")

# 2) PCA or ordination analysis
source("./SRC/Final Project/analysis_1_pca.R")

# 3) Random forest (discriminating variable analysis)
# feature importance plot
source("./SRC/Final Project/analysis_2_random_forest.R")

# 4) Environmental comparison between species or groups
source("./SRC/Final Project/analysis_3_environmental_comparison.R")

# 5) Maps of Impatiens species distribution in Switzerland 
# Maps and in interactive map
source("./SRC/Final Project/analysis_4_map.R")

# 6) Final summary panel figure
source("./SRC/Final Project/analysis_5_summary_panel.R")
