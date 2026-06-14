
# ============================================================
#### My project ####
# ============================================================

# One main ecological threat is invasive species.
# We know that invasive species are partly responsible for biodiversity loss. 
# That is the reason why it would be important to investigate the impact of 
# one invasive plant on an indigenous plant in Switzerland. 

# These two plant species are:
# Impatiens noli-tangere (Impatience ne-me-touchez-pas): indigenous 
#                   → live in shady and damp areas of the forest
# Impatiens glandulifera (Impatiente glanduleuse): invasive 
#                   → live along rivers and in riparian forests
# They belong to the same family: the Balsaminaceae.
# References: InfoFlora 


## My research questions ##
# Which parameters can explain the difference in distribution 
# between Impatiens noli-tangere and Impatiens glandulifera?

# Does Impatiens glandulifera endanger Impatiens noli-tangere 
# (due to competition) in Switzerland?

# Based on environmental variables (temperature, precipitation, elevation…)
# and species distribution (data from GBIF & iNaturalist), we analyze the link  
# between the two Impatiens species (especially by comparing their ecological niches). 



# ============================================================
#### My final matrix ####
# ============================================================

# Load my final matrix
matrix_full_tip_top <- read.csv("Data/matrix_full_tip_top_final.csv")


# Overview 
head(matrix_full_tip_top)
colnames(matrix_full_tip_top)
str(matrix_full_tip_top)

head(matrix_full_tip_top$temp)

min(matrix_full_tip_top$date_obs)
max(matrix_full_tip_top$date_obs)
# Data from 2020 to 2025


#  Look at number of different sites
# My matrix contains plant observations across years
# so a single can be record several times in the same site
# I would like to know how many site I have in my matrix
nrow(matrix_full_tip_top)
# 3699 observations

n_distinct(matrix_full_tip_top$coord_id)
# 1014

n_distinct(
  paste(
    matrix_full_tip_top$coord_id,
    matrix_full_tip_top$species
  )
)
# 1014
# So, there is 1014 unique sites in my data

