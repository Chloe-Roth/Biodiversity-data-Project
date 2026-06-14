
# ============================================================
# PLOTS 
# ============================================================
# Aim: do plots to analyze the environmental comparison between species


# With the Random Forest analysis, we have 
# identify 3 important environmental variables
# to distinguish the 2 Impatiens species.

# These environmental variables are:
#   1) elevation
#   2) NDVI
#   3) W_Ecosystm

# So, with a variety of plots, we will investigate
# our data across these 3 environmental variables




# --------------------------------------------------
# 1) elevation
# --------------------------------------------------

# Based on Random Forest analysis, elevation is the 
# more important environmental variable to distinghish 
# the 2 Impatiens species

# To investigate the elevation data, I do a violin plot:
ggplot(
  matrix_full_tip_top,
  aes(x = species, y = elevation, fill = species)
) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(
    width = 0.105,
    fill = "white",
    outlier.shape = NA
  ) +
  scale_fill_manual(values = c(
    "Impatiens noli-tangere" = "#223e8b",
    "Impatiens glandulifera" = "red"
  )) +
  labs(
    x = NULL,
    y = "Elevation [m]"
  ) +
  theme_bw()

# We see a huge difference in the forms of violin.
# Impatiens glandulifera is closely related to 
# the elevation of 500 m.
# In contrast, Impatiens noli-tangere can be present
# in a elevation of more than 2500 m. However, the 
# median is close to 700 m.

# So, I. glandulifera has a small range of elevation
# (0 to 1500 m).
# I. noli-tangere has a larger range of elevation 
# (0 to 2500 m). I. noli-tangere is the only one to go 
# more than 1500 m -> niche.




# --------------------------------------------------
# 2) NDVI
# --------------------------------------------------

# NDVI = Normalized Difference Vegetation Index
# It is a satellite metric used to measure the 
# health and density of vegetation.

# NDVI has values from -1 to 1
# Interpretation:
#   -1 to -0.1 (negative values): non-vegetated surfaces 
#                Examples: open water, snow, or thick cloud cover
#   0 to 0.1 (low values): bare soil, rocky areas, or deserts 
#                 —> lands with little to no photosynthetic activity
#   0.2 to 0.5 (moderate values): sparse or stressed vegetation 
#                 Examples: grasslands, shrubs, meadows, and fields under drought
#   0.6 to 1 (high values): lush, healthy vegetation
#                 Examples: dense crop canopies or mature forests
# Reference: https://eos.com/blog/normalized-difference-vegetation-index-or-ndvi/


# To represent the NDVI of my data,
# I use a density plot:
ggplot(matrix_full_tip_top,
       aes(x = NDVI,
           fill = species)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c(
    "Impatiens noli-tangere" = "#223e8b",
    "Impatiens glandulifera" = "red"
  ),
    name = "Species"
  ) +
  labs(
    title = "NDVI distribution of the two Impatiens species",
    x = "NDVI",
    y = "Density"
  ) +
  theme_bw()

# We see that the two Impatiens species have different 
# NDVI values

# Impatiens noli-tangere has more NDVI values close to 0.0 (optimum).
# In contrast, Impatiens glandulifera has more NDVI 
# values close to 0.3 (optimum).

# Theses results demonstrate that I. noli-tangere 
# prefers to grow in habitat with less vegetation.
# For examples on bare soil or in rocky areas. It is
# coherent with our results with elevation. This species
# like to grow at higher elevation where we find less 
# vegetation, such as in mountains.
# Then, I. glandulifera seems to grow in area with more
# vegetation (for examples: grasslands, shrubs, meadows).
# It is also coherent with their preferences in terms of  
# elevation: lower elevation, so more habitat in the plains,
# and so more vegetation.




# --------------------------------------------------
# 3) W_Ecosystm
# --------------------------------------------------

# W_Ecosystm is the combinaison of Climate (Climate_Re),
# Landcover and Landforms.

# So, I decide to do a summary plot for W_Ecosystm.
# But since there are many categories and it's complicated,
# I do also one plot for Climate (Climate_Re),Landcover
# and Landforms, respectively (separately).



#### Prepare data ####

base_data <- matrix_full_tip_top %>%
  distinct(coord_id, species, Landcover, Landforms,
           Climate_Re, W_Ecosystm)

eco_summary <- base_data %>%
  count(W_Ecosystm, species, name = "n_sites_eco")

climate_summary <- base_data %>%
  count(Climate_Re, species, name = "n_sites_climate")

landcover_summary <- base_data %>%
  count(Landcover, species, name = "n_sites_landcover")

landform_summary <- base_data %>%
  count(Landforms, species, name = "n_sites_landforms")



#### W_Ecosystm plot ####

# Code to have blank bare if no observation
# (so that there isn't a bar that takes up 
# the width of two bars)
plot_data_eco <- eco_summary %>%
  complete(
    W_Ecosystm,
    species,
    fill = list(n_sites = 0)
  )

plot_data_eco <- plot_data_eco %>%
  mutate(
    W_Ecosystm = forcats::fct_reorder(
      W_Ecosystm,
      n_sites_eco,
      .fun = sum,
      .desc = FALSE
    )
  )

ggplot(plot_data_eco,
       aes( x = n_sites_eco,
            y = W_Ecosystm,
           fill = species)) +
  geom_col(position = "dodge") +
  labs(
    x = "Number of sites",
    y = NULL
  ) +
  theme_bw()

# There is a lot of different ecosystem, so the 
# plot is "lourd".

# However, we can see that the more important ecosystems are:
#   1) Cool Temperate Moist Forest on Mountains
#   2) Cool Temperate Moist Grassland on Mountains
#   3) Cool Temperate Moist Cropland on Mountains

# So, the common point of these 3 ecosystems are 
# the cool temperate moist climate and the mountains (landforms).
# The more variable aspect is the land cover: forest, 
# grassland ans cropland.
# We will analyze that in the next plots. 

# For the ecosystem 1) the 2 Impatiens species are present
# in the same ratio. 
# Then, in the ecosystems 2) and 3) I. glandulifera is more 
# present than I. noli-tangere.



#### Climate_Re plot ####

# Code to have blank bare if no observation
# (so that there isn't a bar that takes up 
# the width of two bars)
plot_data_climate <- climate_summary %>%
  complete(
    Climate_Re,
    species,
    fill = list(n_sites = 0)
  )

ggplot(
  plot_data_climate,
  aes(
    x = Climate_Re,
    y = n_sites_climate,
    fill = factor(
      species,
      levels = c(
        "Impatiens noli-tangere",
        "Impatiens glandulifera" ))
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_manual(
    values = c(
      "Impatiens noli-tangere" = "#00BFC4",
      "Impatiens glandulifera" = "#F8766D"
    )
  ) +
  labs(
    title = "Total plant abundance per climate",
    x = "Climate",
    y = "Total abundance",
    fill = "Species"
  ) +
  theme_bw()

# The Impatiens species are mainly present in 
# cool temperate moist climate
# It is coherent with the W_Ecosystm plot: 
# cool temperate moist is the more important climate
# for the 2 Impatiens species

# I. noli-tangere is a little present in polar moist 
# and warm temperate moist climate. 

# In contrast, I. glandulifera is absent in polar moist 
# climate and more present in warm temperate moist climate.

# So, Impatiens species live in different climate, despite 
# they have one main climate in common. 



#### Landcover plot ####

# Colors of each land cover
landcover_cols <- c(
  "Cropland" = "#FF7F00",
  "Forest" = "#0e870e",
  "Settlement" = "#f14aee",
  "Grassland" = "#52bb4e",
  "Shrubland" = "#d6a634",
  "Snow and Ice" = "#499cdb",
  "Sparsely or Non vegetated" = "#dd2929"
)

ggplot(
  landcover_summary,
  aes(
    x = species,
    y = n_sites_landcover,
    fill = Landcover
  )
) +
  geom_col(position = "fill") +
  scale_fill_manual(values = landcover_cols) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Land cover composition of sites occupied by each species",
    x = "Species",
    y = "Percentage of sites",
    fill = "Land cover"
  ) +
  theme_bw(base_size = 13)

# The 2 Impatiens species are mainly present in grassland, forest
# and cropland.
# It is coherent with the W_Ecosystm plot: 
# the three main land cover in the Landcover plot is the same than 
# for the 3 main ecosystems in W_Ecosystm

# Impatiens glandulifera is more present in grasland and cropland
# compared to I. noli-tangere.
# In contrast, I. noli-tangere is more present in forest.

# The main difference is the presence of Impatiens noli-tangere
# in 3 unique land cover (Impatiens glandulifera is not present 
# there): Shrubland, Snow and Ice, and Sparsely or Non vegetated".



#### Landforms plot ####

# Code to have blank bare if no observation
# (so that there isn't a bar that takes up 
# the width of two bars)
plot_data_landform <- landform_summary %>%
  complete(
    Landforms,
    species,
    fill = list(n_sites = 0)
  )

ggplot(
  plot_data_landform,
  aes(
    x = Landforms,
    y = n_sites_landforms,
    fill = factor(
      species,
      levels = c(
        "Impatiens noli-tangere",
        "Impatiens glandulifera" ))
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  scale_fill_manual(
    values = c(
      "Impatiens noli-tangere" = "#00BFC4",
      "Impatiens glandulifera" = "#F8766D"
    )
  ) +
  labs(
    title = "Total plant abundance per landform",
    x = "Landforms",
    y = "Total abundance",
    fill = "Species"
  ) +
  theme_bw()

# The main landform of Impatiens is mountains. 
# It is coherent with the W_Ecosystm plot: 
# mountains is the more important landform
# for the 2 Impatiens species

# I. noli-tangere is more present in tablelands 
# compared to I. glandulifera

# In contrast, I. noli-tangere is not present in plains.
# I. glandulifera is present in plains and hills. 
  
