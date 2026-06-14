
# ============================================================
# PCA (= Principal Component Analysis)
# ============================================================

#### Establishment of PCA  ####

pca_data <- matrix_full_tip_top %>%
  select(
    elevation,
    tmax_mean_c,
    prec_mean_annual,
    temp_mean_c,
    tas_current_july_c,
    NDVI,
  )

# Run PCA
pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Explore the PCA result
summary(pca_res)

# Scores = coordinates of observations in PCA space
pca_scores <- as.data.frame(pca_res$x)

# Add metadata back
pca_scores <- bind_cols(
  matrix_full_tip_top %>% select(Landforms, species, date_obs),
  pca_scores
)

head(pca_scores)

# Percentage of variance explained
var_explained <- (pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100
pc1_lab <- paste0("PC1 (", round(var_explained[1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(var_explained[2], 1), "%)")



#### Basic PCA plot ####

plot_PCA <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = species)) +
  geom_point(size = 2.4, alpha = 0.75) +
  scale_color_manual(values = c(
    "Impatiens noli-tangere" = "#223e8b",
    "Impatiens glandulifera" = "red"
    )) +
  labs(
    title = "PCA colored by species",
    subtitle = "Raw observations projected in multivariate space",
    x = pc1_lab,
    y = pc2_lab,
    color = "Species"
  ) +
  theme_bw(base_size = 13)
# Color the points by species

plot_PCA
# We see that the 2 Impatiens overlap 
# but there are also different on the edge of PCA plot.
# So, we can conclude that the 2 Impatiens species are similar
# but they are also different for some environmental variables.

# Your PCA looks for elevation, tmax_mean_c, prec_mean_annual,
# temp_mean_c, tas_current_july_c and NDVI.
# We don't know which evironmental variable is the more important
# to distinghish the 2 Impatiens species. 
# That is the reason why we will do Random Forest in the next document.

# (There is too much overlap to add ellipse on the plot.)

