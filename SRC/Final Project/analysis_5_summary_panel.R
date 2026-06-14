# ============================================================
#  Final figure pannel
# ============================================================

quartz()

# Build the plot 
graphA <- ggplot() +

  # WTE raster background (one color per ecosystem)
  geom_raster(data = wte_df,
              aes(x = x, y = y, fill = W_Ecosystm)) +
  scale_fill_manual(values = pal_wte, guide = "none") +

  # new_scale_fill() is needed to use a second fill variable
  # in the same plot (here: point colors by species)
  new_scale_fill() +

  # Observation points
  geom_sf(data = pts_sf,
          aes(fill = species, shape = species),
          size = 3.3, color = "white", stroke = 0.3) +
  scale_fill_manual(
    values = c(
      "Impatiens noli-tangere" = "#1d3557",
      "Impatiens glandulifera" = "#e63946"
    ),
    name = "Species") +
  scale_shape_manual(values = c(21, 24), name = "Species") +

  # Switzerland outline
  geom_sf(data = ch_sf, fill = NA, color = "grey30", linewidth = 0.5) +

  # coord_sf() defines the visible geographic area of the map
  coord_sf(xlim = c(5.9, 10.6), ylim = c(45.8, 47.9), expand = FALSE) +

  labs(title = " ") +
  theme_void() +  # removes axes and background, useful for maps
  theme(plot.title = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))

print(graphA)



# --------------------------------------------------
#  PLOT B – Circular plot
#  Random Forest
#  Hilight the more important variables to distinguish the species
# --------------------------------------------------

importance(rf_species)

sort(
  importance(rf_species)[, "MeanDecreaseGini"],
  decreasing = TRUE
)

importance_values <- sort(
  importance(rf_species)[, "MeanDecreaseGini"],
  decreasing = TRUE
)
importance_values

top3_df <- data.frame(
  feature = names(importance_values),
  importance = as.numeric(importance_values)
) %>%
  arrange(desc(importance)) %>%
  slice(1:3) %>%
  mutate(
    feature = recode(
      feature,
      "elevation" = "Elevation",
      "NDVI" = "NDVI",
      "W_Ecosystm" = "Ecosystem"
    )
  )

graphB <- ggplot(
  top3_df,
  aes(
    x = reorder(feature, importance),
    y = importance
  )
  ) +
  geom_col(fill = "forestgreen", width = 0.8) +
  coord_polar() +

  # annotations des anneaux
  annotate("text", x = 0.5, y = 100,
           label = "100",
           colour = "grey50",
           size = 4) +
  annotate("text", x = 0.5, y = 200,
           label = "200",
           colour = "grey50",
           size = 4) +
  annotate("text", x = 0.5, y = 300,
           label = "300",
           colour = "grey50",
           size = 4) +

  labs(
    title = " ",
    subtitle = "Variables most important for distinguishing Impatiens species",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(graphB)



# --------------------------------------------------
#  PLOT C – Density plot of NDVI
# --------------------------------------------------

graphC <- ggplot(matrix_full_tip_top,
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
    title = " ",
    x = "NDVI",
    y = "Density"
  ) +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 14)) +
  theme_bw()

print(graphC)



# --------------------------------------------------
#  PLOT D – Violin plot of elevation
# --------------------------------------------------

graphD <- ggplot(
  matrix_full_tip_top,
  aes(
    x = elevation,
    y = species,
    fill = species
  )
) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(
    width = 0.15,
    fill = "white",
    outlier.shape = NA
  ) +
  scale_fill_manual(values = c(
    "Impatiens noli-tangere" = "#223e8b",
    "Impatiens glandulifera" = "red"
  )) +
  labs(
    title = " ",
    x = "Elevation [m]",
    y = NULL,
    fill = "Species"
  ) +
  theme(legend.title = element_text(size = 13),
        legend.text = element_text(size = 12))
  theme_bw()

print(graphD)




# ============================================================
#  COMBINE THE PLOTS WITH COWPLOT
#
#  ggdraw() creates an empty 1 × 1 canvas using relative units.
#  draw_plot(plot, x, y, width, height) places each plot on this canvas.
#
#  x, y    = bottom-left corner of the plot (0 = left/bottom edge, 1 = right/top edge)
#  width   = plot width as a proportion of the full figure
#  height  = plot height as a proportion of the full figure
#
#  Layout:
#    ┌──────────────────────────────────────────┐  y = 0.60 → 1.00
#    │             graphA  (map)                │
#    ├────────────────────┬─────────────────────┤  y = 0.25 → 0.60
#    │  graphB (circular) │  graphC  (density)  │
#    ├────────────────────┴─────────────────────┤  y = 0.00 → 0.25
#    │           graphD  (violin)               │
#    └──────────────────────────────────────────┘
# ============================================================


# ---- Final figure ----
# x, y   = bottom-left corner of the plot (0 = left/bottom edge, 1 = right/top edge)
# width  = plot width as a proportion of the full figure
# height = plot height as a proportion of the full figure

figure_finale <- ggdraw() +

  draw_plot(graphA,      x = 0.00, y = 0.60, width = 1.00, height = 0.40) +
  draw_plot(graphB,      x = 0.00, y = 0.25, width = 0.45, height = 0.35) +
  draw_plot(graphC,      x = 0.45, y = 0.25, width = 0.55, height = 0.35) +
  draw_plot(graphD,      x = 0.00, y = 0.00, width = 1.00, height = 0.25) +

  draw_label("A", x = 0.15, y = 0.99, fontface = "bold", size = 20) +
  draw_label("Localisation of the observations", x = 0.33, y = 0.99, size = 20) +
  draw_label("B", x = 0.02, y = 0.59, fontface = "bold", size = 20) +
  draw_label("Top 3 of Random Forest predictors", x = 0.2, y = 0.59, size = 20) +
  draw_label("C", x = 0.46, y = 0.59, fontface = "bold", size = 20) +
  draw_label("NDVI distribution", x = 0.57, y = 0.59, size = 20) +
  draw_label("D", x = 0.08, y = 0.24, fontface = "bold", size = 20) +
  draw_label("Elevation distribution", x = 0.2, y = 0.24, size = 20)

print(figure_finale)
# Don't look with this window !!!
# Dowload the figure to see the good layout !!



# --------------------------------------------------
#  EXPORT
# --------------------------------------------------

ggsave("figure_Impatiens_suisse.png", figure_finale,
       width = 13, height = 16, dpi = 300, bg = "white")

ggsave("figure_Impatiens_suisse.pdf", figure_finale,
       width = 13, height = 16, device = cairo_pdf)

message("Export terminé dans : ", getwd())
