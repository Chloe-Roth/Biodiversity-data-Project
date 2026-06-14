# ============================================================
# MAPS 
# ============================================================

# Goal of this script:
# Visualise on the map the distribution of the Impatiens species
# coupled to some environmental variables (there: temperature and elevation)



# ------------------------------------------------------------
# Prepare the data
# ------------------------------------------------------------

#### Matrix ####

# Look at the structure of the table
glimpse(matrix_full_tip_top)

# Clean and prepare the table
# We keep only rows with valid coordinates
matrix <- matrix_full_tip_top %>%
  mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude),
    species   = as.factor(species),
    elevation = as.numeric(elevation),
    tmax_mean_c = as.numeric(tmax_mean_c),
    prec_mean_annual = as.numeric(prec_mean_annual),
    temp_mean_c = as.numeric(temp_mean_c),
    tas_current_july_c = as.numeric(tas_current_july_c),
    NDVI      = as.numeric(NDVI)
  ) %>%
  filter(
    is.finite(latitude),
    is.finite(longitude),
    is.finite(elevation)
  )

# Quick summary
matrix %>% count(species)
# Impatiens glandulifera: 1632
# Impatiens noli-tangere: 2067

# Convert the occurrence table to an sf object
# EPSG:4326 is the usual coordinate reference system for longitude/latitude
matrix_sf <- st_as_sf(matrix, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)


#### Switzerland layer ####

# Get Switzerland as an sf polygon from Natural Earth
switzerland <- rnaturalearth::ne_countries(
  country = "Switzerland",
  scale = "medium",
  returnclass = "sf"
)

# A slightly larger bounding box around Switzerland, useful for plots
swiss_bbox <- st_bbox(switzerland)


#### Establish the color for each species ####
species_cols <- c(
  "Impatiens glandulifera" = "#ff4400",
  "Impatiens noli-tangere" = "#2b59bd"
)


quartz()




# ------------------------------------------------------------
# Map of Impatiens species distribution by temperature 
# ------------------------------------------------------------

# I use colour to distinghish the different temperature (an environmental variable)
# Species is shown by point shape

p_gg_temp <- ggplot() +
  geom_sf(data = switzerland, fill = "grey95", colour = "grey40") +
  geom_point(
    data = matrix,
    aes(x = longitude, y = latitude, colour = temp_mean_c, shape = species),
    size = 3,
    alpha = 0.85
  ) +
  scale_colour_viridis_c(option = "plasma") +
  coord_sf() +
  labs(
    title = "Impatiens species distribution by temperature",
    colour = "Temperature",
    shape = "Species",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13)

p_gg_temp
# We see a lot of triangles and circles in orange 
# --> similar environment for the 2 species

# But there is no yellow triangle (Impatiens noli-tangere) (only circles in Tessin)
# And there is no purple circles (Impatiens glandulifera) (only triangles)
# So, that demonstrate that the 2 Impatiens species can grow at similar temperature (orange: 5°).
# But they have also specific temperature: 
#   Impatiens noli-tangere grows at higher temperature (yellow: > 10°)
#   Impatiens glandulifer grows at lower temperature (purple: < 0°)




# ------------------------------------------------------------
# Facet: one map per species
# ------------------------------------------------------------

p_gg_facet <- ggplot() +
  geom_sf(data = switzerland, fill = "grey96", colour = "grey50") +
  geom_point(
    data = matrix,
    aes(x = longitude, y = latitude, colour = elevation),
    size = 1.5,
    alpha = 0.85
  ) +
  scale_colour_viridis_c(option = "viridis") +
  facet_wrap(~ species) +
  coord_sf() +
  labs(
    title = "Impatiens distribution in Switzerland - one panel per species",
    colour = "Elevation [m]",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 13)

p_gg_facet

# If necessary addapt the quartz window (elongated rectangle) to well see

# With this map we can well distinghish the distribution of
# the 2 Impatiens species
# In addition, with colors we can look at the elevation (the more
# important environmental variable idendified by Random Forest)

# We see that the invasive species Impatiens glandulifera is less
# present in Switzerland: mainly in the Northeast and a little in 
# the Planteau and in Tessin.
# In contrast the indigenous species Impatiens noli-tangere in most
# present in Switzerland. It's a good new, because the species is 
# indigenous, so Switzerland is her house !
# More precisely, Impatiens noli-tangere is a lot present in the plateau
# and a little in the Valais and inn the Tessin.

# In terms of elevation we see a clear difference with the higher elevation.
# Impatiens glandulifera stays generally at less than 1000 m of elevation.
# Impatiens glandulifera can go to 1500 but it's more rare.
# In contrast, Impatiens noli-tangere is present at higher elevation,
# she can go to 2500 m of elevation. 
# That is the reason why we find I. noli-tangere in the Alpes, but not I. glandulifera.

# So, there are different hypothesis why I. glandulifera is not 
# present in the Alps:
#   1) I. glandulifera is not adapt to higher elevation
#   2) I. glandulifera did not have time to go to the Alps




# ------------------------------------------------------------
# Leaflet map coloured by species
# ------------------------------------------------------------

# I use leaflet to have an interactive map to zoom in or zoom out,
# but also the have supplement information by clikling on the observation dots.

# More base maps: https://leaflet-extras.github.io/leaflet-providers/preview/
# (use the name of the base map in the code)

matrix <- matrix %>%
  mutate(
    tooltip = paste0(
      "<br><b>Species: </b>", species,
      "<br><b>Elevation: </b>", round(elevation, 0), " m",
      "<br><b>Annual precipitation: </b>", round(prec_mean_annual, 2), " mm",
      "<br><b>Annual temperature: </b>", round(temp_mean_c, 2), " C°",
      "<br><b>Maximum temperature: </b>", round(tmax_mean_c, 2), " C°",
      "<br><b>Temperature in July: </b>", round(tas_current_july_c, 2), " C°",
      "<br><b>NDVI: </b>", round(NDVI, 3)
    ),
    point_id = paste0("point_", row_number())
  )

pal_species <- colorFactor(
  palette = species_cols,
  domain = matrix$species
)

leaflet(matrix) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>% # or Esri.WorldImagery
  addPolygons(
    data = switzerland,
    fillColor = "transparent",
    color = "grey40",
    weight = 1.2
  ) %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,
    color = "black",
    weight = 1,
    fillColor = ~pal_species(species),
    fillOpacity = 0.85,
    popup = ~tooltip,
    group = ~species
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_species,
    values = ~species,
    title = "Species"
  ) %>%
  addLayersControl(
    overlayGroups = levels(matrix$species),
    options = layersControlOptions(collapsed = FALSE)
  )

# We can zoom in the Swiss map to see the name of city.
# We can also zoom out to see all the world (it's less useful, 
# since we only have observations in Switzerland, but it's funny)

# With the color points we can distinguish the 2 Impatiens species.
# We can also selected or deselected one species. 
# As the previous map, it is useful to well distinghish where there 
# is overlap. However, it is more precise in this map caused to 
# the function of zoom.

# In addition, we can have supplement information for each observation
# by clickling on the dots. We have information about elevation, 
# precipitation, temperature (annual, maximum, in July) and NDVI.

# So, it's a real complete and interactive map. 




# ------------------------------------------------------------
# 3D terrain map (RAYSHADER)
# ------------------------------------------------------------

# I have created an 3D visualisation map to see my plant species
# on a 3D map of Switzerland

# It's not very usefull for my project so I put the code in comment
# Despite everything, the code works and produces a cool and fun map


#### Download and prepare an elevation raster ####
# quartz()

# We use Switzerland as the area of interest
# The raster package is used here because rayshader works very well with raster
# elevation_switzerland <- elevatr::get_elev_raster(
#   locations = switzerland,
#   z = 7,
#   clip = "locations"
# )

# Convert elevation raster to a matrix for rayshader
# elmat <- rayshader::raster_to_matrix(elevation_switzerland)


#### 3D terrain map ####

# This opens an rgl window. On some computers or in RStudio Server, 3D display
# may not be available.

# elmat %>%
#   sphere_shade(texture = "desert") %>%
#   add_shadow(ray_shade(elmat, zscale = 100), 0.5) %>%
#   add_shadow(ambient_shade(elmat), 0.3) %>%
#   plot_3d(
#     heightmap = elmat,
#     zscale = 120,
#     fov = 0,
#     theta = 135,
#     phi = 45,
#     zoom = 0.75,
#     windowsize = c(1400, 800)
#   )

# Extract elevation at the occurrence points to place them correctly in 3D.
# points_sp <- sp::SpatialPoints(
#   coords = matrix[, c("longitude", "latitude")],
#   proj4string = sp::CRS(SRS_string = "EPSG:4326")
# )
# elevation_points <- raster::extract(elevation_switzerland, points_sp, method = "bilinear")

# Add points on the 3D map.
# rayshader::render_points(
#   extent = raster::extent(elevation_switzerland),
#   lat = matrix$latitude,
#   long = matrix$longitude,
#   altitude = elevation_points + 100,
#   zscale = 120,
#   size = 6,
#   color = species_cols[as.character(matrix$species)]
# )

# To close the 3D window if needed:
# rayshader::rgl::rgl.close()

