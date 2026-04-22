###############################################################################
# ADD CLIMATE DATA TO AN EXISTING SPECIES COORDINATE TABLE
###############################################################################
quartz()
# =========================
# 1) PACKAGES
# =========================

library(Rchelsa)
library(terra)
library(dplyr)
library(ggplot2)

# =========================
# 2) STARTING DATASET
# =========================
# We take our dataset with our 2 species of Impatiens
matrix_full_final

# =========================
# 3) CREATE A SPATIAL OBJECT
# =========================
# CHELSA requires coordinates. We therefore create a spatial vector
# from the longitude and latitude columns.

pts_v <- terra::vect(
  matrix_full_final,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

# Extract simple coordinates as a standard data frame
coords_df <- as.data.frame(terra::geom(pts_v)[, c("x", "y")]) %>%
  rename(
    longitude = x,
    latitude = y
  ) %>%
  mutate(occurrence_id = matrix_full_final$occurrence_id)

coords_df

# =========================
# 4) EXTRACT MONTHLY Tmax FOR 2021
# =========================
# CHELSA variable naming:
# - tas    = near-surface air temperature
# - tasmin = minimum near-surface air temperature
# - tasmax = maximum near-surface air temperature
# - pr     = precipitation
#
# Temperature values are often returned in Kelvin.
# Conversion to Celsius: °C = K - 273.15

coords <- matrix_full_final %>%
  dplyr::select(longitude, latitude) %>%
  dplyr::distinct()
# --> I use distinct() to avoid duplicat
## Warning: when I will mix my data (points with climate): 
## say to R to link data with the latitude and longitude (together)

tmax_r <- getChelsa(
  var       = "tasmax",
  coords    = coords,
  startdate = as.Date("2021-01-01"),
  enddate   = as.Date("2021-12-31"),
  dataset   = "chelsa-monthly"
)
# --> The year of 2021 is the earlier year available 

# Remove the time column with dplyr, then convert to matrix
tmax_mat <- tmax_r %>%
  dplyr::select(-time) %>%
  as.matrix()



# Calculate the mean across the 12 months for each point
# colMeans() works by column, and here each column corresponds to one point
tmax_mean_k <- colMeans(tmax_mat, na.rm = TRUE)

# Convert Kelvin to Celsius
tmax_mean_c <- tmax_mean_k - 273.15

# Create a table containing the new climate variable
tmax_df <- data.frame(
  occurrence_id = coords,
  tmax_mean_c = as.numeric(tmax_mean_c)
)

tmax_df

# =========================
# 5) EXTRACT MONTHLY PRECIPITATION FOR 2021
# =========================

prec_r <- getChelsa(
  var       = "pr",
  coords    = coords %>% select(longitude, latitude),
  startdate = as.Date("2021-01-01"),
  enddate   = as.Date("2021-12-31"),
  dataset   = "chelsa-monthly"
)

# Remove the time column with dplyr, then convert to matrix
prec_mat <- prec_r %>%
  select(-time) %>%
  as.matrix()

# Calculate the mean across the 12 months for each point
prec_mean <- colMeans(prec_mat, na.rm = TRUE)

# Create a table containing the precipitation variable
prec_df <- data.frame(
  occurrence_id = coords,
  prec_mean_annual = as.numeric(prec_mean)
)

prec_df

# =========================
# 6) JOIN THE NEW CLIMATE VARIABLES
#    TO THE ORIGINAL DATASET
# =========================
# This is the key teaching point:
# we start from an existing dataset and add new columns
# extracted from an external source.

# --> due the use of distinct to delate duplicate
# I must create column that combine longitude and latitude coordinates
tmax_df <- tmax_df %>%
  mutate(coord_id = paste0(`occurrence_id.longitude`, "_", `occurrence_id.latitude`))

prec_df <- prec_df %>%
  mutate(coord_id = paste0(`occurrence_id.longitude`, "_", `occurrence_id.latitude`))

matrix_full_final <- matrix_full_final %>%
  mutate(coord_id = paste0(`longitude`, "_", `latitude`))


species_climate_df <- matrix_full_final %>%
  left_join(tmax_df, by = "coord_id") %>%
  left_join(prec_df, by = "coord_id")

species_climate_df


# =========================
# 7) CHECK THE RESULT
# =========================

dim(matrix_full_final)           # original dimensions
# 1455    6
dim(species_climate_df)   # enriched dimensions
# 1455    12
names(species_climate_df) # column names after enrichment

# =========================
# 8) PLOT THE DISTRIBUTION OF ANNUAL MEAN Tmax
# =========================

ggplot(species_climate_df, aes(x = tmax_mean_c)) +
  geom_density(color = "darkred", fill = "salmon", adjust = 1.5) +
  theme_classic() +
  labs(
    title = "Impatiens sp.: annual mean Tmax (2021)",
    x = "Annual mean Tmax (°C)",
    y = "Density"
  )

# =========================
# 9) PLOT THE DISTRIBUTION OF ANNUAL MEAN PRECIPITATION
# =========================

ggplot(species_climate_df, aes(x = prec_mean_annual)) +
  geom_density(color = "black", fill = "darkgreen", adjust = 1.5) +
  theme_classic() +
  labs(
    title = "Impatiens sp.: annual mean precipitation (2021)",
    x = "Annual mean precipitation",
    y = "Density"
  )



# 10)  CURRENT CLIMATE VS FUTURE CLIMATE
#     SIMPLIFIED EXAMPLE WITH JULY ONLY
# =========================
# Here, instead of averaging all 12 months, we extract climate data
# for one particular month only: July.
#
# This is often easier for teaching because the workflow is simpler:
# one month -> one extraction -> one new column

# ------------------------------------------------------------
# 10A) CURRENT CLIMATE: July temperature
#      climatology over 1981-2010
# ------------------------------------------------------------

tas_cur_july <- getChelsa(
  var     = "tas",
  coords  = coords %>% select(longitude, latitude),
  date    = c(7, 1981, 2010),   # July climatology
  dataset = "chelsa-climatologies"
)

tas_cur_july_df <- data.frame(
  occurrence_id = coords,
  tas_current_july_c = tas_cur_july %>%
    select(-time) %>%
    unlist() %>%
    as.numeric() - 273.15
)

tas_cur_july_df

# ------------------------------------------------------------
# 10B) FUTURE CLIMATE: July temperature in 2050 under SSP126
# ------------------------------------------------------------

tas_fut_july <- getChelsa(
  var     = "tas",
  coords  = coords %>% select(longitude, latitude),
  date    = as.Date("2050-07-01"),
  dataset = "chelsa-climatologies",
  ssp     = "ssp126",
  forcing = "MPI-ESM1-2-HR"
)

tas_fut_july_df <- data.frame(
  occurrence_id = coords,
  tas_future_july_2050_c = tas_fut_july %>%
    select(-time) %>%
    unlist() %>%
    as.numeric() - 273.15
)

tas_fut_july_df

# ------------------------------------------------------------
# 10C) ADD CURRENT AND FUTURE JULY TEMPERATURE
#      TO THE ORIGINAL TABLE
# ------------------------------------------------------------

tas_cur_july_df <- tas_cur_july_df %>%
  mutate(coord_id = paste0(`occurrence_id.longitude`, "_", `occurrence_id.latitude`))

tas_fut_july_df <- tas_fut_july_df %>%
  mutate(coord_id = paste0(`occurrence_id.longitude`, "_", `occurrence_id.latitude`))


species_climate_future_df <- species_climate_df %>%
  left_join(tas_cur_july_df, by = "coord_id") %>%
  left_join(tas_fut_july_df, by = "coord_id") %>%
  mutate(
    delta_tas_july_c = tas_future_july_2050_c - tas_current_july_c
  )

species_climate_future_df

# ------------------------------------------------------------
# 10C) ADD CURRENT AND FUTURE CLIMATE TO THE TABLE
# ------------------------------------------------------------

species_climate_future_df <- species_climate_df %>%
  left_join(tas_cur_df, by = "coord_id") %>%
  left_join(tas_fut_df, by = "coord_id") %>%
  mutate(
    delta_tas_c = tas_future_2050_c - tas_current_c
  )

## --> Problem: tas_cur_df et tas_fut_df don't exist
## --> On ne les a pas créé (même dans le doc de base du prof y a pas)

species_climate_future_df

# =========================
# 11) PLOT CURRENT VS FUTURE TEMPERATURE
# =========================

ggplot(species_climate_future_df, aes(x = tas_current_july_c, y = tas_future_july_2050_c)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(
    title = "Impatiens sp.: current vs future July temperature",
    x = "Current July temperature (°C)",
    y = "Future July temperature in 2050 (°C)"
  )



