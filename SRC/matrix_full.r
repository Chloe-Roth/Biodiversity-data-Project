
# =========================
# 1) PACKAGES
# =========================

library(rgbif)         # access to GBIF data
library(rnaturalearth) # country maps 
library(ggplot2)       # graphics
library(rinat)         # access to iNaturalist data
library(raster)        # spatial extent management
library(dplyr)         # table manipulation
library(sf)            # modern spatial objects

# Disable spherical geometry for simpler spatial operations 
sf_use_s2(FALSE)


###############################################################################
# 2) USER PARAMETERS
###############################################################################

# Species of interest
myspecies1 <- "Impatiens glandulifera"
myspecies2 <- "Impatiens noli-tangere"

# Maximum number of GBIF records to download
# limit for species 1
gbif_limit1 <- 6000
# limit for species 1
gbif_limit2 <- 11000
# I adapt the limit to have in finally approxymately the same number of records for each species inn Switzerland


# Time filtering period
date_start <- as.Date("2020-01-01")
date_end   <- as.Date("2025-12-31")

# Simplified geographic extent for Switzerland
xmin <- 6
xmax <- 11
ymin <- 46
ymax <- 48


###############################################################################
# 3) BASE MAP: SWITZERLAND
###############################################################################

# Download the outline of Switzerland
Switzerland <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = "Switzerland"
)

# Simple visualization of the map
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  theme_classic()


###############################################################################
# 4) DOWNLOAD GBIF DATA
###############################################################################

###
### Species 1: Impatiens glandulifera ###
###

# Download occurrences with coordinates
gbif_raw1 <- occ_data(
  scientificName = myspecies1,
  hasCoordinate = TRUE,
  limit = gbif_limit1
)

# Extract the main data table
gbif_occ1 <- gbif_raw1$data

# Quick inspection
head(gbif_occ1)
names(gbif_occ1)

# Select occurrences located in Switzerland
gbif_switzerland1 <- gbif_occ1[gbif_occ1$country == "Switzerland",]

# Check number of records
nrow(gbif_switzerland1)
# 756

quartz()
# Quick base plot for checking
plot(
  gbif_switzerland1$decimalLongitude,
  gbif_switzerland1$decimalLatitude,
  pch = 16,
  col = "darkgreen",
  xlab = "Longitude",
  ylab = "Latitude",
  main = "GBIF occurrences in Switzerland"
)

# Map showing GBIF occurrences only
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_switzerland1,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  theme_classic()

###
### Species 2: Impatiens noli-tangere ###
###

# Download occurrences with coordinates
gbif_raw2 <- occ_data(
  scientificName = myspecies2,
  hasCoordinate = TRUE,
  limit = gbif_limit2
)

# Extract the main data table
gbif_occ2 <- gbif_raw2$data

# Quick inspection
head(gbif_occ2)
names(gbif_occ2)

# Select occurrences located in Switzerland
gbif_switzerland2 <- gbif_occ2[gbif_occ2$country == "Switzerland",]

# Check number of records
nrow(gbif_switzerland2)
# 766

quartz()
# Quick base plot for checking
plot(
  gbif_switzerland2$decimalLongitude,
  gbif_switzerland2$decimalLatitude,
  pch = 16,
  col = "darkgreen",
  xlab = "Longitude",
  ylab = "Latitude",
  main = "GBIF occurrences in Switzerland"
)

# Map showing GBIF occurrences only
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = gbif_switzerland2,
    aes(x = decimalLongitude, y = decimalLatitude),
    size = 3,
    shape = 21,
    fill = "darkgreen",
    color = "black"
  ) +
  theme_classic()


###############################################################################
# 5) FORMAT GBIF DATA
###############################################################################

# Keep only the useful columns
# eventDate may contain date + time; as.Date() keeps only the date
# For species 1
data_gbif1 <- data.frame(
  species   = gbif_switzerland1$species,
  latitude  = gbif_switzerland1$decimalLatitude,
  longitude = gbif_switzerland1$decimalLongitude,
  date_obs  = as.Date(gbif_switzerland1$eventDate),
  source    = "gbif"
)
# For species 2
data_gbif2 <- data.frame(
  species   = gbif_switzerland2$species,
  latitude  = gbif_switzerland2$decimalLatitude,
  longitude = gbif_switzerland2$decimalLongitude,
  date_obs  = as.Date(gbif_switzerland2$eventDate),
  source    = "gbif"
)

# Check structure for species 1 and 2
head(data_gbif1)
str(data_gbif1)

head(data_gbif2)
str(data_gbif2)


###############################################################################
# 6) DOWNLOAD iNaturalist DATA
###############################################################################

# Query iNaturalist for species 1 in Switzerland
# place_id = "switzerland" usually works with rinat
inat_raw1 <- get_inat_obs(
  query = myspecies1,
  place_id = "switzerland"
)

# Inspect the structure
head(inat_raw1)
names(inat_raw1)
nrow(inat_raw1)
# 100

# Map showing iNaturalist occurrences only
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw1,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  theme_classic()


# Query iNaturalist for species 2 in Switzerland
# place_id = "switzerland" usually works with rinat
inat_raw2 <- get_inat_obs(
  query = myspecies2,
  place_id = "switzerland"
)

# Inspect the structure
head(inat_raw2)
names(inat_raw2)
nrow(inat_raw2)
# 100

# Map showing iNaturalist occurrences only
ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = inat_raw2,
    aes(x = longitude, y = latitude),
    size = 3,
    shape = 21,
    fill = "darkred",
    color = "black"
  ) +
  theme_classic()


###############################################################################
# 7) FORMAT iNaturalist DATA
###############################################################################

# In most rinat versions the observation date is stored in observed_on
# Convert it to Date format
# We do it for the 2 species
data_inat1 <- data.frame(
  species   = inat_raw1$scientific_name,
  latitude  = inat_raw1$latitude,
  longitude = inat_raw1$longitude,
  date_obs  = as.Date(inat_raw1$observed_on),
  source    = "inat"
)

data_inat2 <- data.frame(
  species   = inat_raw2$scientific_name,
  latitude  = inat_raw2$latitude,
  longitude = inat_raw2$longitude,
  date_obs  = as.Date(inat_raw2$observed_on),
  source    = "inat"
)

# Check structure
head(data_inat1)
str(data_inat1)

head(data_inat2)
str(data_inat2)


###############################################################################
# 8) MERGE OUR DATABASES
###############################################################################

# Here we want to STACK GBIF and iNaturalist observations.
# Therefore we use bind_rows() instead of merge().
matrix_full <- bind_rows(data_gbif1, data_inat1, data_gbif2, data_inat2)

# Check results
head(matrix_full)
table(matrix_full$source, useNA = "ifany")
# 1522 obs of gbif + 200 obs of inat
summary(matrix_full$date_obs)
# obs from 2005-08-05 to 2026-03-19



###############################################################################
# 9) TIME FILTERING BETWEEN TWO DATES
###############################################################################

# Keep only observations within the selected time interval
matrix_full_date <- matrix_full %>%
  filter(!is.na(date_obs)) %>%
  filter(date_obs >= date_start & date_obs <= date_end)

# Check results
head(matrix_full_date)
summary(matrix_full_date$date_obs)
# obs from 2020-07-12 to 2025-10-28
table(matrix_full_date$source)
# 1521 obs of gbif + 193 obs of inat


###############################################################################
# 10) CHECK THE QUALITY OF OUR MATRIX
###############################################################################

#### Duplicate ####

# Check the duplicate
anyDuplicated(matrix_full_date)
# 78 duplicates

# Look at the duplicated
matrix_full_date[duplicated(matrix_full_date), ]

# Delate the duplicates
matrix_full_clean <- matrix_full_date %>%
  dplyr::distinct(species, latitude, longitude, date_obs, source, .keep_all = TRUE)

# Check results
head(matrix_full_clean)
summary(matrix_full_clean$date_obs)
# obs from 2020-07-12 to 2025-10-28 --> no change
table(matrix_full_clean$source)
# 1390 obs of gbif + 190 obs of inat

#### Name of species ####

# Check the name of our species
unique(matrix_full_clean$species)
# OK: "Impatiens glandulifera"  "Impatiens noli-tangere" 
# Not OK: "Impatientinum asiaticum" "Semiaphis nolitangere" "Agromyzidae" "Insecta" "Podosphaera balsaminae" 

# Keep only "Impatiens glandulifera" & "Impatiens noli-tangere"
matrix_full_final <- matrix_full_clean %>%
  dplyr::filter(species %in% c(
    "Impatiens glandulifera",
    "Impatiens noli-tangere"
  ))

# Check the result
unique(matrix_full_final$species)
# "Impatiens glandulifera"  "Impatiens noli-tangere" -> OK !
matrix_full_final %>%
  dplyr::count(species, sort = TRUE)
# 844 Impatiens glandulifera + 730 Impatiens noli-tangere 
table(matrix_full_final$source)
# 1390 obs of gbif + 184 obs of inat

#### Last visual check ####
View(matrix_full_final)


###############################################################################
# 11) MAP OF COMBINED DATA
###############################################################################

ggplot(data = Switzerland) +
  geom_sf(fill = "grey95", color = "black") +
  geom_point(
    data = matrix_full_final,
    aes(x = longitude, y = latitude, fill = source),
    size = 3,
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  theme_classic()


###############################################################################
# 12) DEFINE A SIMPLE SPATIAL EXTENT
###############################################################################

##### Crop the background using coordinates

# Define the spatial extent
extent(Switzerland)
ext_Switzerland_cut <- as(raster::extent(6, 11, 45, 48), "SpatialPolygons")

# Crop Switzerland map to the defined extent
Switzerland_crop <- st_crop(Switzerland, ext_Switzerland_cut)

# Plot cropped map with occurrence points
ggplot(data = Switzerland_crop) +
  geom_sf() +
  geom_point(
    data = matrix_full_final,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  theme_classic()
# With this plot we have check the CRS projection (if it's the same for gbif and inat)
# It seems good


##### Exclude points outside the specified spatial extent

# Convert occurrences to sf object
data_gbif_sf <- st_as_sf(matrix_full_final, coords = c("longitude", "latitude"), crs = 4326)

# Convert cropped Switzerland polygon to sf
Switzerland_crop_sf <- st_as_sf(Switzerland_crop)

# Identify points located inside the spatial extent
cur_data <- matrix_full_final[as.matrix(st_intersects(data_gbif_sf, Switzerland_crop_sf)),]

# Plot cropped Switzerland map with filtered points
ggplot(data = Switzerland_crop) +
  geom_sf() +
  geom_point(
    data = cur_data,
    aes(x = longitude, y = latitude, fill = source),
    size = 4,
    shape = 23
  ) +
  theme_classic()



###############################################################################
# 13) OPTIONAL SAVE OF THE FINAL TABLE
###############################################################################

# Save filtered occurrence table
write.csv(
  cur_data,
  file = "matrix_full_final.csv",
  row.names = FALSE
)



