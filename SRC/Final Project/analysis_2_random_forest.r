# Here we will do Random Forest to identify the more important
# environmental variables to distinguish the 2 Impatiens species.


# --------------------------------------------------
# 1) Prepare occurrence data for machine learning
# --------------------------------------------------

# I create a clean table for the Random Forest model
#
# The response variable is:
# - species

# The predictor variables are:
# - eco_values
# - Temperatur
# - Moisture
# - Landcover
# - Landforms
# - Climate_Re
# - Red, Green, Blue
# - W_Ecosystm
# - elevation
# - NDVI
# I can not include tmax_mean_c, prec_mean_annual, temp_mean_c and tas_current_july_c
# because they have more than 53 categories (they have between 100 and 500 categories)

ml_matrix <- matrix_full_tip_top %>%
  select(
    species,
    eco_values,
    Temperatur,
    Moisture,
    Landcover,
    Landforms,
    Climate_Re,
    Red,
    Green,
    Blue,
    W_Ecosystm,
    elevation,
    NDVI
  )

# Remove missing values
# (Because Random Forest cannot use rows with NA values)
ml_matrix <- na.omit(ml_matrix)

# Convert the response variable to a factor (= a categorical variable)
ml_matrix$species <- as.factor(ml_matrix$species)

# Convert categorical predictors to factors (= categorical predictors)
ml_matrix$Temperatur <- as.factor(ml_matrix$Temperatur)
ml_matrix$Moisture   <- as.factor(ml_matrix$Moisture)
ml_matrix$Landcover  <- as.factor(ml_matrix$Landcover)
ml_matrix$Landforms  <- as.factor(ml_matrix$Landforms)
ml_matrix$Climate_Re <- as.factor(ml_matrix$Climate_Re)
ml_matrix$W_Ecosystm <- as.factor(ml_matrix$W_Ecosystm)

# Check the final structure
str(ml_matrix)

# Check the number of samples per species
table(ml_matrix$species)
# Impatiens glandulifera: 1632
# Impatiens noli-tangere: 2067


# --------------------------------------------------
# 2) Train / test split
# --------------------------------------------------

# We split the data into:
# - 70% training data --> used to build the model
# - 30% testing data --> used to evaluate the model on unseen data

set.seed(123)

train_index <- createDataPartition(
  y = ml_matrix$species,
  p = 0.7,
  list = FALSE
)

train_matrix <- ml_matrix[train_index, ]
test_matrix  <- ml_matrix[-train_index, ]

# Check that both species are present in both datasets
table(train_matrix$species)
# Impatiens glandulifera Impatiens noli-tangere 
#                  1143                   1447 
table(test_matrix$species)
# Impatiens glandulifera Impatiens noli-tangere 
#                   489                    620 
# It's OK !


# --------------------------------------------------
# 3) Train the Random Forest model
# --------------------------------------------------

rf_species <- randomForest(
  species ~ .,
  data = train_matrix,
  ntree = 500,
  importance = TRUE
)
# Explanation of the code:
# species ~ . => predict species using all other columns as predictors
# ntree = 500 => the forest contains 500 trees
# importance = TRUE => calculate variable importance

print(rf_species)


# --------------------------------------------------
# 4) Prediction on test data
# --------------------------------------------------

# I will ask the model to predict the species
# of the test dataset

pred_species <- predict(
  rf_species,
  newdata = test_matrix
)

head(pred_species)


# --------------------------------------------------
# 5) Model evaluation
# --------------------------------------------------

# The confusion matrix compares:
# - predicted species
# - observed species
# It gives an estimate of model performance

confusionMatrix(
  data = pred_species,
  reference = test_matrix$species
)
# Accuracy : 0.8359 
# High nbr for glandulifera x glandulifera: 397
# High nbr for noli-tangere x noli-tangere: 530
# It's good


# --------------------------------------------------
# 6) Feature importance
# --------------------------------------------------
# Random Forest can estimate which variables are most useful
# for discriminating the species! 
# That is the reason why we use this tool

importance(rf_species)

# Basic Random Forest importance plot
varImpPlot(rf_species)

# Create a cleaner ggplot version
importance_matrix <- importance(rf_species) %>%
  as.data.frame()

importance_matrix$feature <- rownames(importance_matrix)

importance_matrix <- importance_matrix %>%
  arrange(desc(MeanDecreaseGini))

ggplot(
  importance_matrix,
  aes(
    x = reorder(feature, MeanDecreaseGini),
    y = MeanDecreaseGini
  )
) +
  geom_col(fill = "#81c72c") +
  coord_flip() +
  theme_classic() +
  labs(
    title = "Most important features to discriminate the Impatiens species",
    x = "Feature",
    y = "Mean decrease in Gini"
  )
# This plot provides an overview of the contribution of each factor.
# We can evaluate which factors are the most important to discriminate the species.
# Most important factor: elevation
# Second most important factor: NDVI
# Thirty most important factor: W_Ecosystem

# So, based on these results we know that we can especially focus on elevation, 
# NDVI and W_Ecosystem for our analysis. 
# In contrast, I can less focus on Landcover, Landforms, Climate_Re,
# Temperature and Moisture.

