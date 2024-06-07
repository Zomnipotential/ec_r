# import csv file into a for R suitable datastructure
install.packages("jsonlite")
install.packages("jsonlite", repos = "https://cloud.r-project.org/")
# which R gives /usr/local/bin/R
# defaults write org.R-project.R r.rpath.mac /usr/local/bin/R
# The downloaded binary packages are in
# /var/folders/1s/vk0n_73d4ndcz49qcnxf6wf40000gp/T/
# /RtmpBPa30s/downloaded_packages

is_dataframe <- function(table) {
  if (is.data.frame(table)) {
    print(paste("The", deparse(substitute(table)), "is a dataframe"))
  } else {
    print(paste("The", deparse(substitute(table)), "is not a dataframe"))
  }
}

list.files()

# Load the data
data <- read.csv("./2404152328_Cars.csv", header = TRUE, sep = ",")
is_dataframe(data)

# Remove everything in columns 11 and onwards
feed <- data[, 1:10]
is_dataframe(feed)

# Print the feed
print(head(feed))

# Get the number of empty cells in each column and print the result
empty_cells <- sapply(feed, function(x) sum(is.na(x)))
print(empty_cells)

# Get the type of each column and print the result
column_types <- sapply(feed, class)
print(column_types)

# Convert the first column of feed to integers
feed[, 1] <- as.integer(feed[, 1])

# change the type of the column Pris..Y. to integer.
# If the string contains spaces, remove them
feed$Pris..Y. <- as.integer(gsub(" ", "", feed$Pris..Y.))

# Get the type of each column and print the result
column_types <- sapply(feed, class)
print(column_types)

# Convert all columns of the type character to all small letters
#feed <- sapply(feed, function(x) if (is.character(x)) tolower(x) else x)

# Separate the columns of the type integer as a new data frame
#all_integers <- feed[, sapply(feed, is.integer)]
#print(all_integers)

# Print the dimensions of feed
#print(dim(all_integers))

# For each of the columns 3, 6 - 9 of feed, remove all whitespaces
spaces <- "^\\s+|\\s+$"
feed[, c(3, 6:9)] <- sapply(feed[, c(3, 6:9)], function(x) gsub(spaces, "", x))

# Convert all characters in the columns 3, 6 - 9 of feed to small letters
feed[, c(3, 6:9)] <- sapply(feed[, c(3, 6:9)], function(x) tolower(x))

# Remove the square brackets from the cells of the column 6 of feed
feed[, 6] <- gsub("\\[|\\]", "", feed[, 6])

# Remove all spaces in the last column of feed and convert them all to integers
feed[, 10] <- as.integer(gsub(" ", "", feed[, 10]))

# Print out each unique value and its frequency in the first column of feed
print(table(feed[, 6]))

# Get the number of unique values in each column and print the result
unique_values <- sapply(feed, function(x) length(unique(x)))
print(unique_values)

# Save the feed into a new file called "carfeed.csv"
is_dataframe(feed)
write.csv(feed, file = "carfeed.csv", row.names = FALSE)

# -----------------------------------------------------------
# ------------Do not change anything above this line---------
# -----------------------------------------------------------

# Read the file missing_values.csv into a new data frame called missing_values
no_value <- read.csv("car_missing_values.csv", header = FALSE, sep = ",")
print(no_value)
print(class(no_value))

# Remove spaces in missing_values and convert the result to data frame
no_value <- as.data.frame(sapply(no_value, function(x) gsub(" ", "", x)))
print(no_value)
print(class(no_value))

# Convert the missing_values to a dataframe with one column and print the result
no_value <- as.data.frame(no_value)
print(no_value)
print(class(no_value))

# Print the dimensions of the missing_values
print(dim(no_value))

# Add the values in missing_values to the feed in column 1 from row 140 onwards
feed[140:187, 1] <- no_value[1:48, 1]

# Convert the first column of feed to integer
feed[, 1] <- as.integer(feed[, 1])

# Save the feed into "carfeed.csv"
is_dataframe(feed)
write.csv(feed, file = "carfeed.csv", row.names = FALSE)

# --Appendix-------------------------------------------------

# Make a list of all the counties in Sweden
counties <- c("blekinge", "dalarna", "gotland", "gävleborg", "halland"
              , "jämtland", "jönköping", "kalmar", "kronoberg", "norrbotten"
              , "skåne", "stockholm", "södermanland", "uppsala", "värmland"
              , "västerbotten", "västernorrland", "västmanland"
              , "västra götaland", "örebro", "östergötland")
print(length(counties))

# Ensure the Län column is of character type
feed$Län <- as.character(feed$Län)

# Trim whitespace from the Län column
feed$Län <- trimws(feed$Län)

# Get all the unique values in the column Län of feed with their frequency
unique_values <- table(feed$Län)
print(unique_values)

# Write all the changes as dictionary
all_changes <- c(
  "borås" = "västra götaland",
  "falun" = "dalarna",
  "göteborg" = "västra götaland",
  "hörby" = "skåne",
  "järfälla" = "stockholm",
  "karlskrona" = "blekinge", 
  "karlstad" = "värmland",
  "knivsta" = "uppsala",
  "kristianstad" = "skåne",
  "kungälv" = "västra götaland",
  "lidköping" = "västra götaland",
  "linköping" = "östergötland",
  "llinköping" = "östergötland",
  "löstergötland" = "östergötland",
  "lund" = "skåne",
  "malmö" = "skåne",
  "nordvästra södermanland" = "södermanland",
  "norrköping" = "östergötland",
  "nyköping" = "södermanland",
  "nässjö" = "jönköping",
  "skaraborg" = "västra götaland",
  "skövde" = "västra götaland",
  "stcokholm" = "stockholm",
  "tranemo" = "västra götaland",
  "trollhättan" = "västra götaland",
  "vetlanda" = "jönköping",
  "vimmerby" = "kalmar",
  "värnamo" = "jönköping",
  "västervik" = "kalmar",
  "västerås" = "västmanland",
  "växjö" = "kronoberg",
  "älvsborg" = "västra götaland",
  "örnsköldsvik" = "västernorrland",
  "österåker" = "stockholm")

for (old_county in names(all_changes)) {
  new_county <- all_changes[[old_county]]
  feed$Län <- gsub(old_county, new_county, feed$Län, ignore.case = TRUE)
}

# Check that all the elements in 'Län' can be found in 'counties'
print(all(feed$Län %in% counties))

# Show those elements in 'Län' that are not in 'counties'
print(setdiff(unique(feed$Län), counties))

# Save the feed into a new file called "carfeed.csv"
is_dataframe(feed)
write.csv(feed, file = "carfeed.csv", row.names = FALSE)

# -----------------------------------------------------------
# ------------Do not change anything above this line---------
# -----------------------------------------------------------

# Strip the columns in feed_characters of leading and trailing whitespaces
#feed2 <- sapply(feed2, function(x) trimws(x))
feed <- as.data.frame(lapply(feed, function(x) trimws(as.character(x))))
is_dataframe(feed)

# Show the unique values in the column 1 of feed_characters with its frequency
# String-values
print(table(feed[, "Biltyp"])) # Biltyp
print(table(feed[, "Färg"])) # Färg
print(table(feed[, "Märke"])) # Märke
print(table(feed[, "Modell"])) # Modell             <- Similarity
print(table(feed[, "Län"])) # Län
# Numerical values
print(table(feed[, "Miltal"])) # Miltal             
print(table(feed[, "Modellår"])) # Modellår
print(table(feed[, "Drivning"])) # Drivning
print(table(feed[, "Hästkrafter"])) # Hästkrafter
print(table(feed[, "Pris..Y."])) # Pris..Y.

# -----------------------------------------------------------
# ------------Do not change anything above this line---------
# -----------------------------------------------------------

# Model has too many values that are the same, and many of them are just model
# names, having no actual numerical value. In other words, Modell is actually
# could be just a categorical and not an ordinal variable. If we had more 
# infomation about how this variable relates to ordinal qualities, like car 
# size, horse power, cylinder size, Biltyp, and other ordinal features, or 
# categorical such as Drivmedel. We may thus need to engineer this feature, by 
# combining it with another one, such as märke or biltyp to create a new feature
# So we decided to use Märke together with Modell

is_dataframe(feed)
print(mode(table(feed$Hästkrafter[1])))

# Create a new column called Märkell in the data frame feed that combines 
# the two columns Märke and Modell using an underscore
feed$Märkell <- paste(feed$Märke, feed$Modell, sep = "_")

# Print the unique values in the column Märkell
is_dataframe(feed)
print(head(feed))
print(table(feed$Märkell))

# Remove the two columns Märke and Modell from feed
feed <- feed[, !(names(feed) %in% c("Märke", "Modell"))]
print(head(feed))
print(table(feed$Märkell))

# Make a copy of the feed
is_dataframe(feed)
write.csv(feed, file = "carfeed_cleaned.csv", row.names = FALSE)

# -----------------------------------------------------------
# ------------Do not change anything above this line---------
# -----------------------------------------------------------

feed <- read.csv("carfeed_cleaned.csv", header = TRUE, sep = ",")
is_dataframe(data)

# Create a dummy encoding of 'column' in 'data'
dummy_encoding <- function(data, column) {

    # Ensure the column is a factor
  data[[column]] <- as.factor(data[[column]])

  dummy_column <- model.matrix(~ . - 1, data = data[, column, drop = FALSE])
  colnames(dummy_column) <- paste(column, levels(data[[column]]), sep = "_")
  data <- data[, !(names(data) %in% column)]
  last_col_position <- which(names(data) == "Pris..Y.")
  data <- cbind(data[, 1:(last_col_position - 1)], dummy_column, data[, last_col_position:ncol(data)])
  write.csv(data, file = paste("carfeed_", column, "_dummies.csv", sep = ""), row.names = FALSE)
  
  return(data)
}

feed <- dummy_encoding(feed, 'Biltyp')
print(head(feed))

feed <- dummy_encoding(feed, 'Drivning')
print(head(feed))

feed <- dummy_encoding(feed, 'Färg')
print(head(feed))

feed <- dummy_encoding(feed, 'Län')
print(head(feed))

feed <- dummy_encoding(feed, 'Märkell')
print(head(feed))

# What I learned much later, at about this point, was that if you do not use
# row.names = FALSE, then there will be an X column that you won't be able to
# remove by methods like this one:
# Remove the first column of feed
# feed <- feed[, -1]

# Set the name of the target to Y for clarity
colnames(feed)[ncol(feed)] <- "Y"
print(head(feed))

print(paste("Size of the feed is now ", deparse(dim(feed))))

# Make a copy of the feed
is_dataframe(feed)
write.csv(feed, file = "carfeed_ready.csv", row.names = FALSE)

# -----------------------------------------------------------
# ------------Do not change anything above this line---------
# -----------------------------------------------------------

# Uncomment only if you'd need to reinstall the packages
# install.packages("glmnet")
# install.packages("caret")
# install.packages("e1071")

library(glmnet)
library(caret)

# -----------------------------------------------------------
# --------------------Data Preparation-----------------------
# -----------------------------------------------------------

# Load the data from 
feed <- read.csv("carfeed_ready.csv", header = TRUE, sep = ",")
print(head(feed))

# Split the data into features and target
X <- feed[, -which(names(feed) == "Y")]
y <- feed$Y

# Standardize the features
X_scaled <- scale(X)

# Split into training and test sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X_scaled[trainIndex, ]
X_test <- X_scaled[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# -----------------------------------------------------------
# --------------------Ridge Regression-----------------------
# -----------------------------------------------------------

ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)
print(ridge_model$cvm)
best_lambda_ridge <- ridge_model$lambda.min
print(best_lambda_ridge)

# Fit the model with the best lambda
ridge_fit <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda_ridge)

# Evaluate on the test set
ridge_pred <- predict(ridge_fit, s = best_lambda_ridge, newx = X_test)
ridge_rmse <- sqrt(mean((ridge_pred - y_test)^2))
print(paste("Ridge Regression Test RMSE:", ridge_rmse))

# -----------------------------------------------------------
# --------------------Lasso Regression-----------------------
# -----------------------------------------------------------

lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
print(lasso_model$cvm)
best_lambda_lasso <- lasso_model$lambda.min
print(best_lambda_lasso)

# Fit the model with the best lambda
lasso_fit <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda_lasso)

# Evaluate on the test set
lasso_pred <- predict(lasso_fit, s = best_lambda_lasso, newx = X_test)
lasso_rmse <- sqrt(mean((lasso_pred - y_test)^2))
print(paste("Lasso Regression Test RMSE:", lasso_rmse))

# -----------------------------------------------------------
# -----------------Elastic Net Regression--------------------
# -----------------------------------------------------------

set.seed(42)
control <- trainControl(method = "cv", number = 5)
grid <- expand.grid(alpha = seq(0, 1, length = 10), lambda = 10^seq(-3, 3, length = 100))

elastic_net_model <- train(X_train, y_train,
                           method = "glmnet",
                           trControl = control,
                           tuneGrid = grid)

# Best model parameters
best_alpha <- elastic_net_model$bestTune$alpha
best_lambda <- elastic_net_model$bestTune$lambda
print(paste("Best Alpha:", best_alpha))
print(paste("Best Lambda:", best_lambda))

# Evaluate on the test set
elastic_net_pred <- predict(elastic_net_model, X_test)
elastic_net_rmse <- sqrt(mean((elastic_net_pred - y_test)^2))
print(paste("Elastic Net Regression Test RMSE:", elastic_net_rmse))

# -----------------------------------------------------------
# -------------------------Outcome---------------------------
# -----------------------------------------------------------

# Model coefficients

coef(ridge_fit)

# Predictions on the training set
train_pred <- predict(ridge_fit, s = best_lambda_ridge, newx = X_train)
train_rmse <- sqrt(mean((train_pred - y_train)^2))
print(paste("Ridge Regression Train RMSE:", train_rmse))

# Compare with test RMSE
print(paste("Ridge Regression Test RMSE:", ridge_rmse))

library(ggplot2)

print(length(y_test))      # Length of actual values
print(length(ridge_pred))  # Length of predicted values
ridge_pred <- as.vector(ridge_pred)

# Combine actual and predicted values into a dataframe
results <- data.frame(Actual = y_test, Predicted = ridge_pred)
print(head(results))  # Check the first few rows of the dataframe

# Some basic statistics for the sake of comparison
basic_stats <- summary(feed$Y)

# Extracting the specific statistics from the summary output
min <- basic_stats[1]
Q1 <- basic_stats[2]
med <- basic_stats[3]
mean <- basic_stats[4]
Q3 <- basic_stats[5]
max <- basic_stats[6]

# Plot
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Ridge Regression: Actual vs Predicted",
       x = "Actual Values",
       y = "Predicted Values") +
# If we remove this line, the original plot with the outlier will be displayed
  ylim(c(-1250000, +1250000)) +
  theme_minimal()


# Plot the Actual vs Predicted values
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  # Adding text annotations for the summary statistics
  geom_text(x = min, y = min, label = paste("Min"), color = "green") +
  geom_text(x = 0.95*max, y = max, label = paste("Max"), color = "green") +
  geom_segment(aes(x = Q1, y = min, xend = Q1, yend = Q3), color = "blue") +
  geom_segment(aes(x = Q3, y = min, xend = Q3, yend = Q3), color = "blue") +
  geom_text(x = Q1, y = 0.5*min, label = paste("Q1:", Q1), color = "blue") +
  geom_text(x = Q3, y = 0.5*min, label = paste("Q3:", Q3), color = "blue") +
  geom_vline(xintercept = mean, linetype = "dashed", label = paste("Mean:"), color = "orange") +
  geom_vline(xintercept = med, linetype = "dashed", label = paste("Median:"), color = "orange") +
  geom_text(aes(x = 1.2*mean, y = min + 4*(max - min)/5), label = paste("Mean:", mean), color = "orange") +
  geom_text(aes(x = 0.85*med, y = min + (max - min)/20), label = paste("Median:", med), color = "orange") +
  labs(title = "Ridge Regression: Actual vs Predicted with Summary Statistics",
       x = "Actual Values",
       y = "Predicted Values") +
  # If we remove this line, the original plot with the outlier will be displayed
  ylim(c(-1250000, +1250000)) +
  theme_minimal()

# --Appendix-------------------------------------------------
# -----------------------------------------------------------
# ------------This part can be used only if necessary--------
# -----------------------------------------------------------

# Find the value "stcokholm", print its row number and correct it in feed
print(which(feed$Län == "stcokholm"))
feed[feed$Län == "stcokholm", "Län"] <- "stockholm"

# Get all the unique values in the column Län of feed
unique_values <- unique(feed$Län)
print(unique_values)

# Write a function that takes two strings, city and county and replaces 
# instances of city in the column Län with county
replace_city <- function(city, county) {
  feed$Län <<- gsub(city, county, feed$Län, ignore.case = TRUE)
}

# Implement a function that takes the column Län and for each unique value,
# if that value is in the county list, does nothing, otherwise prompts
# the user to enter the correct county, and then replaces the value in 
# the column Län with the correct county for that value
correct_counties <- function() {
  for (county in unique(feed$Län)) {
    if (!(county %in% counties)) {
      repeat {
        print(paste("The county", county, "is not in the list of counties"))
        new_county <- readline(prompt = "Enter a new county: ")
        if (new_county %in% counties) {
          replace_city(county, new_county)
          break
        }
      }
    }
  }
}
correct_counties() # Use for manual correction

# -----------------------------------------------------------
# ------------Code to replace a column with dummies----------
# -----------------------------------------------------------

# Create a dummy encoding of Biltyp in feed and add the dummy columns
dummy_biltyp <- model.matrix(~ Biltyp - 1, data = feed)
print(head(dummy_biltyp))

# Add the dummy columns to feed
feed <- cbind(feed, dummy_biltyp)
print(head(feed))

# Remove the Biltyp column from feed
feed <- feed[, !(names(feed) %in% "Biltyp")]
print(head(feed))

# Make a copy of the feed
is_dataframe(feed)
write.csv(feed, file = "carfeed_biltyp_dummies.csv", row.names = FALSE)

# -----------------------------------------------------------
# -----To reset all the variables, except feed and feed1-----
# -----------------------------------------------------------

ls()
rm(list = ls()[ls() != "feed" & ls() != "feed_1"])
ls()

# -----------------------------------------------------------
# -----To reset all the variables, use this code snippet-----
# -----------------------------------------------------------

ls()
rm(list = ls())
ls()
