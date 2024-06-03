# import csv file into a for R suitable datastructure
install.packages("jsonlite")
install.packages('jsonlite', repos='http://cran.us.r-project.org')
# which R gives /usr/local/bin/R
# defaults write org.R-project.R r.rpath.mac /usr/local/bin/R
# The downloaded binary packages are in
# /var/folders/1s/vk0n_73d4ndcz49qcnxf6wf40000gp/T//RtmpBPa30s/downloaded_packages

# Load the data
data <- read.csv("2404111718_Cars.csv", header = TRUE, sep = ",")

# Remove everything in columns 11 and onwards
feed <- data[,1:10]

# Print the feed
print(feed)

# Get the number of empty cells in each column and print the result
empty_cells <- sapply(feed, function(x) sum(is.na(x)))
print(empty_cells)

# Get the type of each column and print the result
column_types <- sapply(feed, class)
print(column_types)

# Convert all columns of the type character to all small letters
#feed <- sapply(feed, function(x) if(is.character(x)) tolower(x) else x)

# Print the feed
# print(feed)

# Separate the columns of the type integer as a new data frame called all_integers
#all_integers <- feed[, sapply(feed, is.integer)]
#print(all_integers)

# Print the dimensions of feed
#print(dim(all_integers))

# For each of the columns 3, 6 - 9 of feed, remove the leading, middle, and trailing whitespaces
feed[, c(3, 6:9)] <- sapply(feed[,c(3, 6:9)], function(x) gsub("^\\s+|\\s+$", "", x))

# Convert all characters in the columns 3, 6 - 9 of feed to small letters
feed[, c(3, 6:9)] <- sapply(feed[, c(3, 6:9)], function(x) tolower(x))

# Remove the square brackets from the cells of the column 6 of feed
feed[, 6] <- gsub("\\[|\\]", "", feed[,6])

# Convert the first column of feed to integers
feed[, 1] <- as.integer(feed[, 1])

# Remove all spaces in the cells of the last column of feed and convert them all to integers
feed[, 10] <- as.integer(gsub(" ", "", feed[, 10]))

# Print out each unique value and its frequency in the first column of feed
print(table(feed[, 6]))

# Get the number of unique values in each column and print the result, each one in a new line
unique_values <- sapply(feed, function(x) length(unique(x)))
print(unique_values)

# Save the feed into a new file called "carfeed.csv"
write.csv(feed, file = "carfeed.csv")

# -----------------------------------------------------------
# ------------Do not change anything above this line---------
# -----------------------------------------------------------

# Read the file missing_values.csv into a new data frame called missing_values
missing_values <- read.csv("car_missing_values.csv", header = FALSE, sep = ",")
print(missing_values)
print(class(missing_values))

# Remove spaces within the cells of the missing_values and convert the result to data frame
missing_values <- as.data.frame(sapply(missing_values, function(x) gsub(" ", "", x)))
print(missing_values)
print(class(missing_values))

# Convert the missing_values to a dataframe with one column and print the result
missing_values <- as.data.frame(missing_values)
print(missing_values)
print(class(missing_values))

# Print the dimensions of the missing_values
print(dim(missing_values))

# Add the values in missing_values to the feed in column 1 from row 140 onwards
feed[140:187, 1] <- missing_values[1:48, 1]

# Convert the first column of feed to integer
feed[, 1] <- as.integer(feed[, 1])

# Save the feed into a new file called "carfeed.csv"
write.csv(feed, file = "carfeed.csv")

# -----------------------------------------------------------
# ------------Do not change anything above this line---------
# -----------------------------------------------------------

# Find the value "stcokholm", print its row number and change its value to "stockholm" the column "Län" of feed
print(which(feed$Län == "stcokholm"))
feed[feed$Län == "stcokholm", "Län"] <- "stockholm"

# Save the feed into a new file called "carfeed.csv"
write.csv(feed, file = "carfeed.csv")

# -----------------------------------------------------------
# ------------Do not change anything above this line---------
# -----------------------------------------------------------

# Gather all the columns of type integer in a data frame called feed_integers
feed_integers <- feed[, sapply(feed, is.integer)]
print(feed_integers)

# Gather all the clumns of type character in a data frame called feed_characters
feed_characters <- feed[, sapply(feed, is.character)]
print(feed_characters)

# Strip the columns in feed_characters of leading and trailing whitespaces without changing the dimensions of the data frame
feed_characters <- sapply(feed_characters, function(x) trimws(x))

# Show the unique values in the column 1 of feed_characters with its frequency
print(table(feed_characters[, 1])) # Biltyp
print(table(feed_characters[, 2])) # Färg
print(table(feed_characters[, 3])) # Märke
print(table(feed_characters[, 4])) # Modell
print(table(feed_characters[, 5])) # Län

# Show the unique values in the column 1 of feed_integers with its frequency
print(table(feed_integers[, 1])) # Miltal
print(table(feed_integers[, 2])) # Modellår
print(table(feed_integers[, 3])) # Drivning
print(table(feed_integers[, 4])) # Hästkrafter
print(table(feed_integers[, 5])) # Pris

# Empty all variables from the workspace
data <- NULL
feed <- NULL
empty_cells <- NULL
column_types <- NULL
all_integers <- NULL
unique_values <- NULL
missing_values <- NULL
feed_integers <- NULL
feed_characters <- NULL

# Read carfeed.csv into a new data frame called carfeed
carfeed <- read.csv("carfeed.csv", header = TRUE, sep = ",")

