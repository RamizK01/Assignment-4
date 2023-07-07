### Coding in R - DHT ###
### Ramiz Khan        ###
### Assignment 4      ###

# loading any necessary packages (please install if you do not have already with install.packages)
library(dplyr)
library(lubridate)

# Reading data into a dataframe and viewing it

df <- read.csv("ufo_subset.csv")

# Adjusting shape column, changing empty strings with "unknown"

df$shape <- replace(df$shape, df$shape == "", "unknown")

# Removing any observations that did not include country 

df <- df[df$country != "", ]

# Converting class of datetime and date_posted columns to POSIXct

df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M")
df$date_posted <- as.POSIXct(df$date_posted, format = "%d-%m-%Y")

# Adding Hoax column

df$Hoax <- ifelse(grepl("Hoax|HOAX|hoax", df$comments), TRUE, FALSE)
