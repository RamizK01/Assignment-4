### Coding in R - DHT ###
### Ramiz Khan        ###
### Assignment 4      ###

# loading any necessary packages (please install if you do not have already with install.packages)
library(dplyr)
library(lubridate)
library(knitr)

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
# ifelse() can be used to check the comments column for the keyword, hoax while assigning it to the new variable
# Hoax comments were written in three different ways by NUFORC, using grepl() we can check for all three ways 
# and assign a TRUE boolean if the word Hoax was written in the comments

df$is_hoax <- ifelse(grepl("Hoax|HOAX|hoax", df$comments), TRUE, FALSE)


hoax_sightings_per_country <- df %>%
  group_by(country) %>%
  summarise(percentage_of_hoax_sightings_per_country = (sum(is_hoax == TRUE)/n())*100) 

# Found this cool package called knitr which has a table() function called kable()
# not part of tidyverse, more of an html style formatting to create tables and visualizations

kable(hoax_sightings_per_country, 
      digits = 2,
      caption = "Values are in percentile, %, format\n
      --LEGEND--\n au = Australia\n ca = Canada\n de = Denmark\n gb = Great Britain\n us = United States of America")

df$report_delay <- as.numeric(df$date_posted - df$datetime) / (60*60*24)  


# While numeric, removing any observations with a negative report_delay value, meaning it was reported before it happened

df <- df[df$report_delay > 0, ]

df$report_delay <- paste0(floor(df$report_delay), " days")  


          
View(df)  

