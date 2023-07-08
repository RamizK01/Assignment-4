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

df$report_delay_days <- ceiling(as.numeric(df$date_posted - df$datetime) / (60*60*24))  


# While numeric, removing any observations with a negative report_delay value, meaning it was reported before it happened

df <- df[df$report_delay_days >= 0, ]
    
report_delay_per_country <- df %>%
  filter(!is.na(report_delay_days)) %>%
  group_by(country) %>%
  summarise(average_report_delay = mean(report_delay_days))

### Issues with durations.seconds column ###
# duration.hours.min is a character class while duration.seconds is a numeric class
# class conversion can be annoying when dealing with operations on values
# when durations.hours.min is estimated (ex. 10-15 mins), duration.seconds is listed to the upper limit of the estimate (ex. 15 mins = 900 seconds)
  # This is a weird one obs(25650) has listed 45 seconds - 2 hours, and duration.seconds decided to make it 7200 (2 hours in seconds) + 45 seconds to be 7245 seconds
# when durations.hours.mins uses operators such as ~ > <, durations.seconds just takes the numeric value attached to the operator
# "seconds" is automatically to be interpreted as 2.00 seconds
# words such as several, split, multiple, no more than, do not work well when converted into seconds
# approx and about keywords are fine and work well in converting to seconds

# Dealing with durations.seconds

# removing durations.seconds values for durations.hours.min values that have words, "several", "split", "multiple", "no more than", "or more", "still going"
# removing duration.seconds values for durations.hours.min values that are ranges (ex. 10-15 mins) by filtering for "-" or "to"
# removing duration.seconds values for durations.hours.min values that are "days", "evening", "week", "morning", "month"
# single observation has a typo, 23000 instead of 2300, removed that string too just to avoid heavy outliers


# Set values as NA based on the presence of multiple strings in another column
exclusion_words <- c("several", "split", "multiple", "no more than", "or more", "still going", "-", "to", "days", "evening", "week", "morning", "month", "23000")

df <- df %>%
  mutate(duration.seconds = ifelse(grepl(paste(exclusion_words, collapse = "|"), tolower(duration.hours.min)), NA, duration.seconds))

      
View(report_delay_per_country)
View(df)  

