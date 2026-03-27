# Load required libraries
library(tidyverse)
# Import dataset
crime <- read_csv("crime2.csv")

# Count number of crimes per year using a for loop
distinct_year <- sort(unique(crime$YEAR))
counter_year <- c() # empty vector that will save counters from each year
for (year in distinct_year){
  counter = 0
  # start
  for(i in 1:nrow(crime)){
    if(crime$YEAR[i] == year){
      counter = counter + 1
    }
  }
  counter_year <- c(counter_year, counter)
  # end
}
summary_table = tibble(YEAR = distinct_year, n = counter_year)
summary_table

# Weekly crime trend before June 15, 2018
crime_filtered <- crime %>%
  filter(OCCURRED_ON_DATE < mdy("06/15/2018"))

crime_weekly <- crime_filtered %>%
  mutate(Weekly_Date = floor_date(OCCURRED_ON_DATE, unit = "week"))%>%
  count(Weekly_Date)

ggplot(crime_weekly, aes(x = Weekly_Date, y = n)) +
  geom_line()

# Function: First and last occurrence of a crime per year
crime_first_last_date <- function(code) {
  if (!(code %in% crime$OFFENSE_CODE_GROUP)) {
    return("Please provide a valid offense code.")
  }

  crime_filtered <- crime %>%
    filter(OFFENSE_CODE_GROUP == code)
  
  crime_filtered$OCCURRED_ON_DATE <- 
    as.Date(crime_filtered$OCCURRED_ON_DATE)
  
  crime_filtered <- crime_filtered %>%
    arrange(OCCURRED_ON_DATE)
  
  summary_table <- crime_filtered %>%
    group_by(YEAR) %>%
    summarise(
      first_date = first(OCCURRED_ON_DATE),
      last_date  = last(OCCURRED_ON_DATE)
    )
  
  return(summary_table)
}
crime_first_last_date("Vandalism")
crime_first_last_date("Having no fun in MATH 208")

#  Function: Date when crime count reaches threshold
popular_crimes <- function(num_crimes = 100){
  crime %>%
    arrange(OCCURRED_ON_DATE) %>%
    group_by(OFFENSE_CODE_GROUP) %>%
    summarise(date_time = nth(OCCURRED_ON_DATE, num_crimes)) %>%
    filter(!is.na(date_time)) %>%
    arrange(OFFENSE_CODE_GROUP)
}

# Test function
popular_crimes()
popular_crimes(200)
