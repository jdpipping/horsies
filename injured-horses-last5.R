# # load tidyverse, standardized partial data, horse profiles ####
# library(tidyverse)
# 
# std_horse_data <- read_csv('processed-horse-data/standardized_partial_data.csv')
# horse_profiles <- read_csv('outputted-csvs/horse_profiles_all.csv')

# cleaning injury tracking data ####

# identify injured horses
injured_horses <- horse_profiles %>%
  filter(if_injury_reported) %>%
  select(-if_injury_reported)

# join tracking, injury data
injured_horses_tracking <- inner_join(std_horse_data, injured_horses,
                                      by = 'horse_id',
                                      suffix = c('', '.injury'),
                                      relationship = 'many-to-many') %>%
  select(-ends_with(".injury"))

# retrieve data from previous 5 races before injury for each horse ####

# create tibble of last 5 races
injured_last5  <- tibble()

# for loop retrieving last 5 races before injury for each horse
for (horse in unique(injured_horses_tracking$horse_id)) {
  
  # retrieve horse details
  horse_details <- injured_horses_tracking %>%
    filter(horse_id == horse)
  
  # retrieve participation dates
  horse_dates <- horse_details %>%
    select(race_date) %>%
    unique() %>%
    unlist() %>%
    sort()
  
  # retrieve date of injury
  injury_date <- horse_details %>%
    select(injury_date) %>%
    unique() %>%
    unlist()
  
  # subset, count races run pre-injury
  pre_injury <- horse_dates[injury_date >= horse_dates]
  race_count <- length(pre_injury)
  
  # get dates of last 5 races run
  if (race_count == 0) {
    next
  }
  else if (race_count < 5) {
    dates_run <- as.Date(pre_injury, origin = "1970-01-01")
  }
  else {
    dates_run <- as.Date(pre_injury[(race_count-4):race_count], origin = "1970-01-01")
  }
  
  # gather horse data for last 5 races
  last5_dates <- horse_details %>%
    filter(race_date %in% dates_run)
  
  # bind data to tibble of last 5 races
  injured_last5 <- bind_rows(injured_last5, last5_dates)
}

# write csv
write_csv(injured_last5, 'processed-horse-data/injured_last5_full.csv')