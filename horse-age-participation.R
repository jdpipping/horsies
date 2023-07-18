# load tidyverse, horse names, participation data ####
library(tidyverse)

horse_names <- read_csv('initial-horse-data/horse_names.csv')
participation_data <- read_csv('initial-horse-data/horses_3years_racing_NY.csv') |>
  drop_na(horse_name)

# append horse names to participation data ####
id_participation_data <- participation_data |>
  left_join(horse_names, by = 'horse_name')

# add age at race date ####
library(lubridate)

age_participation_data <- id_participation_data |>
  mutate(
    age = time_length(difftime(race_date, birth_date), "years")
  )