# load tidyverse, partially cleaned data ####
library(tidyverse)

partial_horse_data <- read_csv('processed-horse-data/partially_cleaned_data.csv')


# calculate lateral movement between frames ####

# IN PROGRESS

# calculate pairwise distances between horses ####

# IN PROGRESS

# standardizing velocity, acceleration within races ####
std_horse_data <- partial_horse_data %>%
  group_by(race_id, frame_id) %>%
  mutate(std_speed = as.vector(scale(speed)),
         std_acceleration = as.vector(scale(acceleration))) %>%
  ungroup()

# write csv
write_csv(std_horse_data, 'processed-horse-data/final_cooked_data.csv')