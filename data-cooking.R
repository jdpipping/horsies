# load tidyverse, data ####
library(tidyverse)

partial_horse_data <- read_csv('processed-horse-data/partially_cleaned_data.csv')

# standardizing velocity, acceleration within races ####
std_horse_data <- partial_horse_data %>%
  group_by(race_id) %>%
  mutate(std_speed <- as.vector(scale(speed)),
         std_acceleration <- as.vector(scale(acceleration)))
write_csv(std_horse_data, 'processed-horse-data/standardized_partial_data.csv')
