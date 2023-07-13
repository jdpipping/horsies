# load tidyverse, data ####
library(tidyverse)

horse_data <- read_csv("processed-horse-data/tracking_data_cleaned.csv")

# pairwise approach velocities ####

# calculate frame velocities
horse_velocities <- horse_data %>%
  group_by(race_id, horse_id) %>%
  mutate(
    velocity_1 = (euclidean_1 - lag(euclidean_1)) / 0.25,
    velocity_2 = (euclidean_2 - lag(euclidean_2)) / 0.25,
    velocity_3 = (euclidean_3 - lag(euclidean_3)) / 0.25,
    velocity_4 = (euclidean_4 - lag(euclidean_4)) / 0.25,
    velocity_5 = (euclidean_5 - lag(euclidean_5)) / 0.25,
    velocity_6 = (euclidean_6 - lag(euclidean_6)) / 0.25,
    velocity_7 = (euclidean_7 - lag(euclidean_7)) / 0.25,
    velocity_8 = (euclidean_8 - lag(euclidean_8)) / 0.25,
    velocity_9 = (euclidean_9 - lag(euclidean_9)) / 0.25,
    velocity_10 = (euclidean_10 - lag(euclidean_10)) / 0.25,
    velocity_11 = (euclidean_11 - lag(euclidean_11)) / 0.25,
    velocity_12 = (euclidean_12 - lag(euclidean_12)) / 0.25
  ) %>%
  ungroup()

# strain analysis ####

# strain function
strain <- function(distance, velocity) {
  strain = velocity / distance
  return(strain)
}

# calculate pairwise strain
horse_strains <- horse_velocities %>%
  mutate(
    strain_1 = strain(euclidean_1, velocity_1),
    strain_2 = strain(euclidean_2, velocity_2),
    strain_3 = strain(euclidean_3, velocity_3),
    strain_4 = strain(euclidean_4, velocity_4),
    strain_5 = strain(euclidean_5, velocity_5),
    strain_6 = strain(euclidean_6, velocity_6),
    strain_7 = strain(euclidean_7, velocity_7),
    strain_8 = strain(euclidean_8, velocity_8),
    strain_9 = strain(euclidean_9, velocity_9),
    strain_10 = strain(euclidean_10, velocity_10),
    strain_11 = strain(euclidean_11, velocity_11),
    strain_12 = strain(euclidean_12, velocity_12)
  )

# average strains
avg_strains <- horse_strains %>%
  select(starts_with ('strain')) %>%
  rowMeans(na.rm = TRUE) %>%
  as_tibble_col(column_name = 'avg_strain') %>%
  # add change in average strain
  mutate(
    change_avg_strain = avg_strain - lag(avg_strain)
  )

# total strains
total_strains <- horse_strains %>%
  select(starts_with ('strain')) %>%
  rowSums(na.rm = TRUE) %>%
  as_tibble_col(column_name = 'total_strain') %>%
  # add change in total strain
  mutate(
    change_total_strain = total_strain - lag(total_strain)
  )
  
# final table
final_horse_strains <- horse_strains %>%
  bind_cols(avg_strains, total_strains)

# write csv
write_csv(final_horse_strains, 'horse-data/processed-horse-data/final_horse_strains.csv')