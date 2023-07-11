# load tidyverse, data ####
library(tidyverse)

horse_data <- read_csv("horse-cooking-data/partially_cleaned_data.csv") %>%
  select(race_id, horse_id, frame_id, n_horses, position_at_finish, x, y)
# pairwise euclidean distances ####

# reordered horse data tibble()
reordered_horse_data <- tibble()
# euclidean distance tibble
euclidean_distance <- tibble()

# for each race
for (race in unique(horse_data$race_id)) {
  print(race)
  race_data <- horse_data %>%
    filter(race_id == race) %>%
    arrange(desc(position_at_finish))
  # for each frame_id
  for (frame in unique(race_data$frame_id)) {
    frame_data <- race_data %>%
      filter(frame_id == frame)
    # update reordered_horse_data
    reordered_horse_data <- bind_rows(reordered_horse_data, frame_data)
    # calculate distance matrix, convert to tibble
    distance <- frame_data %>%
      select(x, y) %>%
      dist(diag = T, upper = T) %>%
      as.matrix() %>%
      as_tibble()
    # iterate through rows, bind to euclidean_distance tibble
    for (row in 1:nrow(frame_data)) {
      euclidean_distance <- bind_rows(euclidean_distance, distance[row,])
    }
  }
}

# turn all 0-distances into na's
euclidean_distance[euclidean_distance == 0] = NA

# bind euclidean distance tibble to horse_data
horse_distances <- bind_cols(reordered_horse_data, euclidean_distance) %>%
  arrange(race_id, horse_id, frame_id)
colnames(horse_distances)[8:19] <- paste('dist_', c(1:12), sep = '')
# write csv
write_csv(horse_distances, 'horse-cooking-data/horse_distances.csv')
# pairwise approach velocities ####

# # load in distance data
# horse_distances <- read_csv('horse-cooking-data/horse_distances.csv')

# calculate frame velocities
horse_velocities <- horse_distances %>%
  group_by(race_id, horse_id) %>%
  mutate(
    velo_1 = (dist_1 - lag(dist_1)) / 0.25,
    velo_2 = (dist_2 - lag(dist_2)) / 0.25,
    velo_3 = (dist_3 - lag(dist_3)) / 0.25,
    velo_4 = (dist_4 - lag(dist_4)) / 0.25,
    velo_5 = (dist_5 - lag(dist_5)) / 0.25,
    velo_6 = (dist_6 - lag(dist_6)) / 0.25,
    velo_7 = (dist_7 - lag(dist_7)) / 0.25,
    velo_8 = (dist_8 - lag(dist_8)) / 0.25,
    velo_9 = (dist_9 - lag(dist_9)) / 0.25,
    velo_10 = (dist_10 - lag(dist_10)) / 0.25,
    velo_11 = (dist_11 - lag(dist_11)) / 0.25,
    velo_12 = (dist_12 - lag(dist_12)) / 0.25
  ) %>%
  ungroup()

# write csv
write_csv(horse_velocities, 'horse-cooking-data/horse_velocities.csv')
# strain analysis ####

# # load in velocity data
# horse_velocities <- read_csv('horse-cooking-data/horse_velocities.csv')

# strain function
strain <- function(distance, velocity) {
  strain = velocity / distance
  return(strain)
}

# calculate pairwise strain
horse_strains <- horse_velocities %>%
  mutate(
    strain_1 = strain(dist_1, velo_1),
    strain_2 = strain(dist_2, velo_2),
    strain_3 = strain(dist_3, velo_3),
    strain_4 = strain(dist_4, velo_4),
    strain_5 = strain(dist_5, velo_5),
    strain_6 = strain(dist_6, velo_6),
    strain_7 = strain(dist_7, velo_7),
    strain_8 = strain(dist_8, velo_8),
    strain_9 = strain(dist_9, velo_9),
    strain_10 = strain(dist_10, velo_10),
    strain_11 = strain(dist_11, velo_11),
    strain_12 = strain(dist_12, velo_12)
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
  # remove intermediate columns
  select(-n_horses, -position_at_finish, -x, -y, -starts_with(c('dist', 'velo'))) %>%
  # bind average, total columns
  bind_cols(avg_strains, total_strains)

# write csv
write_csv(final_horse_strains, 'horse-cooking-data/final_horse_strains.csv')