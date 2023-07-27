# load tidyverse, partially cleaned data ####
library(tidyverse)

partial_horse_data <- read_csv('processed-horse-data/partially_cleaned_data.csv')


# standardize velocity, acceleration within races ####
std_horse_data <- partial_horse_data |>
  group_by(race_id, frame_id) |>
  mutate(std_speed = as.vector(scale(speed)),
         std_acceleration = as.vector(scale(acceleration))) |>
  ungroup()

# write csv
write_csv(std_horse_data, 'processed-horse-data/std_horse_data.csv')

# calculate pairwise distances between horses ####

# create race-frame id
race_frame_data_all <- std_horse_data |>
  mutate(
    race_frame_id = paste(race_id, frame_id, sep = '_')
  ) |>
  arrange(race_frame_id, finishing_place)

# gather all race ids
all_races <- unique(race_frame_data_all$race_id)

# cut race data, frames into four groups
race_frame_data_1 <- race_frame_data_all |> 
  filter(race_id %in% all_races[1:498])
race_frames_1 <- unique(race_frame_data_1$race_frame_id)

race_frame_data_2 <- race_frame_data_all |> 
  filter(race_id %in% all_races[499:996])
race_frames_2 <- unique(race_frame_data_2$race_frame_id)

race_frame_data_3 <- race_frame_data_all |> 
  filter(race_id %in% all_races[997:1494])
race_frames_3 <- unique(race_frame_data_3$race_frame_id)

race_frame_data_4 <- race_frame_data_all |> 
  filter(race_id %in% all_races[1495:1991])
race_frames_4 <- unique(race_frame_data_4$race_frame_id)

# pairwise distance functions
pairwise_distances_1 <- function(race_frames) {

  # generate pairwise euclidean distances
  distances <- race_frame_data_1 |> 
    filter(race_frame_id == race_frames) |>
    select(x, y) |> 
    dist(diag = TRUE, upper = TRUE) |>
    as.matrix() |>
    as_tibble() |> 
    mutate(race_frame_id = race_frames)
  
  return(distances)
  
}
pairwise_distances_2 <- function(race_frames) {
  
  # generate pairwise euclidean distances
  distances <- race_frame_data_2 |> 
    filter(race_frame_id == race_frames) |>
    select(x, y) |> 
    dist(diag = TRUE, upper = TRUE) |>
    as.matrix() |>
    as_tibble() |> 
    mutate(race_frame_id = race_frames)
  
  return(distances)
  
}
pairwise_distances_3 <- function(race_frames) {
  
  # generate pairwise euclidean distances
  distances <- race_frame_data_3 |> 
    filter(race_frame_id == race_frames) |>
    select(x, y) |> 
    dist(diag = TRUE, upper = TRUE) |>
    as.matrix() |>
    as_tibble() |> 
    mutate(race_frame_id = race_frames)
  
  return(distances)
  
}
pairwise_distances_4 <- function(race_frames) {
  
  # generate pairwise euclidean distances
  distances <- race_frame_data_4 |> 
    filter(race_frame_id == race_frames) |>
    select(x, y) |> 
    dist(diag = TRUE, upper = TRUE) |>
    as.matrix() |>
    as_tibble() |> 
    mutate(race_frame_id = race_frames)
  
  return(distances)
  
}

# run euclidean distance parallelization for each fold
library(furrr)
library(tictoc)

plan(multisession, workers = 7)

# fold 1
tic()
euclidean_horse_data_1 <- future_map_dfr(race_frames_1, pairwise_distances_1, .progress = TRUE)
toc()

# fold 2
tic()
euclidean_horse_data_2 <- future_map_dfr(race_frames_2, pairwise_distances_2, .progress = TRUE)
toc()

# fold 3
tic()
euclidean_horse_data_3 <- future_map_dfr(race_frames_3, pairwise_distances_3, .progress = TRUE)
toc()

# fold 4
tic()
euclidean_horse_data_4 <- future_map_dfr(race_frames_4, pairwise_distances_4, .progress = TRUE)
toc()

# bind rows together
euclidean_horse_data <- bind_rows(euclidean_horse_data_1, euclidean_horse_data_2, euclidean_horse_data_3, euclidean_horse_data_4)
  
# name columns appropriately
colnames(euclidean_horse_data) <- c('euclidean_1', 'euclidean_2', 'euclidean_3', 'euclidean_4', 'euclidean_5', 'race_frame_id_2', 'euclidean_6', 'euclidean_7', 'euclidean_8', 'euclidean_9', 'euclidean_10', 'euclidean_11', 'euclidean_12', 'euclidean_13', 'euclidean_14')

# bind euclidean distances to original data
final_euclidean_data <- race_frame_data_all |> 
  bind_cols(euclidean_horse_data) |> 
  select(-race_frame_id_2)

# write csv
write_csv(final_euclidean_data, 'processed-horse-data/final_euclidean_data.csv')

# calculate pairwise approach velocities ####

# calculate frame velocities
horse_velocities <- final_euclidean_data %>%
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
    velocity_12 = (euclidean_12 - lag(euclidean_12)) / 0.25,
    velocity_13 = (euclidean_13 - lag(euclidean_13)) / 0.25,
    velocity_14 = (euclidean_14 - lag(euclidean_14)) / 0.25
  ) %>%
  ungroup()

# calculate pairwise strain rate ####

# strain function
strain <- function(distance, velocity) {
  strain = -velocity / distance
  return(strain)
}

# calculate pairwise strains
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
    strain_12 = strain(euclidean_12, velocity_12),
    strain_13 = strain(euclidean_13, velocity_13),
    strain_14 = strain(euclidean_14, velocity_14)
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
write_csv(final_horse_strains, 'processed-horse-data/final_horse_strains.csv')

# calculate lateral movement between frames (IN PROGRESS) ####