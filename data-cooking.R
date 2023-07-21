# load tidyverse, partially cleaned data ####
library(tidyverse)

partial_horse_data <- read_csv('processed-horse-data/partially_cleaned_data.csv')


# standardizing velocity, acceleration within races ####
std_horse_data <- partial_horse_data |>
  group_by(race_id, frame_id) |>
  mutate(std_speed = as.vector(scale(speed)),
         std_acceleration = as.vector(scale(acceleration))) |>
  ungroup()

# write csv
write_csv(std_horse_data, 'processed-horse-data/final_cooked_data.csv')

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

final_cooked_data <- race_frame_data_all |> 
  bind_cols(euclidean_horse_data)
# write csv
write_csv(final_cooked_data, 'processed-horse-data/final_cooked_data.csv')

# calculate lateral movement between frames (IN PROGRESS) ####