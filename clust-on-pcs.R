# data prep ####
library(tidyverse)
library(factoextra)
library(broom)
library(mclust)

# file path may vary | this file was downloaded from the google drive
file_path <- file.choose()
## my file path is below:
#file_path <- "C:/Users/krisa/Contacts/Downloads/final_horse_strains.csv"
final_horse_strains <- read_csv(file = file_path)
# take only the variables we need
final_horse_data <- final_horse_strains |> 
  select(-c("x_proj", "y_proj", "raw_x", "raw_y", 
            # remove helper variables
            matches(c("euclid", "velocity")), starts_with("strain")))
# compute summary statistics by horse
horse_summary_stats <- final_horse_data  |> 
  # remove horse with missing data and values with no data for computations
  filter(horse_id != 3563, is.finite(avg_strain) & is.finite(total_strain)) |>
  # want summary stats for horses
  group_by(horse_id, horse_name) |> 
  summarise(mean_speed = mean(speed),
            mean_acceleration = mean(acceleration),
            median_speed = median(speed),
            median_acceleration = median(acceleration),
            min_speed = min(speed),
            max_speed = max(speed),
            range_speed = max(speed) - min(speed),
            min_acceleration = min(acceleration),
            max_acceleration = max(acceleration),
            range_acceleration = max(acceleration) - min(acceleration),
            sd_speed = sd(speed),
            sd_acceleration = sd(acceleration),
            iqr_speed = IQR(speed),
            iqr_acceleration = IQR(acceleration),
            cv_speed = (sd(speed) / mean(speed)),
            mean_avg_strain = mean(avg_strain),
            mean_total_strain = mean(total_strain),
            median_avg_strain = median(avg_strain),
            median_total_strain = median(total_strain),
            min_avg_strain = min(avg_strain),
            max_avg_strain = max(avg_strain),
            range_avg_strain = max(avg_strain) - min(avg_strain),
            min_total_strain = min(total_strain),
            max_total_strain = max(total_strain),
            range_total_strain = max(total_strain) - min(total_strain),
            sd_avg_strain = sd(avg_strain),
            sd_total_strain = sd(total_strain),
            iqr_avg_strain = IQR(avg_strain),
            # need data.frame() to remove grouping variable
            iqr_total_strain = IQR(total_strain))

# save the data!
### saveRDS(horse_summary_stats, "C:/Users/krisa/horse_summary_stats.csv")

# lateral movement detour ####
# read in data
tracking_data_cleaned_1600 <- read_csv("C:/Users/krisa/tracking_data_cleaned_1600.csv")
# select variables we neeed
horse_movement <- tracking_data_cleaned_1600 |> 
  filter(is.finite(side_movement)) |> 
  select(race_id, track_id, race_date, frame_id, horse_id, horse_name, side_movement)

# summary statistics
lateral_movement <- horse_movement |> 
  group_by(horse_id, horse_name) |> 
  summarize(mean_side_movement = mean(side_movement),
            median_side_movement = median(side_movement),
            min_side_movement = min(side_movement),
            max_side_movement = max(side_movement),
            range_side_movement = max(side_movement) - min(side_movement),
            sd_side_movement = sd(side_movement),
            iqr_side_movement = IQR(side_movement))

# pca time! ####
## extract matrices for each feature | convert to matrix for prcomp()
horsies_pca <- horse_summary_stats |> 
  data.frame() |> select(-c(horse_id, horse_name))

# speed stats
speed_stats <- horsies_pca |> 
  select(matches("speed")) |> as.matrix()
# accel stats
acceleration_stats <- horsies_pca |> 
  select(matches("acceleration")) |> as.matrix()
# strain stats
strain_stats <- horsies_pca |> 
  select(matches("strain")) |> as.matrix()
# lat mvmt stats
side_mvmt_pca <- lateral_movement |> 
  data.frame() |> select(-c(horse_id, horse_name)) |> as.matrix()

# strain rate pca
pca_strain <- prcomp(strain_stats, center = TRUE, scale. = TRUE)
summary(pca_strain)

# speed pca
pca_speed <- prcomp(speed_stats, center = TRUE, scale. = TRUE)
summary(pca_speed)

# acceleration pca
pca_acceleration <- prcomp(acceleration_stats, 
                           center = TRUE, scale. = TRUE)
summary(pca_acceleration)

# lat mvmt pca
pca_lat_mvmt <- prcomp(side_mvmt_pca, center = TRUE, scale. = TRUE)

# subset each just for relevant principal components
speed_pcs <- pca_speed$x |> data.frame() |> select(1:4)
accel_pcs <- pca_acceleration$x |> data.frame() |> select(1:4)
lat_mvmt_pcs <- pca_lat_mvmt$x |> data.frame() |> select(1:4)
strain_rate_pcs <- pca_strain$x |> data.frame() |> select(1:5)
## needs 5 PCs to explain >= 90% variation

# clustering on PCs directly ####
set.seed(27072023) # same seed as summer
strain_pcs_mclust <- Mclust(strain_rate_pcs)
speed_pcs_mclust <- Mclust(speed_pcs)
acceleration_pcs_mclust <- Mclust(accel_pcs)
lat_mvmt_pcs_mclust <- Mclust(lat_mvmt_pcs)

# join the data--need to verify it joined right since technically 
# not joining by unique key (e.g., name/id)

# also joined to full data, NOT data w PCs
horse_summary_stats <- cbind(horse_summary_stats,
      "strain_cluster" = strain_pcs_mclust$classification)
horse_summary_stats <- cbind(horse_summary_stats,
      "speed_cluster" = speed_pcs_mclust$classification)
horse_summary_stats <- cbind(horse_summary_stats,
      "acceleration_cluster" = acceleration_pcs_mclust$classification)
lateral_movement <- cbind(lateral_movement,
      "lat_mvmt_cluster" = lat_mvmt_pcs_mclust$classification)

# remove summary stats, keep cluster assignments only
horse_summary_stats <- horse_summary_stats |> 
  select(horse_id, horse_name, contains("cluster"))
lateral_movement <- lateral_movement |> 
  select(horse_id, horse_name, contains("cluster"))
# save results
write.csv(lateral_movement, "C:/horsies/clustering-on-pcs-results/lateral-movement-clust-on-pcs-results.csv")
write.csv(horse_summary_stats, "C:/horsies/clustering-on-pcs-results/horse-clust-on-pcs-results.csv")