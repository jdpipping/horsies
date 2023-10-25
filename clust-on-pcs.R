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
horsies_no_ids <- horse_summary_stats |> 
  data.frame() |> select(-c(horse_id, horse_name)) |> as.matrix()

# lat mvmt stats
side_mvmt_pca <- lateral_movement |> 
  data.frame() |> select(-c(horse_id, horse_name)) |> as.matrix()

# lat mvmt pca
pca_lat_mvmt <- prcomp(side_mvmt_pca, center = TRUE, scale. = TRUE)

# big pca
horsies_pca <- prcomp(horsies_no_ids, center = TRUE, scale. = TRUE)
summary(horsies_pca)

# select principal components that capture 90% of variation
horsies_pcs <- horsies_pca$x |> data.frame() |> select(1:10)
# subset each just for relevant principal components
lat_mvmt_pcs <- pca_lat_mvmt$x |> data.frame() |> select(1:4)

# clustering on PCs directly ####
set.seed(27072023) # same seed as summer
horsies_mclust <- Mclust(horsies_pcs)
lat_mvmt_pcs_mclust <- Mclust(lat_mvmt_pcs)

# also joined to full data, NOT data w PCs
horse_summary_stats <- cbind(horse_summary_stats,
      "cluster" = horsies_mclust$classification)
lateral_movement <- cbind(lateral_movement,
      "cluster" = lat_mvmt_pcs_mclust$classification)

# remove summary stats, keep cluster assignments only
horse_summary_stats <- horse_summary_stats |> 
  select(horse_id, horse_name, contains("cluster"))
lateral_movement <- lateral_movement |> 
  select(horse_id, horse_name, contains("cluster"))
# save results
write.csv(lateral_movement, "C:/horsies/clustering-on-pcs-results/lateral-movement-clust-on-pcs-results.csv")
write.csv(horse_summary_stats, "C:/horsies/clustering-on-pcs-results/horse-clust-on-pcs-results.csv")