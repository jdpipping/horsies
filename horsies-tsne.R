# data prep ####
library(tidyverse)
library(factoextra)
library(broom)
library(mclust)

# file path may vary | this file was downloaded from the google drive
file_path <- file.choose()
## my file path is below:
# file_path <- "C:/Users/krisa/Contacts/Downloads/final_horse_strains.csv"
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

horsies_no_ids <- horse_summary_stats |> 
  data.frame() |> select(-c(horse_id, horse_name))

# speed stats
speed_statistics <- horsies_no_ids |> 
  select(matches("speed")) |> as.matrix()
# accel stats
acceleration_statistics <- horsies_no_ids |> 
  select(matches("acceleration")) |> as.matrix()
# strain stats
strain_statistics <- horsies_no_ids |> 
  select(matches("strain")) |> as.matrix()

# lateral movement detour ####
# read in data
file_path <- file.choose()
# file_path <- "C:/Users/krisa/tracking_data_cleaned_1600.csv"
tracking_data_cleaned_1600 <- read_csv(file_path)
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

# ungroup and drop identifying variables
side_mvmt_no_ids <- lateral_movement |> 
  data.frame() |> select(-c(horse_id, horse_name))

# tsne time ####
library(Rtsne)
library(rgl)
speed_tsne <- Rtsne(speed_statistics, dims = 3, perplexity = 500, 
                    check_duplicates = FALSE, pca = FALSE, 
                    normalize = TRUE)
# the results are not loadings, like in PCA. Recall that the 
# loadings of a principal component represent the weights of
# the linear combination of features/covariates that it was 
# performed on (i.e., how much each feature contributed to
# that principal component). the coordinates for t-SNE's reduceed 
# dimensions represent the pairwise similarities captured in high-
# dimensional space mapped to lower-dimensional space.

speed_coors_x <- speed_tsne$Y[,1]
speed_coors_y <- speed_tsne$Y[,2]
speed_coors_z <- speed_tsne$Y[,3]
speed_coors <- data.frame(X=speed_coors_x, Y=speed_coors_y, Z=speed_coors_z)
plot3d(speed_coors)
