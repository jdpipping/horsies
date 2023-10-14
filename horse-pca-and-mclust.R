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

# strain rate pca
pca_strain <- prcomp(strain_stats, center = TRUE, scale. = TRUE)
#summary(pca_strain)

# visualizations | currently not run
# fviz_eig(pca_strain)
# using a threshold of 90%, we would select the first five principal components. explain 92% of the variance
# fviz_pca_biplot(pca_strain, alpha.ind = 0.5, alpha.var = 0.75,
#                 geom.var = "arrow", geom.ind = "point")
# fviz_pca_ind(pca_strain)

# investigate principal components
strain_loadings <- tidy(pca_strain, matrix = "loadings") |> 
  arrange(PC, desc(abs(value))) |> 
  group_by(PC) |> top_n(1, abs(value)) |> 
  filter(PC <= 5) |> select(PC, column, value)

# speed pca
pca_speed <- prcomp(speed_stats, center = TRUE, scale. = TRUE)
#summary(pca_speed)

# visualizations | not run
# fviz_eig(pca_speed)
# select first four principal components. explain 97% of the variance
# fviz_pca_biplot(pca_speed, alpha.ind = 0.5, 
#                geom.var = "arrow", geom.ind = "point")
# fviz_pca_ind(pca_speed)

# investigate principal components
speed_loadings <- tidy(pca_speed, matrix = "loadings") |> 
  arrange(PC, desc(abs(value))) |> 
  group_by(PC) |> top_n(1, abs(value)) |> 
  filter(PC <= 4) |> select(PC, column, value)
# acceleration pca
pca_acceleration <- prcomp(acceleration_stats, 
                           center = TRUE, scale. = TRUE)
#summary(pca_acceleration)

# visualizations | not run
# fviz_eig(pca_acceleration)
# select first four principal components. explain 95% of the variance
#fviz_pca_biplot(pca_acceleration, alpha.ind = 0.5, 
#                geom.var = "arrow", geom.ind = "point")
#fviz_pca_ind(pca_acceleration)

# extract components
acceleration_loadings <- tidy(pca_acceleration, matrix = "loadings") |> 
  arrange(PC, desc(abs(value))) |> 
  group_by(PC) |> top_n(1, abs(value)) |> 
  filter(PC <= 4) |> select(PC, column, value)

# clustering ####

# extract the important stats as designated by the PCA
race_summary_stats <- final_horse_data |> 
  filter(horse_id != 3563, is.finite(avg_strain) & is.finite(total_strain)) |>
  group_by(horse_id, horse_name) |> 
  summarise(mean_acceleration = mean(acceleration),
            median_speed = median(speed),
            median_acceleration = median(acceleration),
            min_speed = min(speed),
            max_speed = max(speed),
            range_acceleration = max(acceleration) - min(acceleration),
            iqr_acceleration = IQR(acceleration),
            cv_speed = (sd(speed) / mean(speed)),
            median_total_strain = median(total_strain),
            min_avg_strain = min(avg_strain),
            iqr_total_strain = IQR(total_strain),
            min_total_strain = min(total_strain),
            sd_avg_strain = sd(avg_strain)) |> ungroup()

# clean up environment
#rm(acceleration_stats, speed_stats, strain_stats,
#   acceleration_loadings, speed_loadings, strain_loadings,
#   final_horse_data, final_horse_strains)

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

# ungroup and drop identifying variables
side_mvmt_pca <- lateral_movement |> 
  data.frame() |> select(-c(horse_id, horse_name))
# run pca
pca_lat_mvmt <- prcomp(side_mvmt_pca, center = TRUE, scale. = TRUE)
# loadings matrix
lat_mvmt_loadings <- tidy(pca_lat_mvmt, matrix = "loadings") |> 
  arrange(PC, desc(abs(value))) |> 
  group_by(PC) |> top_n(1, abs(value)) |> 
  filter(PC <= 4) |> select(PC, column, value)
# important summary statistics on a race level
side_mvmt_summaries <- horse_movement |> 
  group_by(horse_id, horse_name) |> 
  summarize(mean_side_movement = mean(side_movement),
            max_side_movement = max(side_movement),
            range_side_movement = max(side_movement) - min(side_movement),
            iqr_side_movement = IQR(side_movement)) |> ungroup()

# back to the good stuff ####
# strain stats
strains_summaries <- race_summary_stats |> data.frame() |> 
  select(-matches(c("speed", "acceleration")))
# speed stats
speed_summaries <- race_summary_stats |> data.frame() |> 
  select(-matches(c("strain", "acceleration")))
# acceleration stats
acceleration_summaries <- race_summary_stats |> data.frame() |> 
  select(-matches(c("speed", "strain")))

# clustering time!
# set the seed for reproduceability
set.seed(27072023)
strain_mclust <- Mclust(select(strains_summaries, 
                               -c(horse_id, horse_name)))
speed_mclust <- Mclust(select(speed_summaries, 
                               -c(horse_id, horse_name)))
acceleration_mclust <- Mclust(select(acceleration_summaries, 
                               -c(horse_id, horse_name)))
lat_mvmt_mclust <- Mclust(select(side_mvmt_summaries,
                                 -c(horse_id, horse_name)))

# join the data
strains_summaries <- cbind(strains_summaries, 
                           "cluster" = strain_mclust$classification)
side_mvmt_summaries <- cbind(side_mvmt_summaries, 
                   "cluster" = lat_mvmt_mclust$classification)
speed_summaries <- cbind(speed_summaries,
                         "cluster" = speed_mclust$classification)
acceleration_summaries <- cbind(acceleration_summaries,
                                "cluster" = acceleration_mclust$classification)

# save the data!
# write.csv(acceleration_summaries, "C:/horsies/clustering-results-revised/acceleration-summaries.csv")
# write.csv(side_mvmt_summaries, "C:/horsies/clustering-results-revised/lateral-movement-summaries.csv")
# write.csv(speed_summaries, "C:/horsies/clustering-results-revised/speed-summaries.csv")
# write.csv(strains_summaries, "C:/horsies/clustering-results-revised/strain-summaries.csv")
