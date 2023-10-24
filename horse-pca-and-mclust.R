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

# pca time! ####
## extract matrices for each feature | convert to matrix for prcomp()
horsies_no_ids <- horse_summary_stats |> 
  data.frame() |> select(-c(horse_id, horse_name)) |> as.matrix()

# statistics pca
horsies_pca <- prcomp(horsies_no_ids, 
                           center = TRUE, scale. = TRUE)
#summary(horsies_pca)

# visualizations | not run
# fviz_eig(horsies_pca)
# select first ten principal components. explain 90% of the variance
#fviz_pca_biplot(horsies_pca, alpha.ind = 0.3, 
#                geom.var = "arrow", geom.ind = "point")
#fviz_pca_ind(horsies_pca)

# extract components
horsies_loadings <- tidy(horsies_pca, matrix = "loadings") |> 
  arrange(PC, desc(abs(value))) |> 
  group_by(PC) |> 
  top_n(3, abs(value)) |> 
  filter(PC <= 10) |> 
  select(PC, column, value)

# clustering ####

# extract the important stats as designated by the PCA
race_summary_stats <- horse_summary_stats |> 
  select(horse_id, horse_name, all_of(horsies_loadings$column))

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

# clustering time! ####
# set the seed for reproduceability
set.seed(27072023)
lat_mvmt_mclust <- Mclust(select(side_mvmt_summaries,
                                 -c(horse_id, horse_name)))
horsies_mclust <- Mclust(select(race_summary_stats,
                                -c(horse_id, horse_name)))
# join the data
side_mvmt_summaries <- cbind(side_mvmt_summaries, 
                   "cluster" = lat_mvmt_mclust$classification)
race_summary_stats <- cbind(race_summary_stats,
                         "cluster" = horsies_mclust$classification)

# save the data!
#write.csv(race_summary_stats, "C:/horsies/clustering-results-revised/race-summaries.csv")
# write.csv(side_mvmt_summaries, "C:/horsies/clustering-results-revised/lateral-movement-summaries.csv")
