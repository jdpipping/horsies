setwd("C:/horsies/clustering-results-revised")
library(tidyverse)
# read in data
acceleration_summaries <- read_csv("acceleration-summaries.csv", 
                                   col_select = -1)
side_mvmt_summaries <- read_csv("lateral-movement-summaries.csv",
                                col_select = -1)
speed_summaries <- read_csv("speed-summaries.csv",
                            col_select = -1)
strains_summaries <- read_csv("strain-summaries.csv",
                              col_select = -1)

# read in profiles
setwd("C:/horsies")
horse_profiles <- read_csv("outputted-csvs/horse_profiles.csv") |>
  filter(horse_id != 3563) # horse with no data
# join profile data
accel_profiles <- left_join(acceleration_summaries, horse_profiles,
                            by = c("horse_id", "horse_name"))
lat_mvmt_profiles <- left_join(side_mvmt_summaries, horse_profiles,
                               by = c("horse_id", "horse_name"))
speed_profiles <- left_join(speed_summaries, horse_profiles,
                            by = c("horse_id", "horse_name"))
strain_profiles <- left_join(strains_summaries, horse_profiles,
                             by = c("horse_id", "horse_name"))
# clean up environment
rm(acceleration_summaries, side_mvmt_summaries, speed_summaries, strains_summaries, horse_profiles)

# injury viz ####
speed_injruies_plot <- speed_profiles |> 
  mutate(report_plot = 
           if_else(if_injury_reported == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, fill = report_plot)) + 
  geom_bar(position = "fill") + scale_fill_brewer(
    type = "qual", palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Horses Clustered by Speed Profiles",
       x = "Clusters",y = "Proportion", 
       fill = "Injury Reported?") + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

accel_injruies_plot <- accel_profiles |> 
  mutate(report_plot = 
           if_else(if_injury_reported == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, fill = report_plot)) + 
  geom_bar(position = "fill") + scale_fill_brewer(
    type = "qual", palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Horses Clustered by Acceleration Profiles",
       x = "Clusters",y = "Proportion", 
       fill = "Injury Reported?") + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

lat_mvmt_injruies_plot <- lat_mvmt_profiles |> 
  mutate(report_plot = 
           if_else(if_injury_reported == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, fill = report_plot)) + 
  geom_bar(position = "fill") + scale_fill_brewer(
    type = "qual", palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Horses Clustered by Lateral Movement Profiles",
       x = "Clusters",y = "Proportion", 
       fill = "Injury Reported?") + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

strain_injruies_plot <- strain_profiles |> 
  mutate(report_plot = 
           if_else(if_injury_reported == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, fill = report_plot)) + 
  geom_bar(position = "fill") + scale_fill_brewer(
    type = "qual", palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Horses Clustered by Strain Rate Profiles",
       x = "Clusters",y = "Proportion", 
       fill = "Injury Reported?") + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

cowplot::plot_grid(speed_injruies_plot, accel_injruies_plot,
                   lat_mvmt_injruies_plot, strain_injruies_plot,
                   ncol = 2)

# under-racing viz ####
speed_underrace <- speed_profiles |>
  mutate(report_plot = 
           if_else(ever_under_raced == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, 
             fill = report_plot)) + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(type = "qual", 
                    palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Clustered by Speed Profiles",
       x = "Clusters",y = "Proportion", 
       fill = "Ever Under-Raced? (2019-2021)") + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

accel_underrace <- accel_profiles |>
  mutate(report_plot = 
           if_else(ever_under_raced == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, 
             fill = report_plot)) + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(type = "qual", 
                    palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Clustered by Acceleration Profiles",
       x = "Clusters",y = "Proportion", 
       fill = "Ever Under-Raced? (2019-2021)") + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

lat_underrace <- lat_mvmt_profiles |>
  mutate(report_plot = 
           if_else(ever_under_raced == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, 
             fill = report_plot)) + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(type = "qual", 
                    palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Clustered by Lateral Movement Profiles",
       x = "Clusters",y = "Proportion", 
       fill = "Ever Under-Raced? (2019-2021)") + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

strain_underrace <- strain_profiles |>
  mutate(report_plot = 
           if_else(ever_under_raced == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, 
             fill = report_plot)) + 
  geom_bar(position = "fill") + 
  scale_fill_brewer(type = "qual", 
                    palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Clustered by Strain Rate Profiles",
       x = "Clusters",y = "Proportion", 
       fill = "Ever Under-Raced? (2019-2021)") + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

cowplot::plot_grid(speed_underrace, accel_underrace,
                   lat_underrace, strain_underrace,ncol = 2)

speed_profiles |> 
  filter(cluster == 8, horse_id != 2260, horse_id != 1842) |> 
  select(1:7, 13, 17, 18, 40) |> group_by(if_injury_reported) |> 
  summarize(avg_median_speed = mean(median_speed),
            avg_min_speed = mean(min_speed),
            avg_max_speed = mean(max_speed),
            avg_cv_speed = mean(cv_speed),
            avg_race_distance = mean(avg_race_dist_meters),
            avg_weight_carried = mean(avg_weight_carried)) |> 
  view()

speed_profiles |> 
  filter(horse_id != 2260, horse_id != 1842) |> 
  group_by(cluster) |> 
  summarize(avg_median_speed = mean(median_speed),
            avg_min_speed = mean(min_speed),
            avg_max_speed = mean(max_speed),
            avg_cv_speed = mean(cv_speed)) |> 
  view()