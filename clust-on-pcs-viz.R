setwd("C:/horsies/clustering-on-pcs-results")
library(tidyverse)
# read in data ####
horse_clusters <- read_csv("horse-clust-on-pcs-results.csv", 
                           col_select = -1)
lat_mvmt_clusters <- 
  read_csv("lateral-movement-clust-on-pcs-results.csv",
           col_select = -1)

# read in profiles ####
setwd("C:/horsies")
horse_profiles <- read_csv("outputted-csvs/horse_profiles.csv") |>
  filter(horse_id != 3563) # horse with no data
# join profile data ####
lat_mvmt_profiles <- right_join(lat_mvmt_clusters, horse_profiles,
                               by = c("horse_id", "horse_name"))
all_horse_profiles <- right_join(horse_clusters, horse_profiles,
                             by = c("horse_id", "horse_name"))
# clean up environment
rm(horse_profiles, horse_clusters, lat_mvmt_clusters)

# viz time ####
horse_injruies_plot <- all_horse_profiles |> 
  mutate(report_plot = 
           if_else(if_injury_reported == T, "Yes", "No")) |> 
  ggplot(aes(x = cluster, fill = report_plot)) + 
  geom_bar(position = "fill") + scale_fill_brewer(
    type = "qual", palette = "Set1", direction = -1) + 
  scale_x_discrete() + theme_classic() + 
  labs(subtitle = "Horses Clustered by Movement Profiles",
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