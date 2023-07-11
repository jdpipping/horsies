# loading tidyverse, data ####
library(tidyverse)

horse_data <- read_csv("horse-cooking-data/tracking_data_cleaned.csv")

head(horse_data)
# get variable names ####
variable_names <- c()
for (variable in colnames(horse_data)) {
  variable_names <- c(variable_names, variable)
}
sort(variable_names)
# data visualization (old) ####

# velocity vs time
horse_data %>%
  filter(track_id == "BEL",
         race_date  == "2019-04-26") %>%
  ggplot(aes(x = frame_id,
             y = speed,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ race_number)

# acceleration vs time
horse_data %>%
  filter(track_id == "BEL",
         race_date  == "2019-04-26") %>%
  ggplot(aes(x = frame_id,
             y = acceleration,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ race_number)

# lateral movement correlations ####
horse_data_lat <- horse_data %>%
  group_by(race_id, horse_id) %>%
  mutate(aggregate_lateral_movement = sum(side_movement, na.rm = TRUE))

correlations <- cor(horse_data_lat[sapply(horse_data_lat, is.numeric)])

lat_corr <- correlations[, 'aggregate_lateral_movement'] %>%
  sort(decreasing = TRUE)
lat_corr