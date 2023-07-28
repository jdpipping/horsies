# load required packages ####
library(tidyverse)
library(ggplot2)

# load horse data ####

horse_data <- read_csv('processed-horse-data/final_horse_strains.csv')

track_outline <- read_csv('processed-horse-data/track_outlines_for_viz.csv') |> 
  filter(track_id == 'AQU',
         course_type == 'D')

# visualizing a race ####

# visualize horse trajectories
horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19",
         race_number == 1) %>%
  arrange(horse_id, frame_id) %>%
  ggplot(aes(x = x,
             y = y)) +
  geom_path(show.legend = FALSE,
            aes(color = as.factor(horse_name),
                group = as.factor(horse_name))) +
  # display track outlines
  geom_path(data = track_outline, show.legend = FALSE,
            aes(group = as.factor(outline_type))) +
  labs(title = 'The Aqueduct on April 19th, 2019: Race 1',
       x = 'x (m)',
       y = 'y (m)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# speed vs time ####

horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  ggplot(aes(x = (frame_id / 4),
             y = speed,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Speed vs Time for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Time (s)',
       y = 'Speed (m/s)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# standardized speed vs time ####

horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  ggplot(aes(x = (frame_id / 4),
             y = std_speed,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Standardized Speed vs Time for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Time (s)',
       y = 'Speed (m/s)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# acceleration vs time ####

horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  ggplot(aes(x = (frame_id / 4),
             y = acceleration,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Acceleration vs Time for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Time (s)',
       y = 'Acceleration (m/s^2)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# standardized acceleration vs time ####

horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  ggplot(aes(x = (frame_id / 4),
             y = std_acceleration,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Standardized Acceleration vs Time for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Time (s)',
       y = 'Acceleration (m/s^2)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# load 1600m horse data ####

horse_data_1600 <- read_csv('processed-horse-data/lateral_movement_1600.csv')

# lateral movement vs time ####

horse_data_1600 %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  ggplot(aes(x = (frame_id / 4),
             y = abs(side_movement),
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Lateral Movement vs Time for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Time (s)',
       y = 'Lateral Movement (m)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# cumulative lateral movement vs time ####

horse_data_1600 %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  group_by(horse_id) %>%
  mutate(cum_side_movement = cumsum(abs(side_movement))) %>%
  ggplot(aes(x = frame_id / 4,
             y = cum_side_movement,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Cumulative Lateral Movement vs Time for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Time (s)',
       y = 'Cumulative Lateral Movement (m)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# loading tidyverse, full pre-injury data ####
library(tidyverse)

injured_last5 <- read_csv('processed-horse-data/injured_last5_full.csv')

# velocity vs time: last 5 races before injury ####

injured_last5 %>%
  filter(horse_id < 95) %>%
  ggplot(aes(x = frame_id,
             y = std_speed,
             color = as.double(injury_date - race_date))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Speed (Z) vs Frame ID for Each Horse',
       subtitle = 'Last 5 Races Before Injury',
       x = 'Frame ID',
       y = 'Speed (m/s)') +
  facet_wrap(~ horse_id) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# acceleration vs time: last 5 races before injury ####

injured_last5 %>%
  filter(horse_id < 95) %>%
  ggplot(aes(x = frame_id,
             y = std_acceleration,
             color = as.double(injury_date - race_date))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Acceleration (Z) vs Frame ID for Each Horse',
       subtitle = 'Last 5 Races Before Injury',
       x = 'Frame ID',
       y = 'Acceleration (m/s^2)') +
  facet_wrap(~ horse_id) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
