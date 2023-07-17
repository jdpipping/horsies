# loading tidyverse, data ####
library(tidyverse)

horse_data <- read_csv("~/final_horse_strains.csv")

# data visualization ####

# velocity vs time
horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  ggplot(aes(x = frame_id,
             y = speed,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Speed vs Frame ID for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Frame ID',
       y = 'Speed (m/s)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# acceleration vs time
horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  ggplot(aes(x = frame_id,
             y = acceleration,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Acceleration vs Frame ID for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Frame ID',
       y = 'Acceleration (m/s^2)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# lateral movement vs time 
horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  ggplot(aes(x = frame_id,
             y = abs(side_movement),
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Lateral Movement for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Frame ID',
       y = 'Lateral Movement (m)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))

# cumulative lateral movement vs time
horse_data %>%
  filter(track_id == "AQU",
         race_date  == "2019-04-19") %>%
  group_by(horse_id) %>%
  mutate(cum_side_movement = cumsum(abs(side_movement))) %>%
  ggplot(aes(x = frame_id,
             y = cum_side_movement,
             color = factor(horse_name))) +
  geom_line(show.legend = FALSE) +
  labs(title = 'Cumulative Lateral Movement for Each Horse',
       subtitle = 'At the Aqueduct on April 19th, 2019',
       x = 'Frame ID',
       y = 'Cumulative Lateral Movement (m)') +
  facet_wrap(~ race_number) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
