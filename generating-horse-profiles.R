## Variables we want in the horse profiles ##

# horse ID
# number of races ran
# mean distance of races, run-up, finishing place, and ranking
# date and type of horse death/injury events
# min, 1st quartile, median, 3rd quartile, max days between races
# number of times horse has dnf'd and dnf date


#### Loading the Packages ####

library(tidyverse)
library(lubridate)

#### Loading in the Relevant CSV Files ####

horse_names <- read_csv("horse-cooking-data/horse_names.csv")
jockey_ids <- read_csv("horse-cooking-data/jockey_ids.csv")
partially_cleaned <- read_csv("horse-data/partially_cleaned_data.csv")
horse_injury_info <- read_csv("horse-cooking-data/horse_injuries_2019-2020.csv") 

names(horse_injury_info) <- str_to_lower(names(horse_injury_info)) %>% 
  str_replace(" ","_")


### Kaggle Code:
#horse_injury_events <- read_delim("horse-cooking-data/equine.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
#  mutate(incident_date = as.Date(incident_date, format = "%m/%d/%Y")) %>% 
#  drop_na(horse, death_or_injury, incident_date) %>% 
#  filter(incident_date >= "2019-01-01" & incident_date <= "2020-02-01") 
  

#### Extracting the Number of Races for each Horse ####

horse_race_count <- partially_cleaned %>% 
  group_by(race_id, horse_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  group_by(horse_id) %>% 
  summarize(n_races = n())

#### Extracting the Averages for each Horse ####

horse_avgs <- partially_cleaned %>% 
  group_by(race_id, horse_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  group_by(horse_id) %>% 
  summarize(avg_race_dist_meters = mean(race_distance_metres, na.rm = TRUE),
            avg_race_runup = mean(run_up_distance, na.rm = TRUE),
            avg_finishing_place = mean(finishing_place, na.rm = TRUE),
            avg_ranking = mean(rank, na.rm = TRUE))

#### Extracting the Number of Days since Last Race for each Horse ####

horse_time_between_races <- partially_cleaned %>% 
  group_by(race_id, horse_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  group_by(horse_id) %>% 
  mutate(n_races = n()) %>% 
  filter(n_races > 1) %>% 
  select(horse_id, race_date) %>% 
  arrange(horse_id, race_date)

diff_time <- c()

for(i in 1:(nrow(horse_time_between_races))){
  diff_time[i] <- as.numeric(horse_time_between_races$race_date[i]-horse_time_between_races$race_date[i-1])
}

#### Calculating the Summary Statistics for Days between Races ####

horse_time_between_races <- cbind(horse_time_between_races, diff_time) %>% 
  rename(days_since_last_race = `...3`) %>% 
  mutate(days_since_last_race = if_else(days_since_last_race <= 0, NA, days_since_last_race)) %>% 
  drop_na(days_since_last_race) %>% 
  group_by(horse_id) %>% 
  summarize(min_days_between_races = min(days_since_last_race), 
            first_quartile_days_bewteen_races = quantile(days_since_last_race, probs = .25),
            median_days_between_races = median(days_since_last_race),
            third_quartile_days_bewteen_races = quantile(days_since_last_race, probs = .75),
            max_days_between_races = max(days_since_last_race))


#### Extracting the DNF Information for each Horse ####
  
horse_dnf_info <- partially_cleaned %>% 
  select(race_id, horse_id, race_date, dnf) %>% 
  filter(dnf == TRUE) %>% 
  group_by(race_id, horse_id) %>% 
  slice_head() %>% 
  ungroup() %>% 
  group_by(horse_id) %>% 
  summarize(n_dnf = n(),
            date_dnf_1 = min(race_date),
            date_dnf_2 = if_else(n_dnf > 1, max(race_date), NA)) 

#### Extracting Injury Counts for each Horse ####
  
horse_injury_counts <- horse_injury_info %>%
  mutate(incident_date = as.Date(incident_date, format = "%m/%d/%Y")) %>% 
  mutate(incident_date = str_replace(incident_date, "^", "20")) %>%
  mutate(incident_type = str_to_title(incident_type)) %>% 
  drop_na(horse_name, incident_type, incident_date) %>% 
  group_by(horse_name, incident_type) %>% 
  mutate(n_injury_type = n(),
         date_injury_1 = min(incident_date),
         date_injury_2 = if_else(n_injury_type > 1, max(incident_date), NA)) %>% 
  slice_head() %>% 
  select(horse_name, date_injury_1, date_injury_2, incident_type, n_injury_type) 

#### FINAL JOIN ####

horse_profiles <- horse_names %>% 
  left_join(horse_race_count, by = "horse_id") %>% 
  left_join(horse_avgs, by = "horse_id") %>% 
  left_join(horse_injury_counts, by = "horse_name") %>% 
  mutate(incident_type = if_else(is.na(incident_type), "None", incident_type)) %>% 
  rename(incident_type_reported = incident_type) %>% 
  left_join(horse_dnf_info, by = "horse_id") %>% 
  mutate(n_dnf = if_else(is.na(n_dnf), 0, n_dnf)) %>% 
  left_join(horse_time_between_races, by = "horse_id")

#### Writing the Horse Profiles CSV ####

write_csv(horse_profiles, "horse-cooking-data/horse_profiles.csv")
  
  
  

  

