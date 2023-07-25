### How the Strain Information was extracted for each Horse ###

#horse_strain_info |> 
#  dplyr::select(horse_id, race_id, ends_with("strain")) |> 
#  filter(is.finite(total_strain)) |>
#  group_by(race_id, horse_id) |>
#  summarize(total_strain_per_race = sum(total_strain, na.rm = TRUE), .groups = "keep") |> 
#  ungroup() |> 
#  group_by(horse_id) |> 
#  summarize(avg_total_race_strain = mean(total_strain_per_race, na.rm = TRUE)) |>
#  write_csv("horse-cooking-data/avg_horse_strains.csv")

#### Loading the Packages ####

library(tidyverse)
library(lubridate)

#### Loading in the Relevant CSV Files ####

horse_names <- read_csv("horse-cooking-data/horse_names.csv")
tracking_partially_cleaned <- read_csv("horse-cooking-data/partially_cleaned_data.csv")
horse_injury_info <- read_csv("horse-cooking-data/horse_injuries.csv") 
horse_strain_avgs <- read_csv("horse-cooking-data/avg_horse_strains.csv")
horse_3year_racing_NYRA <- read_csv("horse-cooking-data/horses_3years_racing_NYRA.csv")
extreme_resid_horses <- read_csv("outputted-csvs/extreme_resid_horses.csv")

#### Extracting the Number of Races for each Horse ####

horse_race_count <- tracking_partially_cleaned |> 
  group_by(race_id, horse_id) |> 
  slice_head() |> 
  ungroup() |> 
  group_by(horse_id) |> 
  summarize(n_races_tracked_2019 = n())

#### Extracting the Averages for each Horse ####

horse_avgs <- tracking_partially_cleaned |> 
  group_by(race_id, horse_id) |> 
  slice_head() |> 
  ungroup() |> 
  group_by(horse_id) |> 
  summarize(avg_race_dist_meters = mean(race_distance_metres, na.rm = TRUE),
            avg_finishing_place = mean(finishing_place, na.rm = TRUE),
            avg_ranking = mean(rank, na.rm = TRUE),
            avg_acceleration = mean(acceleration, na.rm = TRUE),
            avg_speed = mean(speed, na.rm = TRUE),
            avg_weight_carried = mean(weight_carried, na.rm = TRUE))
  

#### Extracting the Number of Days since Last Race for each Horse ####

horse_time_between_races <- tracking_partially_cleaned |> 
  group_by(race_id, horse_id) |> 
  slice_head() |> 
  ungroup() |> 
  group_by(horse_id) |> 
  mutate(n_races = n()) |> 
  filter(n_races > 1) |> 
  dplyr::select(horse_id, race_date) |> 
  arrange(horse_id, race_date)

diff_time <- c()

for(i in 1:(nrow(horse_time_between_races))){
  diff_time[i] <- as.numeric(horse_time_between_races$race_date[i]-horse_time_between_races$race_date[i-1])
}

#### Calculating the Summary Statistics for Days between Races ####

horse_time_between_races <- cbind(horse_time_between_races, diff_time) |> 
  rename(days_since_last_race = `...3`) |> 
  mutate(days_since_last_race = if_else(days_since_last_race <= 0, NA, days_since_last_race)) |> 
  drop_na(days_since_last_race) |> 
  group_by(horse_id) |> 
  summarize(min_days_between_races = min(days_since_last_race), 
            first_quartile_days_between_races = quantile(days_since_last_race, probs = .25),
            median_days_between_races = median(days_since_last_race),
            third_quartile_days_between_races = quantile(days_since_last_race, probs = .75),
            max_days_between_races = max(days_since_last_race))


#### Extracting the DNF Information for each Horse ####
  
horse_dnf_info <- tracking_partially_cleaned |> 
  dplyr::select(race_id, horse_id, race_date, dnf) |> 
  filter(dnf == TRUE) |> 
  group_by(race_id, horse_id) |> 
  slice_head() |>
  ungroup() |> 
  group_by(horse_id) |> 
  summarize(n_dnf = n(),
            date_dnf_1 = if_else(n_dnf > 0,min(race_date), NA),
            date_dnf_2 = if_else(n_dnf > 1, max(race_date), NA)) 

#### Extracting the Horses that Raced at least once in 2019 ####

horses_starts_2019_2020 <- horse_3year_racing_NYRA |> 
  left_join(horse_names, by = c("horse name" = "horse_name")) |> 
  mutate(race_year = year(`race date`)) |>
  #only New Year's Race from 2022 is recorded. So, the year is removed
  filter(!(race_year == 2022)) |> 
  group_by(horse_id, race_year) |> 
  summarize(n_races = n(), .groups = "keep") |>  
  pivot_wider(names_from = race_year, values_from = n_races, names_prefix = "n_races_")

#### Extracting Severe Injury Counts for each Horse ####
  
horse_injury_counts <- horse_injury_info |>
  mutate(injury_date = ymd(as.Date(Date, format = "%m/%d/%Y"))) |> 
  mutate(last_race_pre_injury = as.Date(`Last Race`, format = "%m/%d/%Y")) |>
  filter(injury_date >= "2019-01-01" & injury_date <= "2020-02-01") |> 
  rename(horse_name = `Name of Horse`, injury_location = `Activity`, 
         injury_surface_location = `Surface/Location`, injury_track_location = `Specific Location`,
         injury_details = `Injury/Illness Details`, injury_track = `Track`,
         days_injury_to_race = `Days from incident to next race`,
         race_length = `Length of Race`, days_off_pre_injury = `Days Off`, fatal = Fatal) |>
  dplyr::select(horse_name, injury_track, contains("injury"), contains("race"), contains("days"), fatal, 
         -`Last Race`, -starts_with("Not")) |>
  mutate(race_length_cleaned = str_remove(race_length, "Miles|Mile|F")) |> 
  mutate(race_length_cleaned = str_remove(race_length_cleaned, "(Hurdle)")) |> 
  mutate(race_length_cleaned = str_remove(race_length_cleaned, "\\(\\)")) |>
  mutate(race_length_cleaned = str_replace(race_length_cleaned, "/12", "1/2")) |>
  mutate(race_length_cleaned = str_replace(race_length_cleaned, "Scratch",  NA_character_)) |> 
  mutate(race_length_cleaned = if_else(str_length(race_length_cleaned) > 2, 
                                       str_replace(race_length_cleaned, " ","+"), race_length_cleaned)) |> 
  mutate(race_length_cleaned = str_remove(race_length_cleaned,"~")) |> 
  mutate(race_length_cleaned_eval = sapply(race_length_cleaned, function(x) eval(parse(text=x)))) |> 
  mutate(race_length_injury_meters = case_when(
    grepl("F", race_length) ~ race_length_cleaned_eval*201.168,
    grepl("Miles|Mile", race_length) ~ race_length_cleaned_eval*1609.34,
    TRUE ~ NA)) |> 
  mutate(across(where(is.character), ~if_else(is.na(.x), "Unknown", .x))) |> 
  group_by(horse_name, injury_details) |> 
  mutate(n_injury_type = n()) |> 
  mutate(injury_track_location = str_to_lower(injury_track_location)) |> 
  mutate(injury_track_location_clean = case_when(
    str_detect(injury_track_location, "1/2|8th pole|backstretch|back stretch|finalized|after the start|start of race") ~ "Backstretch",
    str_detect(injury_track_location, "5/16|3/8|7/16|1/4|first turn") ~ "Far Turn",
    str_detect(injury_track_location, "gate|paddock|stall") ~ "Starting Gate",
    str_detect(injury_track_location, "1/8|head|1/16|top|stretch") ~ "Homestretch",
    str_detect(injury_track_location, "after the race|finish|wire") ~ "After the Race",
    TRUE ~ injury_track_location
  )) |> 
  mutate(injury_track_location_clean = str_to_title(injury_track_location_clean)) |> 
  mutate(injury_track_location_clean = factor(injury_track_location_clean,
                                              levels = c("Unknown", "After The Race", "Homestretch", "Far Turn", "Backstretch", "Starting Gate" ))) |> 
  dplyr::select(-race_length_cleaned, -race_length, -race_length_cleaned_eval, -injury_track_location)


#### Compiling horses who under-raced relative to other horse's their age in a specific calendar year ####

under_raced <- extreme_resid_horses |>
  mutate(under_raced = TRUE) |>
  rename(age_year_when_under = age_year,
         race_year_when_under = race_year,
         n_races_when_under = n_races) |>
  dplyr::select(horse_id, under_raced, race_year_when_under) |>
  group_by(horse_id, race_year_when_under) |>
  slice_head() |>
  pivot_wider(names_from = race_year_when_under, values_from = under_raced, names_prefix = "under_raced_") |>
  select(horse_id, under_raced_2019, under_raced_2020, everything()) |>
  mutate(ever_under_raced = TRUE)

#### FINAL JOIN AND MUTATIONS ####

horse_profiles <- horse_names |>
  right_join(horse_race_count, by = "horse_id") |> 
  left_join(horses_starts_2019_2020, by = "horse_id") |> 
  mutate(n_races_diff_2019 = n_races_2019 - n_races_tracked_2019) |> 
  left_join(horse_avgs, by = "horse_id") |> 
  left_join(horse_dnf_info, by = "horse_id") |> 
  mutate(n_dnf = if_else(is.na(n_dnf), 0, n_dnf)) |> 
  left_join(horse_strain_avgs, by = "horse_id") |> 
  left_join(horse_time_between_races, by = "horse_id") |> 
  left_join(horse_injury_counts, by = "horse_name") |> 
  mutate(injury_details = if_else(is.na(injury_details), "No Reported Injury", injury_details)) |> 
  mutate(n_injury_type = if_else(is.na(n_injury_type), 0, n_injury_type)) |> 
  rename(injury_details_reported = injury_details) |> 
  mutate(if_injury_reported = if_else(injury_details_reported == "No Reported Injury", FALSE, TRUE)) |> 
  mutate(dnf_from_injury = if_else(injury_date == date_dnf_1 | injury_date == date_dnf_2, TRUE, FALSE, missing = NA)) |>
  left_join(under_raced, by = "horse_id") |>
  mutate(across(contains("under_raced"), ~replace_na(.x, FALSE))) |>
  group_by(horse_id) |>
  slice_head()
  

#### Writing the Horse Profiles CSV ####

write_csv(horse_profiles, "./outputted-csvs/horse_profiles.csv")

