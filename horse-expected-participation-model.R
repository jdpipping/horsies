#### load tidyverse, horse names, participation data ####
library(tidyverse)
library(broom)

horse_names <- read_csv("horse-cooking-data/horse_names.csv")
participation_data <- read_csv("horse-cooking-data/horses_3years_racing_NYRA.csv") |>
  rename(horse_name = `horse name`,
         birth_date = `birth date`,
         race_date = `race date`) |>
  drop_na(horse_name)
horse_profiles_injured <- read_csv("outputted-csvs/horse_profiles_all.csv") |>
  filter(if_injury_reported == TRUE) 

#### append horse names to participation data ####
## remove 2022 because only race from that year is from 01/01/2022

id_participation_data <- participation_data |>
  left_join(horse_names, by = 'horse_name') |>
  mutate(race_year = year(race_date)) |>
  filter(!(race_year == 2022))

#### count number of races per horse at each age (in years) and calendar year ####

library(lubridate)

participation_data_model <- id_participation_data |>
  mutate(
    age = as.period(interval(start = birth_date, end = race_date)),
    age_year = year(age)) |> 
  drop_na(horse_id, age_year) |> 
  group_by(horse_id, age_year, race_year) |> 
  summarize(n_races = n(), .groups = "keep")


#### visualizing our data ####

participation_data_model |> 
  ggplot(aes(y=n_races, x = age_year, color = as.factor(race_year))) +
  geom_point()+
  guides(color = guide_legend(title = "Race Year"))+
  theme_minimal()

#### creating the model ####

library(MASS)

nb_model <- glm.nb(n_races ~ age_year + race_year, data = participation_data_model)


summary(nb_model)

#### using MASS package to get standardized residuals ####

model_resids <- MASS::stdres(nb_model) |> 
  as_tibble() |>
  rename(.std.resid = value) |>
  cbind(participation_data_model)

#### visualizing standardized residuals #####

model_resids|> 
  ggplot(aes(x=.std.resid)) +
  geom_histogram()+
  scale_x_continuous(breaks = seq(-2,4,0.5))+
  scale_y_continuous(breaks = seq(0,300,50))+
  theme_minimal()

## looks skew right but mean ~0 and sd ~1. So, should roughly follow expected distribution
## we then extract only horses with standardized residual more than 1 standard deviation below the mean

extreme_resid_horses <- model_resids |>
  filter(.std.resid < -1)
  
#### Outputting the CSV #### 

write_csv(extreme_resid_horses, "outputted-csvs/extreme_resid_horses.csv")

  

