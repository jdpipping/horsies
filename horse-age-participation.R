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
id_participation_data <- participation_data |>
  left_join(horse_names, by = 'horse_name') |>
  mutate(race_year = year(race_date)) |>
  filter(!(race_year == 2022))

#### count number of races per horse at each age (in years) ####
library(lubridate)

age_participation_data <- id_participation_data |>
  mutate(
    age = as.period(interval(start = birth_date, end = race_date)),
    age_year = year(age)) |> 
  drop_na(horse_id, age_year) |> 
  group_by(horse_id, age_year) |> 
  summarize(n_races = n(), .groups = "keep") |> 
  mutate(age_year_factor = as.factor(age_year))


#### visualizing our data ####

age_participation_data |> 
  ggplot(aes(y=n_races, x = age_year, color = as.factor(horse_id))) +
  geom_point()+
  theme_minimal()+
  theme(legend.position = "none")

age_participation_data |> 
  ggplot(aes(y=n_races, x = age_year_factor, color = as.factor(horse_id))) +
  geom_point()+
  theme_minimal()+
  theme(legend.position = "none")

#### creating the model ####
library(MASS)

poisson_model <- glm(n_races ~ age_year, 
    data= age_participation_data, family="poisson")

poisson_model_factored <- glm(n_races ~ age_year_factor, 
                     data= age_participation_data, family="poisson")

nb_model <- glm.nb(n_races ~ age_year, data = age_participation_data)


summary(poisson_model)
summary(poisson_model_factored)
summary(nb_model)

#### checking for overdispersion ####

library(AER)

AER::dispersiontest(poisson_model)

## from AER's dispersion test, we concluded that we have have overdispersion
## in turn, we will fit a quasi-poisson model.

poisson_model <- glm(n_races ~ age_year, 
                     data= age_participation_data, family="quasipoisson")

summary(poisson_model)

#### injury data: where are injured horses located ####

resids <- nb_model |> 
  augment()

injured_horses_resids <- age_participation_data |>
  filter(horse_id %in% horse_profiles_injured$horse_id) |>
  left_join(resids, by = c("n_races", "age_year")) |> 
  group_by(horse_id, n_races, age_year) |> 
  slice_head() |> 
  ungroup()

### visualizing residuals ####

nb_model |> 
  augment() |> 
  ggplot(aes(x=.std.resid)) +
  geom_histogram()+
  scale_x_continuous(breaks = seq(-2,4,0.5))+
  scale_y_continuous(breaks = seq(0,300,50))+
  theme_minimal()

library(gghighlight)

nb_model |> 
  augment() |> 
  ggplot(aes(x=.fitted, y=.std.resid)) +
  geom_point()+
  gghighlight(abs(.std.resid) > 1.645)+
  theme_minimal()

extreme_resid <- nb_model |> 
  augment() |>
  filter(abs(.std.resid) > 1.645)

extreme_resid_horses <- age_participation_data |> 
  right_join(extreme_resid, by = c("n_races", "age_year")) |> 
  group_by(horse_id, n_races, age_year) |> 
  slice_head() |> 
  ungroup()

#### Outputting the CSV #### 

write_csv(extreme_resid_horses, "outputted-csvs/extreme_resid_horses.csv")

  

