#### load tidyverse, horse names, participation data ####
library(tidyverse)

horse_names <- read_csv("horse-cooking-data/horse_names.csv")
participation_data <- read_csv("horse-cooking-data/horses_3years_racing_NYRA.csv") |>
  rename(horse_name = `horse name`,
         birth_date = `birth date`,
         race_date = `race date`) |>
  drop_na(horse_name)

#### append horse names to participation data ####
id_participation_data <- participation_data |>
  left_join(horse_names, by = 'horse_name')

#### count number of races per horse at each age (in years) ####
library(lubridate)

age_participation_data <- id_participation_data |>
  mutate(
    age = as.period(interval(start = birth_date, end = race_date)),
    age_year = year(age)) %>% 
  drop_na(horse_id, age_year) %>% 
  group_by(horse_id, age_year) %>% 
  summarize(n_races = n(), .groups = "keep") %>% 
  mutate(age_year_factor = as.factor(age_year))


#### visualizing our data ####

age_participation_data %>% 
  ggplot(aes(y=n_races, x = age_year, color = as.factor(horse_id))) +
  geom_point()+
  theme_minimal()+
  theme(legend.position = "none")

age_participation_data %>% 
  ggplot(aes(y=n_races, x = age_year_factor, color = as.factor(horse_id))) +
  geom_point()+
  theme_minimal()+
  theme(legend.position = "none")

#### creating poisson model ####

poisson_model <- glm(n_races ~ age_year, 
    data= age_participation_data, family="poisson")

poisson_model_factored <- glm(n_races ~ age_year_factor, 
                     data= age_participation_data, family="poisson")

summary(poisson_model)
summary(poisson_model_factored)

#### checking for overdispersion ####

library(AER)

AER::dispersiontest(poisson_model)

## from AER's dispersion test, we concluded that we have have overdispersion
## in turn, we will fit a quasi-poisson model.

poisson_model <- glm(n_races ~ age_year, 
                     data= age_participation_data, family="quasipoisson")

summary(poisson_model)

### visualizing residuals ####


poisson_model %>% 
  augment() %>% 
  ggplot(aes(x=.std.resid)) +
  geom_histogram()+
  scale_x_continuous(breaks = seq(-2,4,0.5))+
  scale_y_continuous(breaks = seq(0,300,50))+
  theme_minimal()

library(gghighlight)

poisson_model %>% 
  augment() %>% 
  ggplot(aes(x=.fitted, y=.std.resid)) +
  geom_point()+
  gghighlight(.std.resid < -1.645 | .std.resid > 1.645)+
  theme_minimal()

extreme_resid <- poisson_model %>% 
  augment() %>%
  filter(.std.resid < -1.645 | .std.resid > 1.645)

extreme_resid_horses <- age_participation_data %>% 
  right_join(extreme_resid, by = c("n_races", "age_year")) %>% 
  group_by(horse_id, n_races, age_year) %>% 
  slice_head() %>% 
  ungroup()


