# Any data analysis will be written in this R script file and sourced into the R markdown file. 
# This file is named "analysis.R"

library(dplyr)

# Temporary assignment for testing - remove before submitting
# Both are dataframes
excess_deaths <- read.csv("data/excess_deaths.csv", stringsAsFactors = FALSE)
drug_poisoning <- read.csv("data/drug_poisoning.csv", stringsAsFactors = FALSE)

filter_excess_deaths <- function(df){
  filtered_excess_deaths <- df %>%
  filter(Locality == "All", Age.Range == "0-84", Benchmark == "Floating") %>%
  select(-State.FIPS.Code, -HHS.Region, -Locality, -Age.Range, -Benchmark)
  
  filtered_excess_deaths
}

filter_drug_poisoning <- function(df){
  filtered_drug_poisoning <- df %>%
  filter(Sex == "Both Sexes", Age.Group == "All Ages", Race.and.Hispanic.Origin == "All Races-All Origins") %>%
  select(-Sex, -Age.Group, -Race.and.Hispanic.Origin)
  
  filtered_drug_poisoning
}

# Remove before submitting
filtered_excess_deaths <- filter_excess_deaths(excess_deaths)
filtered_drug_poisoning <- filter_drug_poisoning(drug_poisoning)

summarize_excess_deaths <- function(df){
  summarized_excess_deaths <- filter_excess_deaths(df) %>%
  group_by(State) %>%
  summarize(
    mean_observed_deaths = mean(Observed.Deaths),
    min_observed_deaths = min(Observed.Deaths),
    max_observed_deaths = max(Observed.Deaths),
    mean_population = mean(Population),
    min_population = min(Population),
    max_population = max(Population)
  )
  
  head(summarized_excess_deaths)
}

summarize_drug_poisoning <- function(df){
  summarized_drug_poisoning <- filter_drug_poisoning(df) %>%
  group_by(State) %>%
  summarize(
    mean_deaths = mean(Deaths),
    min_deaths = min(Deaths),
    max_deaths = max(Deaths),
    mean_population = mean(Population),
    min_population = min(Population),
    max_population = max(Population)
  )
<<<<<<< HEAD
}
###########################
## Analysis for Questions##
##    (very general)     ##
###########################

# High level analysis for Q1
unintentional_death_rates <- excess_deaths %>% 
  filter(Cause.of.Death == "Unintentional Injury") %>% 
  filter(Age.Range == "0-84") %>% 
  select(Year, Observed.Deaths, Population) %>% 
  mutate(percent_death = Observed.Deaths / Population) %>% 
  group_by(Year) %>% 
  summarise(mean_death = mean(percent_death, na.rm = TRUE))

poisoning_deaths <- drug_poisoning %>% 
  filter(
    Age.Group == "All Ages",
    Sex == "Both Sexes",
    Race.and.Hispanic.Origin == "All Races-All Origins"
    ) %>% 
  select(Year, Deaths, Population) %>% 
  mutate(percent_death = Deaths / Population) %>% 
  group_by(Year) %>% 
  summarise(mean_death = mean(percent_death, na.rm = TRUE))

##############
# Comparison #
##############
# comparing average rate of excess deaths per year and average rate of death by drug poisoning

# average rate of excess deaths per year
excess_death_rate <- excess_deaths %>% 
  filter(
    Age.Range == "0-84",
    Locality == "All", 
    Benchmark == "Floating"
  ) %>% 
  select(Year, Percent.Potentially.Excess.Deaths) %>% 
  group_by(Year) %>% 
  summarise(avg_percent_excess_deaths = mean(Percent.Potentially.Excess.Deaths))

# average rate of drug poisoning deaths per year, already calculated in High level analysis for Q1

######################
## Joining Datasets ##
######################

#finding which state has the highest combined rate of excess death and drug poisoning

avg_excess <- excess_deaths %>% 
  filter(
    Age.Range == "0-84",
    Locality == "All", 
    Benchmark == "Floating"
  ) %>% 
  select(State, Percent.Potentially.Excess.Deaths) %>% 
  group_by(State) %>% 
  summarise(avg_excess_percent = mean(Percent.Potentially.Excess.Deaths))

avg_poison <- drug_poisoning %>% 
  filter(
    Age.Group == "All Ages",
    Sex == "Both Sexes",
    Race.and.Hispanic.Origin == "All Races-All Origins"
  ) %>% 
  select(State, Deaths, Population) %>% 
  mutate(percent_death = (Deaths / Population) * 100) %>% 
  group_by(State) %>% 
  summarise(avg_poison_rate = mean(percent_death))

avg_excess_poison <- left_join(avg_excess, avg_poison, by = "State")
avg_excess_poison$avg_excess_percent <- as.numeric(avg_excess_poison$avg_excess_percent)
avg_excess_poison$avg_poison_rate <- as.numeric(avg_excess_poison$avg_poison_rate)
View(avg_excess_poison)
  
highest_avg <- avg_excess_poison %>% 
  mutate(combined_rates = (avg_excess_percent + avg_poison_rate)) %>% 
  filter(complete.cases(.)) %>% 
  filter(combined_rates == max(combined_rates))
  

  
=======
  
  head(summarized_drug_poisoning)
}
>>>>>>> 60f292e1bda23a269c39ad81eca10d4315b6c517
