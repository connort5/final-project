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
  
  head(summarized_drug_poisoning)
}