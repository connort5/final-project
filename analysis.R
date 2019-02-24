# Any data analysis will be written in this R script file and sourced into the R markdown file. 
# This file is named "analysis.R"
library(dplyr)

# Temporary assignment for testing - remove before submitting
# Both are dataframes
drug_poisoning <- read.csv("data/drug_poisoning.csv", stringsAsFactors = FALSE)
excess_deaths <- read.csv("data/excess_deaths.csv", stringsAsFactors = FALSE)

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