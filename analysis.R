# Any data analysis will be written in this R script file and sourced into the R markdown file. 
# This file is named "analysis.R"
library(dplyr)

# Temporary assignment for testing - remove before submitting
# Both are dataframes
drug_poisoning <- read.csv("data/drug_poisoning.csv", stringsAsFactors = FALSE)
excess_deaths <- read.csv("data/excess_deaths.csv", stringsAsFactors = FALSE)

filtered_excess_deaths <- excess_deaths %>%
  filter(Locality == "All", Age.Range == "0-84", Benchmark == "Floating") %>%
  select(-State.FIPS.Code, -HHS.Region, -Locality, -Age.Range, -Benchmark) %>%
  group_by(State) %>%
  filter(Percent.Potentially.Excess.Deaths == max(Percent.Potentially.Excess.Deaths))

View(filtered_excess_deaths)