#Section 2 Data Description Code
library(dplyr)
drug_data <- read.csv(
  file =
    "data/NCHS_-_Drug_Poisoning_Mortality_by_State__United_States.csv", stringsAsFactors = FALSE
)

excess_death_data <- read.csv(file = "data/NCHS_-_Potentially_Excess_Deaths_from_the_Five_Leading_Causes_of_Death.csv", stringsAsFactors = FALSE)

sorted_drug_data <- select(drug_data, State, Year, Sex, Age.Group, Race.and.Hispanic.Origin, 
                           Deaths, Population, Crude.Death.Rate)

sorted_excess_death_data <- select(excess_death_data, Year, Cause.of.Death, State, Age.Range, 
                                   Benchmark, Locality, Observed.Deaths, Population, Expected.Deaths,
                                   Potentially.Excess.Deaths, Percent.Potentially.Excess.Deaths)  

