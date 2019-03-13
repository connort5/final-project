library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

source("analysis.R")

my_server <- function(input, output) {
  
  #############
  ##  JAMIE  ##
  #############
  
  output$injury_poison_plot <- renderPlot({
    
    poisoning_deaths <- poisoning_2005_2015 %>% 
      filter(Age.Group == input$age_range) %>% 
      select(Year, Deaths, Population) %>% 
      mutate(percent_death = Deaths / Population) %>% 
      group_by(Year) %>% 
      summarise(mean_death_poison = mean(percent_death, na.rm = TRUE))
   
   
    poisoning_and_injury <- inner_join(poisoning_deaths, unintentional_death_rates, by = "Year")
   
    poison_injury_plot <- ggplot(data = poisoning_and_injury) +
       geom_point(mapping = aes(x = mean_death_poison, y = mean_death_injury)) +
       geom_smooth(mapping = aes(x = mean_death_poison, y = mean_death_injury), color = "black", alpha = 0.6)+
       theme(plot.background = element_rect(fill = "white", colour = "#afd5d8")) +
       labs(title = "Poisoning Deaths vs. Deaths from Unintentional Injury", x = "Percent Death by Poisoning", y = "Percent Death by Unintentional Injury")
   
   poison_injury_plot
  })
  
  output$injury_poison_explaination <- renderText({
    paste("The above plot displays the Average Percent Death by Drug Poisoning in people of (the age)", input$age_range,  
          "against the Average Percent Death by Unintentional Injury (all ages). Each dot represents a particular year from 2005 - 2015.
          In general (plotting with all ages), there is a positive correlation between drug poisoning and unintentional injury. 
          There is a trend that when poisoning deaths are higher, unintentional deaths are generally higher also. However, when focusing
          on the extreme ends of age (0-14) or 75+, we see the correlation weaken significantly or dissappear altogether. This tells us that
          the correlation between unintentional injury and drug poisoning are not as present within young children and the elderly, which may
          be explained by the fact that these age groups are more likely to have somone watching or caring for them.")
  })

  ############
  ## Connor ##
  ############
  output$connor_plot <- renderPlot({
    
    earlier_year <- input$slide_key[1]
    later_year <- input$slide_key[2] 
    
    excess_death_sort_by_earlier_year <- filter(excess_death_sort_by_year, Year == earlier_year)
    excess_death_sort_by_later_year <- filter(excess_death_sort_by_year, Year == later_year)
    excess_death_sort_by_earlier_year <- na.omit(select(excess_death_sort_by_earlier_year, State, Year, Potentially.Excess.Deaths))
    excess_death_sort_by_later_year <- na.omit(select(excess_death_sort_by_later_year, State, Year, Potentially.Excess.Deaths))
    combined_death_data <- merge(excess_death_sort_by_earlier_year, excess_death_sort_by_later_year, by = "State")
    combined_death_data <- mutate(combined_death_data, percent_change = round((Potentially.Excess.Deaths.y / Potentially.Excess.Deaths.x) * 100, digits = 2))
    
    drug_sort_by_earlier_year <- filter(sorted_drug_data, Year == earlier_year, State != "United States")
    drug_sort_by_later_year <- filter(sorted_drug_data, Year == later_year, State != "United States")
    combined_drug_data <- merge(drug_sort_by_earlier_year, drug_sort_by_later_year, by = "State")
    combined_drug_data <- mutate(combined_drug_data, percent_change = round((Deaths.y / Deaths.x) * 100, digits = 2))
    combined_drug_data <- select(combined_drug_data, State, Year.x, Year.y, Deaths.x, Deaths.y, percent_change)
    combined_data <- merge(combined_death_data, combined_drug_data, by = "State") 
    combined_data <- mutate(combined_data, bigger_drug_increase = percent_change.y > percent_change.x, region = tolower(State))
    combined_data_with_states <- right_join(united_states_data, combined_data)
   
     ggplot() + 
      geom_polygon(data = combined_data_with_states, aes(x = long, y = lat, group = group, fill = bigger_drug_increase))+ 
      scale_fill_manual(name = "Legend", labels = c("TRUE" = "Drugs", "FALSE" = "Cancer"), values = c("TRUE" = "blueviolet", "FALSE" = "pink"))+
      coord_quickmap()
  })
  
  #############
  ## Sangwon ##
  #############
  
  output$CLRD_poison_plot <- renderPlot({
    
    filtered_drug_poisoning <- filter_drug_poisoning(drug_poisoning) %>%
      filter(Year >= 2005, Year <= 2015) %>%
      select(Year, Deaths) %>%
      group_by(Year) %>%
      summarize("Drug Poisoning" = sum(Deaths))
    
    filtered_CLRD <- filter_excess_deaths(excess_deaths) %>%
      select(Year, Cause.of.Death, Observed.Deaths) %>%
      group_by(Year, Cause.of.Death) %>%
      summarize(Deaths = sum(Observed.Deaths)) %>%
      spread(
        key = Cause.of.Death,
        value = Deaths
      ) %>%
      # select(-"Cancer", -"Heart Disease") %>%
      left_join(filtered_drug_poisoning, by = "Year") %>%
      ungroup(Year)
    
    colnames(filtered_CLRD) <- c("Year", "Cancer", "Chronic_Lower_Respiratory_Disease", "Heart_Disease", "Stroke", "Unintentional_Injury", "Drug_Poisoning")
    
    death_plot <- ggplot(data = filtered_CLRD) +
      geom_point(mapping = aes_string(x = "Drug_Poisoning", y = input$cause_select)) +
      labs(title = "Deaths by Poisoning vs. Deaths by Other Causes", x = "Deaths by Poisoning", y = paste("Deaths by", str_replace(input$cause_select, "_", " ")))
    
    death_plot
  })

#############
## Sangwon ##
#############

output$CLRD_poison_plot <- renderPlot({
  
  filtered_drug_poisoning <- filter_drug_poisoning(drug_poisoning) %>%
    filter(Year >= 2005, Year <= 2015) %>%
    select(Year, Deaths) %>%
    group_by(Year) %>%
    summarize("Drug Poisoning" = sum(Deaths))
  
  filtered_CLRD <- filter_excess_deaths(excess_deaths) %>%
    select(Year, Cause.of.Death, Observed.Deaths) %>%
    group_by(Year, Cause.of.Death) %>%
    summarize(Deaths = sum(Observed.Deaths)) %>%
    spread(
      key = Cause.of.Death,
      value = Deaths
    ) %>%
    # select(-"Cancer", -"Heart Disease") %>%
    left_join(filtered_drug_poisoning, by = "Year") %>%
    ungroup(Year)
  
  colnames(filtered_CLRD) <- c("Year", "Cancer", "Chronic_Lower_Respiratory_Disease", "Heart_Disease", "Stroke", "Unintentional_Injury", "Drug_Poisoning")
  
  death_plot <- ggplot(data = filtered_CLRD) +
    geom_point(mapping = aes_string(x = "Drug_Poisoning", y = input$cause_select)) +
    labs(title = "Deaths by Poisoning vs. Deaths by Other Causes", x = "Deaths by Poisoning", y = paste("Deaths by", str_replace(input$cause_select, "_", " ")))
  
  death_plot
})
#############
## Sangwon ##
#############

output$CLRD_poison_plot <- renderPlot({
  
  filtered_drug_poisoning <- filter_drug_poisoning(drug_poisoning) %>%
    filter(Year >= 2005, Year <= 2015) %>%
    select(Year, Deaths) %>%
    group_by(Year) %>%
    summarize("Drug Poisoning" = sum(Deaths))
  
  filtered_CLRD <- filter_excess_deaths(excess_deaths) %>%
    select(Year, Cause.of.Death, Observed.Deaths) %>%
    group_by(Year, Cause.of.Death) %>%
    summarize(Deaths = sum(Observed.Deaths)) %>%
    spread(
      key = Cause.of.Death,
      value = Deaths
    ) %>%
    # select(-"Cancer", -"Heart Disease") %>%
    left_join(filtered_drug_poisoning, by = "Year") %>%
    ungroup(Year)
  
  colnames(filtered_CLRD) <- c("Year", "Cancer", "Chronic_Lower_Respiratory_Disease", "Heart_Disease", "Stroke", "Unintentional_Injury", "Drug_Poisoning")
  
  death_plot <- ggplot(data = filtered_CLRD) +
    geom_point(mapping = aes_string(x = "Drug_Poisoning", y = input$cause_select)) +
    labs(title = "Deaths by Poisoning vs. Deaths by Other Causes", x = "Deaths by Poisoning", y = paste("Deaths by", str_replace(input$cause_select, "_", " ")))
  
  death_plot
})

##########
## Esha ##
##########

  output$Cancer_plot <- renderPlot({
    
    filtered_drug_poisoning <- filter_drug_poisoning(drug_poisoning) %>%
      filter(Year >= 2005, Year <= 2015) %>%
      select(Year, Deaths) %>%
      group_by(Year) %>%
      summarize("Cancer" = sum(Deaths))
    
    filtered_CLRD <- filter_excess_deaths(excess_deaths) %>%
      select(Year, Cause.of.Death, Observed.Deaths) %>%
      group_by(Year, Cause.of.Death) %>%
      summarize(Deaths = sum(Observed.Deaths)) %>%
      spread(
        key = Cause.of.Death,
        value = Deaths
      ) %>%
      # select(-"Cancer", -"Heart Disease") %>%
      left_join(filtered_drug_poisoning, by = "Year") %>%
      ungroup(Year)
    
    colnames(filtered_CLRD) <- c("Year", "Cancer")
    
    death_plot <- ggplot(data = filtered_CLRD) +
      geom_point(mapping = aes_string(x = "Cancer", y = input$cause_select)) +
      labs(title = "Deaths by Poisoning vs. Deaths by Cancer", x = "Deaths by Poisoning", y = paste("Deaths by Cancer"))
    
    death_plot
  })
  
  output$explaination <- renderText({
    paste("The plot above compares the death rate from Fatal Poisoning to the leading cause of death, which is Cancer.")
  })
}
