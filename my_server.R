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
  })
  
  #############
  ## Sangwon ##
  #############
  
  
}
