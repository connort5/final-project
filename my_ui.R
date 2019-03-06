library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("rsconnect")

my_ui <- fluidPage(
  titlePanel("Drug Poisoning Mortality Rates and Excess Deaths from the Five Leading Causes of Death"),
  
  p("The two datasets that our group chose to analyze are Drug Poisoning Mortality Rates and Excess Deaths from the Five Leading Causes of Death. This domain is worth analyzing because the two datasets can be compared to figure out whether excess deaths or posioning have a larger impact on the United States mortality rate. Furthermore, by critically assesing the data, we can potentially figure out possible measures that could be taken to best address mortality rates from these causes in the United States that could prevent deaths and lower the mortality rate.

    To die from drug poisoning means that the cause of death was from an excess of a toxic chemical in the body. There are three main categories in which these deaths fall under: unintentional, suicide, and homocide. There is also a fourth category in which the intent was not clear. 
    
    Excess deaths are definied as \"deaths that exceed the numbers that would be expected if the death rates of states with the lowest rates (benchmarks) occurred across all states.\" [data.gov](https://catalog.data.gov/dataset/nchs-potentially-excess-deaths-from-the-five-leading-causes-of-death) The five leading causes of death in this dataset are: Cancer, Chronic Lower Respiratory Disease, Stroke, Heart Disease, and Unintentional Injury."),
  sidebarLayout(
    
    sidebarPanel(
      
      
      
      
      
    ),
    
    mainPanel(
      
      
    )
    
  )
)