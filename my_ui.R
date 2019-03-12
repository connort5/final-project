library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("rsconnect")
library("maps")
library("stringr")

source("analysis.R")

# Defining Variables to use in interactivity

age_range_choices <- unique(drug_poisoning$Age.Group)
united_states_data <- map_data("state")


# Start of defining UI
my_ui <- fluidPage(
  
  includeCSS("style.css"),
  
  titlePanel("Drug Poisoning Mortality Rates and Excess Deaths from the Five Leading Causes of Death"),
  
  tabsetPanel(type = "tabs",
              
              tabPanel("Intro", 
                       h3("Our Problem Space and Datasets", class = "intro"),
                       p("The two datasets that our group chose to analyze are Drug Poisoning Mortality Rates and Excess Deaths from the Five Leading Causes of Death. This domain is worth analyzing because the two datasets can be compared to figure out whether excess deaths or posioning have a larger impact on the United States mortality rate. Furthermore, by critically assesing the data, we can potentially figure out possible measures that could be taken to best address mortality rates from these causes in the United States that could prevent deaths and lower the mortality rate.
                       To die from drug poisoning means that the cause of death was from an excess of a toxic chemical in the body. There are three main categories in which these deaths fall under: unintentional, suicide, and homocide. There is also a fourth category in which the intent was not clear. 
                       Excess deaths are definied as \"deaths that exceed the numbers that would be expected if the death rates of states with the lowest rates (benchmarks) occurred across all states.\" [data.gov](https://catalog.data.gov/dataset/nchs-potentially-excess-deaths-from-the-five-leading-causes-of-death) The five leading causes of death in this dataset are: Cancer, Chronic Lower Respiratory Disease, Stroke, Heart Disease, and Unintentional Injury.", class = "intro")
              ),
              
              tabPanel("Chronic Lower Respiratory Disease",
                       h3(),
                       p(),
                       sliderInput(inputId = "year_input", label = "Pick a Year Range", min = 2005, max = 2005, value = c(2005, 2015)),
                       plotOutput(outputId = "CLRD_poison_plot"),
                       p()
              ),
              
              tabPanel("Cancer & Drug Overdose", 
                       h3("Which percentage of deaths to population (drug overdose or potentially excess deaths due to cancer) has seen a bigger increase between the years of 2005-2015 in the United States?"), 
                       p("By comparing the rates of deaths caused by drug overdoses and potentially excess deaths due to cancer, it is apparent that cancer is responsible for more deaths yearly in the United States. However, when we compare the percent change of drug overdoses to the percent change in potentially excess deaths due to cancer over time, the results are surprising. Law makers could use this information to assist in making decisions about what measures could be taken to save the most lives. "),
                       sliderInput(inputId = "slide_key", label = "Pick a Year Range", min = 2005, max = 2015, value = c(2005, 2015)), 
                       plotOutput(outputId = "connor_plot"), 
                       p("The map seen above allows the user to pick a range of years, and the states that saw a bigger percent increase in drug overdoses are seen in purple, while the states that appear in pink represent a greater percent increase in potentially excess deaths due to cancer. As you can see in the 2005-2015 data range, all but three of the states saw a higher percent increase in deaths due to drug overdose. This information suggests that drug overdoses have become a greater problem over time compared to potentially excess deaths due to cancer. However, although it seems that drug overdoses are drastically more deadly then potentially excess deaths due to cancer, it is important to keep in mind that the data is based off of percent change. In reality, the number of deaths due to cancer annually far surpasses the amount of drug overdose deaths, but nontheless, lawmakers could find this information useful when addressing drug control policy.  ")
                       ),
              
              
              tabPanel("Injury and Poisoning", 
                       h3("How are the rates of unintentional injury related to the rates of fatal poisoning in the United States from 2005-2015?", class = "question"),
                       p("This question is of interest because by analyzing the unintentional death rates from injury and comparing them to rates of drug poisoning, 
                         we can understand how strongly drug use correlates with deaths from unintentional injury. It is known that drugs inhibit a person's judgement and coordination, 
                         which may lead to injuries. We can analyze the data in the years that they overlap and compare the death rates from unintentional injury to poisoning.", class = "question"),
                       selectInput(inputId = "age_range", label = "Age Range (for poisoning)", 
                                   choices = age_range_choices, selected = "0-84"),
                       plotOutput(outputId = "injury_poison_plot"),
                       textOutput(outputId = "injury_poison_explaination")
                       
              ),
              
              tabPanel("Esha")
  )
)

