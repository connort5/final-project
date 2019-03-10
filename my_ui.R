library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("rsconnect")

source("analysis.R")

# Defining Variables to use in interactivity

age_range_choices <- unique(drug_poisoning$Age.Group)





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
              
              tabPanel("Sangwon"),
              
              tabPanel("Connor", 
                       sliderInput(inputId = "slide_key", label = "Pick a Year(s)", min = 1992, max = 2016, value = c(1992, 2016))
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

