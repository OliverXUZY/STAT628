library(shiny)
library(shinythemes)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Body fat Calculator"),
  
  # and to specify whether outliers should be included
  sidebarPanel(
    numericInput(
      "num1",
      h3("Put in your Abdomen 2 circumference:"),
      value = NULL
    ), 
    
    selectInput(
      "unit1", 
      "Units:",
      list("cm" = "cm",
           "inch" = "inch")
    ),
    
    helpText("Note: Please enter a correct measurement, 
             our App can only work efficiently for Abdomen 
             ranges from 60cm to 130cm(23 inches ~ 51 inches)"),
    
    numericInput(
      "num2",
      h3("Put in your Weight:"),
      value = NULL
    ),
    
    selectInput(
      "unit2", 
      "Units:",
      list("lbs" = "lbs", 
           "kg" = "kg")
    ),
    helpText("Note: Please enter a correct measurement, 
             our App can only work efficiently for weight
             ranges from 88 lbs to 263 lbs (40kg ~ 120kg)"),
    
    radioButtons("radio", h3("Gender"),
                 choices = list("Male" = "Male", "Female" = "Female"),
                 selected = "Male"),

    actionButton(inputId = "go", label = "Submit")
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h2("Result:"),
    textOutput("bodyfat"),
    textOutput("tips"),
    img(src = 'badger.png', height = 500, width = 350)
  )
))