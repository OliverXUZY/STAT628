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
             ranges from 60cm to 130cm(23.62 inches ~ 51.181 inches)"),
    
    numericInput(
      "num2",
      h3("Put in your Wrist circumference:"),
      value = NULL
    ),
    
    selectInput(
      "unit2", 
      "Units:",
      list("cm" = "cm", 
           "inch" = "inch")
    ),
    helpText("Note: Please enter a correct measurement, 
             our App can only work efficiently for Abdomen 
             ranges from 15cm to 22cm(5.9055 inches ~ 8.6614 inches)"),
    
    actionButton(inputId = "go", label = "Submit")
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h2("Result:"),
    textOutput("bodyfat"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    img(src = "college2.png", height = 90, width = 200)
  )
))