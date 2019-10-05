library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Body fat Calculator"),
  
  # and to specify whether outliers should be included
  sidebarPanel(
    numericInput(
      "num1",
      h3("Put in your weight:"),
      value = NULL
    ),
    
    selectInput(
      "unit1", 
      "Units:",
      list("lb" = "lb", 
           "kg" = "kg")
    ),
    
    numericInput(
      "num2",
      h3("Put in your:"),
      value = NULL
    ),
    
    selectInput(
      "unit2", 
      "Units:",
      list("m" = "m", 
           "inch" = "inch")
    ),
    
    actionButton(inputId = "go", label = "Submit")
  ),
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h2("Result:"),
    textOutput("bodyfat")
  )
))