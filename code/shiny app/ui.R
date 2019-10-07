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
    h2("Result:(%)"),
    tags$style("#bodyfat {font-size:22px; 
       color:Tomato; 
       display:block; }"), 
    tags$style("#tips {font-size:15px; 
       display:block; 
       bottom: 12px; 
       position:absolute; 
       width: 100%; 
       left:0px;}"), 
    div(style="text-align:center; 
     box-shadow: 20px 20px 10px gray; 
     width:300px; 
     height:300px; 
     padding-top:70px; 
     position:relative;", 
        textOutput("bodyfat"), 
        textOutput("tips") 
    ), 
    br(),
    img(src = 'badger.png', height = 300, width = 200),
    br(),
    
    h3("Contact Info:"),
    h5("Zhuoyan Xu (zxu444@wisc.edu)"),
    h5("Zhao Li (zli872@wisc.edu)"),
    h5("Yaobin Ling (yling23@wisc.edu)"),
    h5("Yujie Zhang (zhang2329@wisc.edu)")
  )
))