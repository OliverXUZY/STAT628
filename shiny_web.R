library(shiny)

# ui ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel("Bodyfat Calculator"),
  
  sidebarLayout(
    # Input weight
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
      
      submitButton("Submit")
    ),
    mainPanel(
      h2("Result:")
    )
  )
)


# Server ------------------------------------------------------------------


server <- function(input, output){
  l <- input$num1
  w <- input$num2
  unit1 <- input$unit1
  unit2 <- input$unit2
  
  if (unit1 == "lb"){
    
  }else if(unit1 == "kg"){
    
  }
}

shinyApp(ui = ui, server = server)
