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
  
  if (input$unit1 == "lb"){
    if (input$unit2 == "inch"){
      output$bodyfat <- 1 * input$num1 + 1 * input$num2 
    }else{
      output$bodyfat <- 1 * input$num1 + 1 * input$num2 * 39.3700787 
    }
  }else{
    output$bodyfat <- 1 * input$num1 * 2.20462262 * 1
  }
}

shinyApp(ui = ui, server = server)
