library(shiny)
ui <- fluidPage(
  titlePanel("Bodyfat Calculator"),
  
  fluidRow(
    column(3,
      numericInput(
        "num1",
        h3("Put in your:"),
        value = NULL
      ),
      
      numericInput(
        "num2",
        h3("Put in your:"),
        value = NULL
      ),
      
      submitButton("Submit")
    )
  ),
  
  mainPanel(
    h2('Result:'),
    h3(textOutput("Your body fat is:"))
  )
)

server <- function(input, output){}
shinyApp(ui = ui, server = server)
