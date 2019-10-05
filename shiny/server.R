library(shiny)

shinyServer(function(input, output) {
  data <- eventReactive(input$go, {
    # justify whether the values are proper
    # Then compute the value
    if (input$unit1 == "lb"){
      if (input$unit2 == "inch"){
        input$num1 * 1 + input$num2 * 1
      }else{
        input$num1 * 1 + input$num2 * 1 * 39.3700787
      }
    }else{
      if (input$unit2 == "inch"){
        input$num1 * 1 * 2.20462262 + input$num2 * 1
      }else{
        input$num1 * 1 * 2.20462262 + input$num2 * 1 * 39.3700787
      }
    }
  })
  output$bodyfat <- renderText({paste("Your body fat is:", data())})
})