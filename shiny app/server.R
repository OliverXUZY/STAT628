library(shiny)


shinyServer(function(input, output){
  data <- eventReactive(input$go, {
    # justify whether the values are proper
    # Then compute the value
    if (input$unit1 == "cm"){
      if (input$num1 < 60 | input$num1 > 130){
        "Your input Abdomen value is not proper, we cannot provide with a correct estimation of your body fat."
      }else if (input$unit2 == "cm"){
        if (input$num2 < 15 | input$num2 > 22){
          "Your input Wrist value is not proper, we cannot provide with a correct estimation of your body fat."
        }else{
          input$num1 * 0.7245  + input$num2 * (-2.0134) - 11.3414
        }
      }else if (input$unit2 == "inch"){
        if (input$num2 < 5.9055 | input$num2 > 8.6614){
          paste("Your input Wrist value is not proper, we cannot provide with a correct estimation of your body fat.")
        }else{
          input$num1 * 0.7245 + input$num2 * (-2.0134) / 0.3937 - 11.3414
        }
      }
    }else if (input$unit2 == "cm"){
      if (input$num2 < 15 | input$num2 > 22){
        "Your input Wrist value is not proper, we cannot provide with a correct estimation of your body fat."
      }else{
        input$num1 * 0.7245 / 0.3937 + input$num2 * (-2.0134) - 11.3414
      }
    }else if (input$unit2 == "inch"){
      if (input$num2 < 5.9055 | input$num2 > 8.6614){
        paste("Your input Wrist value is not proper, we cannot provide with a correct estimation of your body fat.")
      }else{
        input$num1 * 0.7245 / 0.3937 + input$num2 * (-2.0134) / 0.3937 - 11.3414
      }
    }
  })

  output$bodyfat <- renderText({
    paste(data(), "%")
  })
  
  output$tips <- renderText({
    a <- data()
    if (input$radio == "Male"){
      if (a <= 5){
        paste("You belong to 'Essential fat' group.")
      }else if(a > 5 & a <= 13){
        paste("You belong to 'Atheletes' group.")
      }else if(a > 13 & a <= 17){
        paste("You belong to 'Fitness' group.")
      }else if(a > 17 & a <= 24){
        paste("You belong to 'Average' group.")
      }else if(a >= 24){
        paste("You belong to 'Obese' group")
      }
    }else{
      if (a <= 13){
        paste("You belong to 'Essential fat' group.")
      }else if(a > 13 & a <= 20){
        paste("You belong to 'Atheletes' group.")
      }else if(a > 20 & a <= 24){
        paste("You belong to 'Fitness' group.")
      }else if(a > 25 & a <= 31){
        paste("You belong to 'Average' group.")
      }else if(a >= 31){
        paste("You belong to 'Obese' group")
      }
    }
  })
})