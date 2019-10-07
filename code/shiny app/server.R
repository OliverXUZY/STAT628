library(shiny)


shinyServer(function(input, output){
  data <- eventReactive(input$go, {
    
    # justify whether the values are proper
    # Then compute the value
    if (input$unit1 == "cm"){
      if (input$num1 < 60 | input$num1 > 130){
        "Your input Abdomen value is not proper, we cannot provide with a correct estimation of your body fat."
      }else if (input$unit2 == "lbs"){
        if (input$num2 < 88 | input$num2 > 263){
          "Your input weight value is not proper, we cannot provide with a correct estimation of your body fat."
        }else{
          (input$num1 * 0.89  + input$num2 * (-0.12) - 41.91) 
        }
      }else if (input$unit2 == "kg"){
        if (input$num2 < 40 | input$num2 > 120){
          paste("Your input weight value is not proper, we cannot provide with a correct estimation of your body fat.")
        }else{
          (input$num1 * 0.89 + input$num2 * (-0.12) / 0.4536 - 41.91070) 
        }
      }
    }else if(input$num1 < 23 | input$num1 > 51){
      "Your input Abdomen value is not proper, we cannot provide with a correct estimation of your body fat."
    }else if (input$unit2 == "lbs"){
      if (input$num2 < 88 | input$num2 > 263){
        "Your input Wrist value is not proper, we cannot provide with a correct estimation of your body fat."
      }else{
        (input$num1 * 0.89 * 2.54  + input$num2 * (-0.12) - 41.91) 
      }
    }else if (input$unit2 == "kg"){
      if (input$num2 < 40 | input$num2 > 120){
        paste("Your input Wrist value is not proper, we cannot provide with a correct estimation of your body fat.")
      }else{
        (input$num1 * 0.89 * 2.54 + input$num2 * (-0.12) / 0.45 - 41.91) 
      }
    }
  })

  output$bodyfat <- renderText({
    paste(data())
  })
  
  output$tips <- renderText({
    a <- data()
    if (input$unit1 == "inch"){
      a1 <- input$num1 * 2.54
    }else{
      a1 <- input$num1
    }
    if (input$unit2 == "kg"){
      a2 <- input$num2 * 2.205
    }else{
      a2 <- input$num2
    }
    if (a1 >= 60 & a1 <= 130 & a2 >= 88 & a2 <= 263){
      if (input$radio == "Male"){
        if(a <= 3){
          paste("It may not be a proper estimate of your bodyfat.")
        }else if (a <= 5){
          paste("You belong to 'Essential fat' group. Your health are really not good. We recommend you to eat more, now!")
        }else if(a > 5 & a <= 13){
          paste("You belong to 'Atheletes' group. Your health are negatively affected. ")
        }else if(a > 13 & a <= 17){
          paste("You belong to 'Fitness' group. Keep it, my friends!")
        }else if(a > 17 & a <= 24){
          paste("You belong to 'Average' group. Keep it, my friends!")
        }else if(a >= 24 ){
          paste("You belong to 'Obese' group. Go to exercise, buddy!")
        }
      }else{
        if(a <= 3){
          paste("It may not be a proper estimate of your bodyfat.")
        }else if (a <= 13){
          paste("You belong to 'Essential fat' group. Your health are really not good. We recommend you to eat more, now!")
        }else if(a > 13 & a <= 20){
          paste("You belong to 'Atheletes' group. Your health are negatively affected.")
        }else if(a > 20 & a <= 24){
          paste("You belong to 'Fitness' group. Keep it, my friends!")
        }else if(a > 25 & a <= 31){
          paste("You belong to 'Average' group.  Keep it, my friends!")
        }else if(a >= 31){
          paste("You belong to 'Obese' group. Go to exercise, buddy!")
        }
      }
    }
  })
})