library(shiny)

shinyalert <- function(id, click.hide = TRUE, auto.close.after = NULL) {
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$script(src = "shinysky/shinyalert.js"))), 
                 shiny::HTML(paste0("<div id=\"", id, "\" class=\"shinyalert alert fade\" data-alert=\"alert\" click-hide=\"",as.character(click.hide),"\" data-auto-close-after ='",auto.close.after,"'></div>")))
}

showshinyalert <- function(session, id, HTMLtext, styleclass = "success") {
  alert.css.style = paste("alert", styleclass, sep = "-")
  session$sendCustomMessage("shinyalerthandler", list(id = id, HTMLtext = HTMLtext, alert.css.style = alert.css.style, show=TRUE))
}

hideshinyalert <- function(session, id) {
  session$sendCustomMessage("shinyalerthandler", list(id = id, show=FALSE))
}

shinyServer(function(input, output){
  data <- eventReactive(input$go, {
    # justify whether the values are proper
    # Then compute the value
    if (input$unit1 == "cm"){
      if (input$unit2 == "cm"){
        input$num1 * 0.7245  + input$num2 * (-2.0134) - 11.3414
      }else{
        input$num1 * 0.7245 + input$num2 * (-2.0134) / 0.3937 - 11.3414
      }
    }else{
      if (input$unit2 == "cm"){
        input$num1 * 0.7245 / 0.3937 + input$num2 * (-2.0134) - 11.3414
      }else{
        input$num1 * 0.7245 / 0.3937 + input$num2 * (-2.0134) / 0.3937 - 11.3414
      }
    }
  })
  
  output$bodyfat <- renderText({
    paste("Your body fat is:", data()),
    if (input$unit1 == "")
    })
})