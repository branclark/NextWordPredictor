#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("nextWordPredictor.R")


shinyServer(function(input, output, session) {
    
    values <- reactiveValues()
    values$predictions <- c('','','')
    
    
    observe({
        text <- input$userText
        isolate({
            values$predictions <- nextWordPredictor(text)
        })
    })


    
    output$word1_label <- renderText({
        values$predictions[1]
    })
    output$word2_label <- renderText({
        values$predictions[2]
    })
    output$word3_label <- renderText({
        values$predictions[3]
    })
    
    
    observe({
        if (input$word1 == 0) return()
        isolate({
            updateTextInput(session, "userText",
                            value = paste(input$userText, values$predictions[1]))
        })
    })
    observe({
        if (input$word2 == 0) return()
        isolate({
            updateTextInput(session, "userText",
                            value = paste(input$userText, values$predictions[2]))
        })
    })
    observe({
        if (input$word3 == 0) return()
        isolate({
            updateTextInput(session, "userText",
                            value = paste(input$userText, values$predictions[3]))
        })
    })
    

})
