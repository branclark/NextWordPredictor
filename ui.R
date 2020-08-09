#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)


inputTextarea <- function(inputId, value="", nrows, ncols, ph) {
    tagList(
        singleton(tags$head(tags$script(src = "textarea.js"))),
        tags$textarea(id = inputId,
                      class = "inputtextarea",
                      rows = nrows,
                      cols = ncols,
                      placeholder = ph,
                      as.character(value))
    )
}

# Define UI for application that draws a histogram 
shinyUI(fixedPage(
    
    #theme = "bootstrap.min.css",
    setBackgroundColor("DarkSlateGray"),


    # Application title
    titlePanel(h1("Predict your next word", style = 'color:white')),
    
    
    
    inputTextarea("userText", "", 8, 45, "Type your text here"),
    
    h3("Predicted words:",style = 'color:white'),
    

    
    fixedRow(column(6,
                    condition = "input.numPred == 3",
                                     actionButton("word1", label = textOutput("word1_label"),
                                                  class="btn btn-info"),
                                     actionButton("word2", label = textOutput("word2_label"),
                                                  class="btn btn-info"),
                                     actionButton("word3", label = textOutput("word3_label"),
                                                  class="btn btn-info"))
    ),
    
    h6("Please click on a word to populate the input text area",style = 'color:white'),
    
    h6(''),
    
    h6(''),
    
    h6(''),
    
    h6( em('*initial prediction results may take up to 30 seconds to load'),style = 'color:white' )
))
