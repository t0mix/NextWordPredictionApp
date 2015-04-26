# loading shiny library
library(shiny)
# loading required library, for trim() function
library("gdata")

# loading prediction data
load("freqs.RData")
# sourcing file containing preprocessing and prediction functions
source("model.R")


shinyServer(

        # get the phrase contained in the UI input text field
        # predict the next word
        # set the textOutput with the predicted word
        function(input, output) {               
                output$nextword <- renderPrint({ backOffPredict(input$phrase,freqs) })
        }
        
)


