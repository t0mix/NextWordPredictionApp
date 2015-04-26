# loading shiny library
library(shiny)

# Most simple UI one can think of
shinyUI(fluidPage(
        headerPanel("Next Word Prediction App"),

        mainPanel(
                hr(),
                p("This app has been built for my Data Science Capstone project 
                  and has been kept as simple as possible. It takes a phrase
                  as input and outputs a prediction of the next word."),    
                hr(),
                textInput(inputId="phrase", label="Type a phrase in the below input text field"),
                p("The predicted next word (for the inputted phrase) is:"),
                verbatimTextOutput("nextword"), 
                hr(),
                p("The modelling approach, algorithm and implementation of 
                  this app is explained on the following presentation hosted 
                  on R Pubs:"),
                a(href="http://rpubs.com/t0mix/76819", "http://rpubs.com/t0mix/76819",target="_blank"),
                hr(),
                p("Author: Thomas Tsang | Date: April 26, 2015"),
                a(href="mailto:thomastsang1979@gmail.com", "[email me]",target="_blank")
        )
))