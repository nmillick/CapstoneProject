library(shiny)
library(markdown)
library(shinythemes)

## SHINY UI
shinyUI(
  fluidPage(theme = shinytheme("superhero"),
    titlePanel("Predictive Word Application"),
    sidebarLayout(
      sidebarPanel(
        helpText("Pick a word, any word"),
        hr(),
        textInput("inputText", "Enter your selection here.",value = ""),
        hr(),
        helpText("1 - After a word is input, the prediction will appear on the right.", 
                 hr(),
                 "2 - You have to enter a word to get a prediction."),
        hr()
      ),
      mainPanel(
        h2("Here is the next word"),
        verbatimTextOutput("prediction"),
        hr()
      )
    )
  )
)