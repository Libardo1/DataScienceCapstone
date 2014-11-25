library(shiny)  

shinyUI(pageWithSidebar( 
    
    headerPanel("English Language Text Predictor"),
    
    sidebarPanel(
        fluidRow(sliderInput("numberOfTerms",
                             label="Number of terms to suggest",
                             min=1,
                             max=5,
                             value=3,
                             step=1),
        submitButton(text = "Apply Changes", icon = NULL)
    )),
    
    mainPanel(
        textInput("currentPhrase", "Enter Phrase", value = ""),
        textOutput("text1")
    )
))
