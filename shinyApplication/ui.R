library(shiny)  

shinyUI(pageWithSidebar( 
    
    headerPanel("English Language Text Predictor"),
    
    sidebarPanel(
        fluidRow(conditionalPanel(
            condition = "output.serverStatus == 'Text predictor initialized'",
            submitButton(text = "Predict Word",
                         icon = NULL)))),

    #http://stackoverflow.com/questions/17930985/conditional-output-shiny-ui
    mainPanel(
        conditionalPanel(condition = paste("output.serverStatus !=",
                                           "'Text predictor initialized'"),
                         div(class="initializationStatus"), checked=NA,
                         h3("Text predictor initialization in progress")),
        conditionalPanel(condition = paste("output.serverStatus !=",
                                           "'Text predictor initialized'"),
                         textOutput("serverStatus")),
        textInput("currentPhrase", "Enter Phrase", value = ""),
        tabsetPanel(
            tabPanel("Suggested Terms",
                     div(class="suggestedTerms", checked=NA,
                         p(paste("Maximum likelihood next word that is computed",
                                 "by a Markov Chain model:"))),
                     conditionalPanel(
                         condition="input.currentPhrase != ''",
                         textOutput('predictedNextWord'))
            )
        )
    )
))
