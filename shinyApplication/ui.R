library(shiny)  

shinyUI(pageWithSidebar( 
    
    headerPanel("English Language Text Predictor"),
    
    sidebarPanel(
        fluidRow(conditionalPanel(
            condition = "output.serverStatus == 'Text predictor initialized'",
            sliderInput("numberOfTerms",
                        label="Number of terms to suggest",
                        min=1,
                        max=5,
                        value=3,
                        step=1),
            submitButton(text = "Apply Changes",
                         icon = NULL)))),

    #http://stackoverflow.com/questions/17930985/conditional-output-shiny-ui
    mainPanel(
        conditionalPanel(condition = "output.serverStatus != 'Text predictor initialized'",
                         div(class="initializationStatus"), checked=NA,
                         h3("Text predictor initialization in progress")),
        conditionalPanel(condition = "output.serverStatus != 'Text predictor initialized'",
                         textOutput("serverStatus")),
        textInput("currentPhrase", "Enter Phrase", value = ""),
        tabsetPanel(
            tabPanel("Suggested Terms",
                     div(class="suggestedTerms", checked=NA,
                         p(paste("This tab displays the requested number of",
                                 "potential next words that is computed by",
                                 "a Markov Chain model"))),
                     conditionalPanel(
                         condition="input.currentPhrase != ''",
                         tableOutput('suggestedTerms'))
            )
        )
    )
))
