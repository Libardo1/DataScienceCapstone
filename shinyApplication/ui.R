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
        tabsetPanel(
            tabPanel("Suggested Terms",
                     div(class="suggestedTerms", checked=NA,
                         p(paste("This tab displays the requested number of",
                                 "potential next words that is computed by",
                                 "a Markov Chain model"))),
                     tableOutput('suggestedTerms')
            )
        )
    )
))
