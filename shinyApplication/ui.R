library(shiny)  

shinyUI(pageWithSidebar( 
    
    headerPanel("English Language Text Predictor"),
    
    sidebarPanel(
        submitButton(text = "Apply Changes", icon = NULL)
    ),
    
    mainPanel(
    )
))
    