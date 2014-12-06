#https://groups.google.com/forum/#!msg/shiny-discuss/IFpkIuPTVRU/P3D4AuNRkUAJ
options(shiny.maxRequestSize=95*1024^2)

library(rJava)
library(RWeka)
library(R.utils)
library(stringi)
library(stringr)
library(shiny)
library(textcat)
library(tm)
library(markovchain)

source("./predictNextWord.R")
source("./readBlackList.R")

blackList <- readBlackList("./Terms-to-Block.csv")

load(file="./transitionMatrix.RData")

textPredictor <- new("markovchain",
                     transitionMatrix=transitionMatrix)
rm(transitionMatrix)

# http://withr.me/blog/2014/01/03/add-calculation-in-process-indictor-for-shiny-application/

shinyServer(function(input, output) {
    output$serverStatus <- renderText({
        "Text predictor initialized"
    })
    
    # http://stackoverflow.com/questions/22251956/
    # r-shiny-how-to-output-a-good-looking-matrix-using-rendertable
    output$predictedNextWord = renderText({
        #http://r.789695.n4.nabble.com/length-of-empty-string-td4083712.html        
        currentPhrase <- preprocessTextInput(input$currentPhrase,
                                             blackList)
        
        if (length(currentPhrase) > 0) {
            textPrediction <- predictNextWord(currentPhrase,
                                              1,
                                              textPredictor)
            
            predictedNextWord <- 
                t(as.matrix(textPrediction$conditionalProbability))
            
            rownames(predictedNextWord) <- "P(term)"

            return(colnames(predictedNextWord)[1])
        }
    })
})
