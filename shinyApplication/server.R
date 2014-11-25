library(rJava)
library(RWeka)
library(R.utils)
library(stringi)
library(stringr)
library(shiny)
library(textcat)
library(tm)
library(markovchain)

# http://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
# http://stackoverflow.com/questions/18153504/removing-non-english-text-from-corpus-in-r-using-tm
removeNonASCII <-
    content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))

# http://stackoverflow.com/questions/14281282/
# how-to-write-custom-removepunctuation-function-to-better-deal-with-unicode-cha
#
# http://stackoverflow.com/questions/8697079/remove-all-punctuation-except-apostrophes-in-r
customRemovePunctuation <- content_transformer(function(x) {
    x <- gsub("[[:punct:]]"," ",tolower(x))
    return(x)
})

readBlackList <- function(blackListFile) {
    blackList <- read.csv(blackListFile,header=FALSE,skip=4)
    blackList <- blackList[,2]
    blackList <- gsub(",","",blackList)
    blackList <- gsub(" ","",blackList)
    blackList <- gsub("[0-9]+","",blackList)
    blackList <- gsub("[\\.-]","",blackList)
    blackList <- blackList[!grepl("^a$",blackList)]
    blackList <- unique(blackList[blackList != ""])
    return(blackList)
}

preprocessTextInput <- function(textInput) {
    textInputCorpus <- Corpus(VectorSource(textInput))
    
    textInputCorpus <- tm_map(textInputCorpus, removeNonASCII)
    
    textInputCorpus <- tm_map(textInputCorpus, customRemovePunctuation)
    
    textInputCorpus <- tm_map(textInputCorpus, removeNumbers)
    
    textInputCorpus <- tm_map(textInputCorpus, stripWhitespace)
    
    textInputCorpus <- tm_map(textInputCorpus, removeWords, blackList)
    
    predictorInput <- 
        unlist(str_split(as.character(textInputCorpus[[1]])," "))
}

predictNextWord <- function(curPhrase,
                            textPredictor) {
    textPrediction <- list()
    textPrediction$stateHistory <- character()
    
    numberWords <- length(curPhrase)
    curState <- curPhrase[1]
    vocabulary <- states(textPredictor)
    
    if (!curState %in% vocabulary) {
        randomIdx <- floor(length(vocabulary) * runif(1)) + 1
        curState <- vocabulary[randomIdx]
    }
    
    textPrediction$stateHistory <- 
        append(textPrediction$stateHistory, curState)
    
    for (n in seq(2,numberWords)) {
        nextState <- curPhrase[n]
        if (!nextState %in% vocabulary) {
            curConditionalProbability <- 
                conditionalDistribution(textPredictor, curState)
            
            nextState <- names(which.max(curConditionalProbability))
        }
        curState <- nextState
        
        textPrediction$stateHistory <- 
            append(textPrediction$stateHistory, curState)
    }
    
    textPrediction$conditionalProbability <- 
        sort(conditionalDistribution(textPredictor, curState),
             decreasing=TRUE)[1:4]
    
    return(textPrediction)
}

blackList <- readBlackList("./Terms-to-Block.csv")

load(file="./transitionMatrix.RData")

#textPredictor <- new("markovchain",
#                     transitionMatrix=transitionMatrix)

shinyServer(function(input, output) {
    # http://stackoverflow.com/questions/22251956/
    # r-shiny-how-to-output-a-good-looking-matrix-using-rendertable
    output$suggestedTerms = renderTable({
        currentPhrase <- preprocessTextInput(input$currentPhrase)
        
        print(currentPhrase)
        
        #textPrediction <- predictNextWord(currentPhrase,
        #                                  textPredictor)
        textPrediction <- list()
        textPrediction$conditionalProbability <- c(0.4,0.2,0.2,0.2)
        names(textPrediction$conditionalProbability) <- 
            c("auto","kingdom","remaining","states")
        
        suggestedTerms <- 
            t(as.matrix(textPrediction$conditionalProbability))
        
        rownames(suggestedTerms) <- "P(term)"
        
        return(suggestedTerms)
    })
})
