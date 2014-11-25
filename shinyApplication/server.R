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
    
    predictorInput <- as.character(textInputCorpus[[1]])
}

blackList <- readBlackList("./Terms-to-Block.csv")

load(file="./transitionMatrix.RData")

shinyServer(function(input, output) {
    output$text1 <- renderText({ 
        predictorInput <- preprocessTextInput(input$currentPhrase)
        
        paste("Predictor input", predictorInput)
    })
})