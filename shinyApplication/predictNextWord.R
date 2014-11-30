predictNextWord <- function(curPhrase,
                            numberOfTerms,
                            textPredictor) {
    #--------------------------------------------------------------------------
    # Predicts the next word of an n-gram using a Markov chain
    #
    # Args:
    #   curPhrase: String that stores an n-gram
    #
    #   numberOfTerms: Number of terms to predict
    #
    #   textPredictor: Markovchain class object
    #
    # Returns:
    #   textPrediction: List that contains prediction(s) of the next term
    #
    #   Keyword:        Description:
    #   -------         -----------
    #   stateHistory    Character vector that stores the markov chain state
    #                   history
    #
    #   textPrediction  Numeric vector that stores the conditional probability
    #                   for the predicted next term(s)
    #--------------------------------------------------------------------------
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
            
            if (length(nextState) > 1) {
                randomIdx <- floor(length(nextState) * runif(1)) + 1
                nextState <- nextState[randomIdx]
            }
        }
        curState <- nextState
        
        textPrediction$stateHistory <- 
            append(textPrediction$stateHistory, curState)
    }
    
    textPrediction$conditionalProbability <- 
        sort(conditionalDistribution(textPredictor, curState),
             decreasing=TRUE)[1:numberOfTerms]
    
    return(textPrediction)
}
