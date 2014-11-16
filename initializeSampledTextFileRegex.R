initializeSampledTextFileRegex <- function(percentageToSample) {
    #--------------------------------------------------------------------
    # Initializes a regular expression to find a set of text files
    # that were sampled with the input percentage to sample
    #
    # Args:
    #   percentageToSample: Text file sampling percentage 
    #
    # Returns:
    #   sampledTextFileRegex: regular expression to find a set of text 
    #   files that were sampled with the input percentage to sample
    #--------------------------------------------------------------------
    samplingStr <- initializeSamplingString(percentageToSample)
    
    sampledTextFileRegex <- paste0(".*",samplingStr,".*\\.txt")
    
    return(sampledTextFileRegex)
}