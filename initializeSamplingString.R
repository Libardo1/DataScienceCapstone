initializeSamplingString <- function(percentageToSample) {
    #--------------------------------------------------------------------
    # Initializes a string that identifies the percentage of a text file
    # that was randomly sampled
    #
    # Args:
    #   percentageToSample: Percentage of text file to randomly sample
    #
    # Returns:
    #   samplingStr: String that identifies the percentage of a text file
    #                that was randomly sampled
    #--------------------------------------------------------------------
    samplingStr = gsub("\\.","p",sprintf("%.2fPercent", percentageToSample))
    
    return(samplingStr)
}