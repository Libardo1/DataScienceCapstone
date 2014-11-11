determineTextFileSize <- function(textFilePath,
                                  outputDataPath) {    
    #--------------------------------------------------------------------
    # Determines the number of lines in each text file contained in a 
    # directory and stores it in a list. The resulting list is written
    # to an *.RData file in the current working directory whose prefix
    # is set to the name of the input text file directory.
    #
    # Args:
    #   textFilePath: Full path to a directory that contains text files
    #
    #   outputDataPath: Full path to a directory that stores processing 
    #                   outputs
    #
    # Returns:
    #   None
    #--------------------------------------------------------------------
    outputFilePath <- 
        file.path(outputDataPath,
                  paste0(basename(textFilePath), "NumLines.RData"))

    if (!file.exists(outputFilePath)) {
        num_lines <- list()
        
        for(curTextFile in dir(textFilePath, pattern="(.)*.txt")) {
            # http://www.inside-r.org/packages/cran/R.utils/docs/countLines
            h_conn <- file(file.path(textFilePath, curTextFile), "rb")
            num_lines[[curTextFile]] <- countLines(h_conn)
            close(h_conn)
        }
        save(file=outputFilePath, num_lines)
    }
}