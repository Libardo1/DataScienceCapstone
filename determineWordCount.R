countTextFileWords <- function(textFilePath,
                               textFile,
                               num_lines) {
    library(stringi)
    library(stringr)
    #--------------------------------------------------------------------------
    # Counts the number of words in a text file
    #
    # Args:
    #   textFilePath: String that stores the path to a text file
    #
    #   textFile: String that stores the name of a text file
    #
    #   num_lines: List that stores the number of text file lines
    #
    # Returns:
    #   wordCount: Number of words in a text document
    #--------------------------------------------------------------------------
    
    lines_to_read = min(ceiling(num_lines[[textFile]]/10), 10000)
    
    # http://stackoverflow.com/questions/15532810/reading-40-gb-csv-file-into-r-using-bigmemory?lq=1
    # http://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
    # http://www.r-bloggers.com/counting-the-number-of-words-in-a-latex-file-with-stringi/
    h_conn <- file(file.path(textFilePath, textFile), "r", blocking=FALSE)
    cur_chunk <- readLines(h_conn, lines_to_read, skipNul=TRUE)
    firstChunk <- TRUE
    
    wordCount <- 0
    
    repeat {
        if (length(cur_chunk) == 0) {
            break
        }
        else {
            cur_chunk <- str_trim(iconv(cur_chunk,"latin1","ASCII",sub=""))
            
            cur_stats <- as.data.frame(t(stri_stats_latex(cur_chunk)))
            
            wordCount <- wordCount + cur_stats$Words
        }
        cur_chunk <- readLines(h_conn, lines_to_read, skipNul=TRUE)
    }
    close(h_conn)
    
    return(wordCount)
}

determineWordCount <- function(textFilePath,
                               outputDataPath,
                               num_lines) {
    #--------------------------------------------------------------------
    # Determines the number of wrods in each text file contained in a 
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
    #   num_lines: List that stores the number of text file lines
    #
    # Returns:
    #   None
    #--------------------------------------------------------------------
    outputFilePath <- 
        file.path(outputDataPath,
                  paste0(basename(textFilePath), "NumWords.RData"))
    
    if (!file.exists(outputFilePath)) {
        num_words <- list()
        
        for(curTextFile in dir(textFilePath, pattern="(.)*.txt")) {
            num_words[[curTextFile]] <- countTextFileWords(textFilePath,
                                                           curTextFile,
                                                           num_lines)
        }
        save(file=outputFilePath, num_words)
    }
}
