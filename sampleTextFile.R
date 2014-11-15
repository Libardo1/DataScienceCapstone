sampleTextFile <- function(inputTextFilePath,
                           num_lines,
                           percentageToSample,
                           outputTextFilePath,
                           displayStatus=FALSE) {
    #--------------------------------------------------------------------
    # Generates a random sample of a text file and writes it to disk
    #
    # Args:
    #   inTextFilePath: Full path to the input text file
    #
    #   num_lines: List that stores the number of lines of each text file
    #              contained in a directory
    #
    #   percentageToSample: % of the text file to sample
    #
    #   outTextFilePath: Full path to the output text file that is a 
    #                    random sample of the input text file
    #
    #   displayStatus: Optional Boolean input that controls whether or not
    #                  text document processing status is printed to the 
    #                  status window
    #
    # Returns:
    #   sample_line_idx: Vector that stores which lines of the input
    #                    text were written to the output text file
    #--------------------------------------------------------------------
    
    # Step #1: Generate a random sampling of a text file
    #
    # Technincal Reference:
    # --------------------
    # https://class.coursera.org/dsscapstone-002/wiki/Task_1
    lines_to_read <- ceiling(10 / (percentageToSample / 100))
    
    # http://stackoverflow.com/questions/15532810/reading-40-gb-csv-file-into-r-using-bigmemory?lq=1
    # http://stackoverflow.com/questions/7260657/how-to-read-whitespace-delimited-strings-until-eof-in-r
    if (lines_to_read > num_lines[[basename(inputTextFilePath)]][1]) {
        lines_to_read <- num_lines[[basename(inputTextFilePath)]][1]
    }
    
    sample_line_idx <- numeric()
    file_subset <- character()
    
    if (displayStatus) {
        print("--------------------------------------------------------------")
        print(sprintf("Generating random sample of %s",
                      basename(inputTextFilePath)))        
    }

    h_conn <- file(inputTextFilePath, "r", blocking=FALSE)
    lines_read <- 0
    repeat {
        cur_chunk <- readLines(h_conn, lines_to_read, skipNul=TRUE)
        
        if (length(cur_chunk) == 0) {
            break
        }
        else {            
            cur_sample_line_idx <- which(rbinom(lines_to_read,
                                                1,
                                                percentageToSample/100) == 1)
            
            file_subset <- append(file_subset,
                                  cur_chunk[cur_sample_line_idx])
            
            sample_line_idx <- append(sample_line_idx,
                                      cur_sample_line_idx + lines_read)
            
            lines_read <- lines_read + lines_to_read
            
            if (displayStatus) {
                print(sprintf("Lines read: %d (Out of %d)",
                              lines_read,
                              num_lines[[basename(inputTextFilePath)]][1]))
            }
        }
    }
    close(h_conn)
    
    print(sprintf("Requested sampling percentage: %.5f", percentageToSample))
    
    print(sprintf("Percentage of lines sampled: %.5f",
                  100.0*length(file_subset) / 
                      num_lines[[basename(inputTextFilePath)]][1]))
    
    # Step #4: Write the random sample of a text file to disk
    h_conn <- file(outputTextFilePath, "w")
    write(file_subset, file=h_conn)
    close(h_conn)
    
    return(sample_line_idx)
}

sampleTextFileUnitTest <- function(textFilePath,
                                   num_lines,
                                   numberOfLinesToRead,
                                   percentageToSample) {
    #--------------------------------------------------------------------
    # sampleTextFile() Unit Test
    #
    # Args:
    #   textFilePath: Full path to large text file
    #
    #   num_lines: List that stores the number of lines of each text file
    #              contained in a directory
    #
    #   numberOfLinesToRead: Number of lines to read in order to 
    #                        construct unit test input
    #
    #   percentageToSample: Percentage of lines of the unit test input to
    #                       sample
    #
    # Returns:
    #   None
    #--------------------------------------------------------------------
    
    # Step #1: Read the requested number of lines from the beginning of a
    #          large text file
    h_conn <- file(textFilePath, "r")
    doc_head <- readLines(h_conn, numberOfLinesToRead)
    close(h_conn)
    
    # Step #2: Write the large text file sample to disk
    filePrefix = strsplit(basename(textFilePath),'\\.txt')
    
    inputTextFilePath <- 
        file.path(".",paste0(filePrefix, numberOfLinesToRead, ".txt"))
    
    h_conn <- file(inputTextFilePath,"w")
    write(doc_head, file=h_conn)
    rm(doc_head)
    close(h_conn)
    
    # http://www.inside-r.org/packages/cran/R.utils/docs/countLines
    num_linesCopy <- num_lines
    h_conn <- file(inputTextFilePath, "rb")
    num_linesCopy[[basename(inputTextFilePath)]] <- countLines(h_conn)
    close(h_conn)
    
    # Step #3: Generate a random sample of a text file
    filePrefix <- strsplit(basename(inputTextFilePath),'\\.txt')
    
    outputTextFilePath <- file.path(".",paste0(filePrefix, "Sample", ".txt"))
    
    sample_line_idx <- sampleTextFile(inputTextFilePath,
                                      num_linesCopy,
                                      percentageToSample,
                                      outputTextFilePath)
    
    # Step #4: Verify that the random text file sampler is functioning 
    #          correctly
    h_conn <- file(inputTextFilePath, "r")
    inputTextFile <- scan(h_conn, what=character(), sep="\n", quiet=TRUE)
    close(h_conn)
    
    h_conn <- file(outputTextFilePath, "r")
    outputTextFile <- scan(h_conn, what=character(), sep="\n", quiet=TRUE)
    close(h_conn)
    
    for (n in seq_len(length(sample_line_idx))) {
        if (outputTextFile[n] == inputTextFile[sample_line_idx[n]]) {
            print(sprintf("line #%d matched", sample_line_idx[n]))
        }
        else {
            print("---------------------------------------------------")
            print(sprintf("Line #: %d", sample_line_idx[n]))
            print(paste("Input:", inputTextFile[sample_line_idx[n]]))
            print(paste("Output:", outputTextFile[n]))
        }
    }
}

applyRandomSamplerToTextFiles <- function(inputTextFileDirectory,
                                          percentageToSample,
                                          outputTextFileDirectory,
                                          displayStatus=FALSE) {
    #--------------------------------------------------------------------
    # Generates a random sample of each text file contained in a 
    # directory
    #
    # Args:
    #   inputTextFileDirectory: Full path to a directory that contains a 
    #                           set of text files
    #
    #   percentageToSample: Percentage of text file to randomly sample
    #
    #   outputTextFileDirectory: Full path to a directory that contains a 
    #                           random sample of a set of text files
    #
    #   displayStatus: Optional Boolean input that controls whether or not
    #                  text document processing status is printed to the 
    #                  status window
    #
    # Returns:
    #   None
    #--------------------------------------------------------------------
    load(file=file.path(outputTextFileDirectory,
                        paste0(basename(outputTextFileDirectory),
                               "NumLines.RData")))
    
    textFileSampling <- list()

    samplingStr = gsub("\\.","p",sprintf("%.2fPercent", percentageToSample))

    for(curTextFile in dir(inputTextFileDirectory, pattern="(.)*.txt")) {        
        print(sprintf("Generating a %.2f%% random sample of %s",
                      percentageToSample, curTextFile))
        
        curOutputFileName <- 
            paste0(strsplit(curTextFile,"\\.txt"),samplingStr,".txt")
    
        textFileSampling[[curOutputFileName]] <- 
            sampleTextFile(file.path(inputTextFileDirectory,
                                     curTextFile),
                           num_lines,
                           percentageToSample,
                           file.path(outputTextFileDirectory,
                                     curOutputFileName),
                           displayStatus)
    }
    save(file=file.path(outputTextFileDirectory,
                        paste0(basename(inputTextFileDirectory),
                               samplingStr,'Sampling.RData')),
         textFileSampling)
}
