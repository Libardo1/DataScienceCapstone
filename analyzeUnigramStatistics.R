library(data.table)
source("./formLineCorpus.R")

computeTermFrequencies <- function(curTDM) {
    #-----------------------------------------------------------------
    # Transforms the row sums of a TermDocumentMatrix into a 
    # data.table
    #
    # Args:
    #   curTDM: Term Document Matrix
    #
    # Returns:
    #   curTermFreq: data.table that stores the row sums of a 
    #                term document matrix
    #-----------------------------------------------------------------
    curTermFreq <- sort(rowSums(as.matrix(curTDM)), decreasing=TRUE)
    
    curTermFreq <- as.data.frame(curTermFreq)
    curTermFreq$unigram <- rownames(curTermFreq)
    rownames(curTermFreq) <- NULL
    colnames(curTermFreq) <- c("count","unigram")
    curTermFreq <- as.data.table(curTermFreq)
    setkey(curTermFreq,unigram)
    
    return(curTermFreq)
}

analyzeUnigramStatistics <- function(textFileDirectory,
                                     textDataFile,
                                     num_lines,
                                     blackList,
                                     chunkSparsity = 0.999,
                                     textFileLanguage = "english",
                                     numberCores = 6) {
    #-----------------------------------------------------------------
    # Computes the unigram (i.e. word) statistics of a text file
    #
    # Args:
    #   textFileDirectory: String that stores the full path to a text
    #                      data file directory
    #
    #   textDataFile: Name of a text data file
    #
    #   num_lines: List that stores the number of lines in each file
    #              located in textFileDirectory
    #
    #   blackList: Character vector that stores a list of words to 
    #              exclude from from a line corpus
    #
    #   chunkSparsity: Optional floating point input that defines 
    #                  the removeSparseTerms() sparse input
    #
    #   textFileLanguage: Optional string input that defines which 
    #                     language to segment (i.e. "english")
    #
    #   numberCores: Optional input that defines the number of cores
    #                to use when performing tm_map operations
    #
    # Returns:
    #   None (This R script writes the "..Terms.RData file to 
    #         textFileDirectory that has the same prefix as
    #         textDataFile)
    #-----------------------------------------------------------------
    #http://stackoverflow.com/questions/24099098/language-detection-
    #   in-r-with-the-textcat-package-how-to-restrict-to-a-few-lang
    profileDb <- TC_byte_profiles[names(TC_byte_profiles) %in% 
                                      c("english",
                                        "french",
                                        "finnish",
                                        "russian-iso8859_5",
                                        "russian-koi8_r",
                                        "russian-windows1251")]
    
    inputTextFilePath <- file.path(textFileDirectory, textDataFile)
    
    total_num_lines <- num_lines[[textDataFile]][1]
    num_lines_to_read <- ceiling(total_num_lines/100)
    
    firstChunk <- TRUE
    lines_read <- 0
    word_count <- 0
    h_conn <- file(inputTextFilePath, "r", blocking=FALSE)    

    print(sprintf("Analyzing %s", textDataFile))
    
    repeat {
        cur_chunk <- readLines(h_conn, num_lines_to_read, skipNul=TRUE)
        
        if (length(cur_chunk) > 0) {
            lines_read <- lines_read + length(cur_chunk)
            
            print("-------------------------------------------------------------")
            print(sprintf("Lines read: %d (Out of %d)", lines_read,
                          total_num_lines))
            
            # http://stackoverflow.com/questions/9546109/how-to-remove-002-char-in-ruby
            #
            # http://stackoverflow.com/questions/11874234/difference-between-w-and-b-
            #   regular-expression-meta-characters
            cur_chunk <- gsub("\\W+"," ", cur_chunk)
            
            curChunkLanguage <- textcat(cur_chunk, p = profileDb)
            
            validLanguageIdx <- 
                which(grepl(paste0(textFileLanguage,"[a-z0-9_]*"),
                            curChunkLanguage))
            
            cur_chunk <- cur_chunk[validLanguageIdx]
            
            if (length(cur_chunk) == 0) {
                break
            }
            else {
                curLineCorpus <- processDocumentChunk(cur_chunk,
                                                      blackList,
                                                      numberCores)
                
                curTDM <- TermDocumentMatrix(curLineCorpus)
                
                word_count <- word_count + sum(rowSums(as.matrix(curTDM)))
                
                curTDM <- removeSparseTerms(curTDM, chunkSparsity)
                
                curChunkTermFreqs <- computeTermFrequencies(curTDM)
                
                if (firstChunk == TRUE) {
                    termFreqs <- curChunkTermFreqs
                    firstChunk <- FALSE
                }else {
                    termFreqs <- merge(termFreqs, curChunkTermFreqs, all=TRUE)
                    termFreqs$count.x[is.na(termFreqs$count.x)] = 0
                    termFreqs$count.y[is.na(termFreqs$count.y)] = 0
                    
                    termFreqs <- as.data.frame(termFreqs)
                    termFreqs$count <- termFreqs$count.x + termFreqs$count.y
                    termFreqs <- data.table(termFreqs[,c("count","unigram")])
                    setkey(termFreqs,unigram)
                }
                
                rm(cur_chunk)
                rm(curLineCorpus)
                rm(curChunkTermFreqs)
                
                print(sprintf("Current number of terms: %d", nrow(termFreqs)))
            }
        } else {
            break
        }
    }
    close(h_conn)
    filePrefix <- unlist(str_split(basename(inputTextFilePath),"\\.txt"))[1]
    save(file=file.path(textFileDirectory,paste0(filePrefix,"Terms.RData")),
         termFreqs, word_count)
}

analyzeTextDataUnigramStatistics <- function(textFileDirectory,
                                             textFilePattern,
                                             num_lines,
                                             blackList,
                                             chunkSparsity = 0.999,
                                             textFileLanguage = "english",
                                             numberCores = 6) {
    #-----------------------------------------------------------------
    # Computes the unigram (i.e. word) statistics of a set of text 
    # file(s) contained in a directory
    #
    # Args:
    #   textFileDirectory: String that stores the full path to a text
    #                      data file directory
    #
    #   textFilePattern: Regular expression that refers to a set of 
    #                    text files
    #
    #   num_lines: List that stores the number of lines in each file
    #              located in textFileDirectory
    #
    #   blackList: Character vector that stores a list of words to 
    #              exclude from from a line corpus
    #
    #   chunkSparsity: Optional floating point input that defines 
    #                  the removeSparseTerms() sparse input
    #
    #   textFileLanguage: Optional string input that defines which 
    #                     language to segment (i.e. "english")
    #
    #   numberCores: Optional input that defines the number of cores
    #                to use when performing tm_map operations
    #
    # Returns:
    #   None (This R script writes the "..Terms.RData file to 
    #         textFileDirectory that has the same prefix as
    #         textDataFile)
    #-----------------------------------------------------------------
    for (curTextFile in dir(textFileDirectory,pattern=textFilePattern)) {
        filePrefix <- unlist(str_split(curTextFile,"\\.txt"))[1]
    
        if (!file.exists(file.path(textFileDirectory,
                                   paste0(filePrefix,"Terms.RData")))) {
            analyzeUnigramStatistics(textFileDirectory,
                                     curTextFile,
                                     num_lines,
                                     blackList,
                                     chunkSparsity,
                                     textFileLanguage,
                                     numberCores)
        }
    }    
}
