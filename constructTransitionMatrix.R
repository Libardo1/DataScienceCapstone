source("./formLineCorpus.R")

# http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-
#   in-termdocument-matrix-using-r-and-rweka)
BigramTokenizer <- function(x) {
    RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
}

# http://stackoverflow.com/questions/19615181/finding-ngrams-in-r-and-
#   comparing-ngrams-across-corpora
TrigramTokenizer <- function(x) {
    RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
}

tokenizeTrigrams <- function(cur_chunk,
                             blackList,
                             numberCores) {
    #-----------------------------------------------------------------
    # Initializes a named numeric vector that stores trigram counts
    #
    # Args:
    #   cur_chunk: Character vector that stores a subset of a text 
    #              file
    #
    #   blackList: Character vector that stores a list of words to 
    #              exclude from a line corpus
    #
    #   numberCores: Optional input that defines the number of cores
    #                to use when performing tm_map operations
    #
    # Returns:
    #   tdmTri: Named numeric vector that stores trigram counts
    #-----------------------------------------------------------------
    curLineCorpus <- processDocumentChunk(cur_chunk,
                                          blackList,
                                          numberCores)
    
    #http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-
    #   in-termdocument-matrix-using-r-and-rweka
    options(mc.cores=1)
    tdmTri <- 
        as.matrix(TermDocumentMatrix(curLineCorpus,
                                     control =
                                         list(tokenize = TrigramTokenizer)))
    
    tdmTri <- rowSums(as.matrix(tdmTri))
    
    return(tdmTri)
}

tokenizeBigrams <- function(cur_chunk,
                            blackList,
                            numberCores) {
    #-----------------------------------------------------------------
    # Initializes a named numeric vector that stores bigram counts
    #
    # Args:
    #   cur_chunk: Character vector that stores a subset of a text 
    #              file
    #
    #   blackList: Character vector that stores a list of words to 
    #              exclude from a line corpus
    #
    #   numberCores: Optional input that defines the number of cores
    #                to use when performing tm_map operations
    #
    # Returns:
    #   tdmBi: Named numeric vector that stores bigram counts
    #-----------------------------------------------------------------
    curLineCorpus <- processDocumentChunk(cur_chunk,
                                          blackList,
                                          numberCores)

    #http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-
    #   in-termdocument-matrix-using-r-and-rweka
    options(mc.cores=1)
    tdmBi <- 
        as.matrix(TermDocumentMatrix(curLineCorpus,
                                     control =
                                         list(tokenize = BigramTokenizer)))
    
    tdmBi <- rowSums(as.matrix(tdmBi))
    
    return(tdmBi)
}

initializeCommonTrigramIndices <- function(curTriTdm,
                                           vocabulary) {
    #-----------------------------------------------------------------
    # Initializes a numeric vector that stores the indices 
    # corresponding to trigrams that are constructed from the
    # input vocabulary
    #
    # Args:
    #   tdmTri: Named numeric vector that stores trigram counts
    #
    #   vocabulary: Character vector that stores a vocabulary
    #
    # Returns:
    #   commonIdx: numeric vector that stores the indices 
    #              corresponding to trigrams that are constructed
    #              from the input vocabulary
    #-----------------------------------------------------------------
    trigrams <- names(curTriTdm)
    commonIdx <- numeric()
    
    for (n in seq_len(length(trigrams))) { 
        curWords <- unlist(str_split(trigrams[n]," "))
        
        if (sum(curWords %in% vocabulary) == 3) {
            commonIdx <- append(commonIdx,n)
        }
    }
    
    return(commonIdx)    
}

initializeTrigramCounts <- function(textFileDirectory,
                                    textDataFile,
                                    num_lines,
                                    commonTerms,
                                    blackList,
                                    textFileLanguage = "english",
                                    numberCores = 1) {
    #-----------------------------------------------------------------
    # Initializes triagram counts in a Markov chain transition matrix
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
    #   commonTerms: Character vector that stores common terms (i.e
    #                words that are used to segment trigrams
    #
    #   blackList: Character vector that stores a list of words to 
    #              exclude from a line corpus
    #
    #   textFileLanguage: Optional string input that defines which 
    #                     language to segment (i.e. "english")
    #
    #   numberCores: Optional input that defines the number of cores
    #                to use when performing tm_map operations
    #
    # Returns:
    #   transitionMatrix: Square matrix that stores term counts
    #                     (this matrix is an unnormalized Markov chain
    #                      transition matrix)
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
    num_lines_to_read <- 2500

    lines_read <- 0
    word_count <- 0
    h_conn <- file(inputTextFilePath, "r", blocking=FALSE)
    
    print("---------------------------------------------------------")
    print(sprintf("Analyzing %s", textDataFile))
    
    vocabularySize <- length(commonTerms)
    
    transitionMatrix = matrix(numeric(vocabularySize^2),
                              byrow=TRUE,
                              nrow=vocabularySize,
                              dimnames=list(commonTerms,
                                            commonTerms))
    
    transitionMatrixPath <- 
        file.path(dirname(inputTextFilePath),
                  paste0(unlist(str_split(textDataFile,"\\.txt"))[1],
                         "_TransitionMatrix.RData"))
    
    repeat {
        cur_chunk <- readLines(h_conn, num_lines_to_read, skipNul=TRUE)
        
        if (length(cur_chunk) > 0) {
            lines_read <- lines_read + length(cur_chunk)
            
            print("---------------------------------------------------------")
            print(sprintf("Lines read: %d (Out of %d)", lines_read,
                          total_num_lines))
            
            # http://stackoverflow.com/questions/9546109/how-to-
            #   remove-002-char-in-ruby
            #
            # http://stackoverflow.com/questions/11874234/difference-between-w-
            #   and-b-regular-expression-meta-characters
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
                tdmTri <- tokenizeTrigrams(cur_chunk,
                                           blackList,
                                           numberCores)
                
                commonIdx <- initializeCommonTrigramIndices(tdmTri,
                                                            commonTerms)
                
                commonTriTdm <- tdmTri[commonIdx]
                
                for (m in seq_len(length(commonTriTdm))) {                    
                    curWords <- unlist(str_split(names(commonTriTdm[m])," "))
                    
                    for (n in seq(2,3)) {
                        rowIdx <- which(grepl(paste0("^",curWords[n-1],"$"),
                                              commonTerms))

                        colIdx <- which(grepl(paste0("^",curWords[n],"$"),
                                              commonTerms))

                        transitionMatrix[rowIdx,colIdx] <- 
                            transitionMatrix[rowIdx,colIdx] + commonTriTdm[m]
                    }
                }
                
                save(file=transitionMatrixPath, transitionMatrix)

                rm(cur_chunk)
                rm(tdmTri)
                rm(commonTriTdm)
            }
        } else {
            break
        }
    }
    close(h_conn)
    
    return(transitionMatrix)
}

constructTransitionMatrix <- function(textFileDirectory,
                                      num_lines,
                                      commonTerms,
                                      blackList,
                                      textFileLanguage = "english",
                                      numberCores = 6) {
    #-----------------------------------------------------------------
    # Initializes a Markov chain transition matrix
    #
    # Args:
    #   textFileDirectory: String that stores the full path to a text
    #                      data file directory
    #
    #   num_lines: List that stores the number of lines in each file
    #              located in textFileDirectory
    #
    #   commonTerms: Character vector that stores common terms (i.e
    #                words that are used to segment trigrams
    #
    #   blackList: Character vector that stores a list of words to 
    #              exclude from a line corpus
    #
    #   textFileLanguage: Optional string input that defines which 
    #                     language to segment (i.e. "english")
    #
    #   numberCores: Optional input that defines the number of cores
    #                to use when performing tm_map operations
    #
    # Returns:
    #   None (Writes the transiition matrix to an RData file before
    #         after normalization)
    #-----------------------------------------------------------------
    trainingDataFiles <- dir(outputTextFileDirectory,
                             pattern=".*TrainingData.txt")
    
    for (n in seq_len(length(trainingDataFiles))) {
        curTransitionMatrix <- initializeTrigramCounts(textFileDirectory,
                                                       trainingDataFiles[n],
                                                       num_lines,
                                                       commonTerms,
                                                       blackList)
        
        if (n == 1) {
            transitionMatrix <- curTransitionMatrix
        } else {
            transitionMatrix <- transitionMatrix + curTransitionMatrix
        }
    }
    
    minProbability <- 0.01/(length(commonTerms)-1)
    
    for (m in seq_len(nrow(transitionMatrix))) {
        curRowSum <- sum(transitionMatrix[m,])
        
        if (curRowSum > 0) {
            transitionMatrix[m,] <- 
                transitionMatrix[m,] / curRowSum
        } else {
            transitionMatrix[m,m] <- 0.99
            
            n <- seq_len(ncol(transitionMatrix))
            n <- n[n != m]
            transitionMatrix[m,n] <- minProbability
        }
    }
    save(file="./transitionMatrix.RData", transitionMatrix)
}

loadVocabularyCounts <- function(textFileDirectory) {
    #-----------------------------------------------------------------
    # Loads un-normalized transition matricies (i.e. vocabulary word
    # counts)
    #
    # Args:
    #   textFileDirectory: String that defines the directory that contains
    #                      un-normalized transition matricies for the
    #                      english language training data
    #
    # Returns:
    #   vocabularyCounts: List that contains the vocabulary counts for
    #                     the english language training data
    #-----------------------------------------------------------------
    vocabularyCounts <- list()
    
    load(file.path(textFileDirectory,
                   "en_US.blogs_TrainingData_TransitionMatrix.RData"))
    vocabularyCounts[["blogs"]] <- transitionMatrix
         
    load(file.path(textFileDirectory,
                   "en_US.twitter_TrainingData_TransitionMatrix.RData"))
    vocabularyCounts[["twitter"]] <- transitionMatrix
    
    load(file.path(textFileDirectory,
                   "en_US.news_TrainingData_TransitionMatrix.RData"))
    vocabularyCounts[["news"]] <- transitionMatrix
    
    return(vocabularyCounts)
}

initializeVocabularyDistribution <- function(vocabularyCounts) {
    #------------------------------------------------------------------------
    # Initializes a data frame that describes the distribution of
    # vocabulary words across the english language training data
    #
    # Args:
    #   vocabularyCounts: List that contains the vocabulary counts for
    #                     the english language training data
    #
    # Returns:
    #   vocabularyDistribution: Data frame that describes the 
    #                           distribution of vocabulary words across 
    #                           the english language training data
    #------------------------------------------------------------------------    
    blogs <- data.frame(counts=rowSums(vocabularyCounts[["blogs"]]))
    blogs$type <- "blogs"
    blogs$vocabularyindex <- seq(1,nrow(blogs))
    rownames(blogs) <- NULL
    
    twitter <- data.frame(counts=rowSums(vocabularyCounts[["twitter"]]))
    twitter$type <- "twitter"
    twitter$vocabularyindex <- seq(1,nrow(twitter))
    rownames(twitter) <- NULL
    
    news <- data.frame(counts=rowSums(vocabularyCounts[["news"]]))
    news$type <- "news"
    news$vocabularyindex <- seq(1,nrow(news))
    rownames(news) <- NULL
    
    vocabularyDistribution <- rbind(rbind(blogs, twitter), news)
    
    vocabularyDistribution$counts <- 
        vocabularyDistribution$counts / ncol(vocabularyCounts[["blogs"]])
    
    return(vocabularyDistribution)
}

constructAverageTransitionMatrix <- function(vocabularyCounts) {
    #------------------------------------------------------------------------
    # Constructs an average normalized transition matrix based on the
    # vocabulary counts for the enligsh language training data
    #
    # Args:
    #   vocabularyCounts: List that contains the vocabulary counts for
    #                     the english language training data
    #
    # Returns:
    #   averageTranstionMatrix: List that contains the following data:
    #       - transitionData: An average normalized transition matrix based on 
    #                         the vocabulary counts for the enligsh language 
    #                         training data
    #
    #       - zeroCount: Number of columns in each transition matrix row where
    #                    the frequency of occurence in the training data was 
    #                     zero
    #------------------------------------------------------------------------
    transitionMatrix <- (vocabularyCounts[["blogs"]] + 
                         vocabularyCounts[["twitter"]] + 
                         vocabularyCounts[["news"]]) / 3.0
    

    zeroCount <- vector('numeric',ncol(transitionMatrix))
    for (n in seq_len(length(zeroCount))) {
        zeroColIdx <- which(transitionMatrix[n,] == 0)
        zeroCount[n] <- length(zeroColIdx)
        
        if (zeroCount[n] != ncol(transitionMatrix)) {
            transitionMatrix[n,] <- 
                transitionMatrix[n,] / sum(transitionMatrix[n,])
            
            if (zeroCount[n] > 0) {
                nonZeroColIdx <- which(transitionMatrix[n,] > 0)
                
                minProbability = (1 - length(nonZeroColIdx) / 
                                 ncol(transitionMatrix))/zeroCount[n]
                
                nonZeroColIdx <- which(transitionMatrix[n,] > minProbability)
                
                probabilityAdjustment <- 
                    (minProbability*zeroCount[n])/length(nonZeroColIdx)
                
                transitionMatrix[n,nonZeroColIdx] <- 
                    transitionMatrix[n,nonZeroColIdx] - probabilityAdjustment
                
                transitionMatrix[n,zeroColIdx] <- minProbability
            }    
        }
        else {
            transitionMatrix[n,] <- 1.0/ncol(transitionMatrix)
        }    
    }
    
    averageTranstionMatrix <- list()
    averageTranstionMatrix[["zeroCount"]] <- zeroCount
    averageTranstionMatrix[["transitionMatrix"]] <- transitionMatrix
    
    return(averageTranstionMatrix)
}
