source("./initializeSamplingString.R")

# http://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
# http://stackoverflow.com/questions/18153504/removing-non-english-text-from-corpus-in-r-using-tm
removeNonASCII <-
    content_transformer(function(x) iconv(x, "latin1", "ASCII", sub=""))

convertToLowerCase <- content_transformer(function(x) tolower(x))

generateFeatures <- function(featureParams,
                             displayStatus=FALSE) {
    #--------------------------------------------------------------------------
    # This function constructs the following word features:
    # - Unigrams (i.e. words)
    # - Bigrams (i.e. word pairs)
    # - Trigrams (i.e. word triplets)
    #
    # by randomly sampling a text file
    #
    # Args:
    #   featureParams: List that contains the following parameters:
    #
    #                 Field:              Description:
    #                 ------              -----------
    #                 textFilePath        String that stores the path to a 
    #                                     directory where a text file is 
    #                                     located
    #
    #                 textFile            String that stores the name of a text 
    #                                     file
    #
    #                 textFileLanguage    String that describes the text file 
    #                                     language (i.e "english")
    #
    #                 num_lines           List that stores the number of lines 
    #                                     in a set of text documents
    #
    #                 blackList           Character vector that stores a list
    #                                     of words to exclude from from a line 
    #                                     corpus
    #
    #   displayStatus: Optional Boolean input that controls whether or not
    #                  text document processing status is printed to the 
    #                  status window
    #
    # Returns:
    #   wordFeatures:  List that contains the following word features
    #
    #                 Field:              Description:
    #                 ------              -----------
    #                 wordFreqs           Word frequency vector
    #
    #                 bigramFreqs         Bigram frequency vector
    #
    #                 trigramFreqs        Trigram frequency vector
    #
    #                 sample_line_idx     Numeric vector that stores which
    #                                     text file lines were sampled
    #--------------------------------------------------------------------------
    lines_to_read <- ceiling(10 / (featureParams$percentageToSample / 100))
    
    print(featureParams$textFile)
    
    # http://stackoverflow.com/questions/15532810/reading-40-gb-csv-file-into-r-using-bigmemory?lq=1
    # http://stackoverflow.com/questions/7260657/how-to-read-whitespace-delimited-strings-until-eof-in-r
    if (lines_to_read > 10000) {
        lines_to_read <- 10000
    }else if (lines_to_read < 1000) {
        lines_to_read <- 1000
    }
    
    sample_line_idx <- numeric()
    file_subset <- character()
    
    if (displayStatus) {
        print("--------------------------------------------------------------")
        print(sprintf("Generating corpus from %s", featureParams$textFile))        
    }
    
    BigramTokenizer <- function(x) {
        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
    }
    
    TrigramTokenizer <- function(x) {
        RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))
    }
    
    #http://stackoverflow.com/questions/24099098/language-detection-in-r-with-the-textcat-package-how-to-restrict-to-a-few-lang
    profileDb <- TC_byte_profiles[names(TC_byte_profiles) %in% 
                                      c("english",
                                        "french",
                                        "finnish",
                                        "russian-iso8859_5",
                                        "russian-koi8_r",
                                        "russian-windows1251")]
    
    firstChunk <- TRUE
    wordFeatures <- list()
    
    h_conn <- file(file.path(featureParams$textFilePath,
                             featureParams$textFile), "r", blocking=FALSE)
    cur_chunk <- readLines(h_conn, lines_to_read, skipNul=TRUE)
    firstChunk <- TRUE
    num_lines_read <- length(cur_chunk)

    languageRegex <- paste0(featureParams$textFileLanguage,"[a-z0-9_]*")
    total_num_lines <- featureParams$num_lines[[featureParams$textFile]]
    
    wordFeatures$sample_line_idx <- numeric()

    repeat {        
        if (length(cur_chunk) == 0) {
            break
        }
        else {
            if (displayStatus) {
                print(sprintf("# of lines read: %d (out of %d)",
                              num_lines_read, total_num_lines))
            }
            
            cur_sample_line_idx <-
                which(rbinom(lines_to_read,
                             1,
                             featureParams$percentageToSample/100) == 1)
            
            wordFeatures$sample_line_idx <-
                append(wordFeatures$sample_line_idx,
                       cur_sample_line_idx + num_lines_read)

            cur_chunk <- cur_chunk[cur_sample_line_idx]
            
            curChunkLanguage <- textcat(cur_chunk, p = profileDb)
            
            validLanguageIdx <- which(grepl(languageRegex, curChunkLanguage))
            
            cur_chunk <- cur_chunk[validLanguageIdx]
            
            if (length(cur_chunk) > 0) {
                curChunkCorpus <- processDocumentChunk(cur_chunk,
                                                       featureParams$blackList)
                
                tdm <- as.matrix(TermDocumentMatrix(curChunkCorpus))
                curWordFreqs <- sort(rowSums(tdm), decreasing=TRUE)
                rm(tdm)
                
                tdmBi <- 
                    as.matrix(TermDocumentMatrix(curChunkCorpus,
                                                 control =
                                                     list(tokenize =
                                                          BigramTokenizer)))
                
                curBigramFreqs <- sort(rowSums(tdmBi), decreasing=TRUE)
                rm(tdmBi)
                
                tdmTri <- 
                    as.matrix(TermDocumentMatrix(curChunkCorpus,
                                                 control =
                                                     list(tokenize = 
                                                          TrigramTokenizer)))
                
                rm(curChunkCorpus)
                curTrigramFreqs <- sort(rowSums(tdmTri), decreasing=TRUE)
                                
                if (firstChunk) {
                    wordFeatures$wordFreqs <- curWordFreqs
                    wordFeatures$bigramFreqs <- curBigramFreqs
                    wordFeatures$trigramFreqs <- curTrigramFreqs
                    firstChunk <- FALSE
                }
                else {
                    wordFeatures$wordFreqs <- c(wordFeatures$wordFreqs,
                                                curWordFreqs)

                    wordFeatures$bigramFreqs <- c(wordFeatures$bigramFreqs,
                                                  curBigramFreqs)
                    
                    wordFeatures$trigramFreqs <- c(wordFeatures$trigramFreqs,
                                                   curTrigramFreqs)
                }
            
                if (displayStatus) {
                    print(sprintf('total # of words: %d',
                                  sum(wordFeatures$wordFreqs)))
                }

                rm(curWordFreqs)
                rm(curBigramFreqs)
                rm(curTrigramFreqs)
            }
        }
        cur_chunk <- readLines(h_conn, lines_to_read, skipNul=TRUE)
        num_lines_read <- num_lines_read + length(cur_chunk)
    }
    close(h_conn)

    return(wordFeatures)
}

processDocumentChunk <- function(cur_chunk,
                                 blackList) {
    #--------------------------------------------------------------------------
    # This function applies the following set of transformations to a set of 
    # text file lines:
    #   - Removes non-ASCII characters
    #   - Removes punctuation
    #   - Removes whitespace
    #   - Converts text to lower case
    #   - Removes words contained in a "black list"
    #
    # Args:
    #   cur_chunk: Character vector that stores a set of text file lines
    #
    #   blackList: Character vector that stores a list of words to exclude from
    #              from a line corpus
    #
    # Returns:
    #   curChunkCorpus: tm R package Corpus object that contains a corpus for
    #                   a set of text file lines
    #--------------------------------------------------------------------------
    curChunkCorpus <- Corpus(VectorSource(cur_chunk))
    
    curChunkCorpus <- tm_map(curChunkCorpus, removeNonASCII)
    
    curChunkCorpus <- tm_map(curChunkCorpus, removePunctuation)
    
    curChunkCorpus <- tm_map(curChunkCorpus, stripWhitespace)
    
    curChunkCorpus <- tm_map(curChunkCorpus, convertToLowerCase)
    
    curChunkCorpus <- tm_map(curChunkCorpus, removeWords, blackList)    
    
    return (curChunkCorpus)
}
