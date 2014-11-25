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

formLineCorpus <- function(textFilePath,
                           textFile,
                           textFileLanguage,
                           num_lines,
                           blackList,
                           displayStatus=FALSE) {
    #--------------------------------------------------------------------------
    # This function forms a Corpus from a text document's lines
    #
    # Args:
    #   textFilePath: String that stores the path to a directory where a text 
    #                 file is located
    #
    #   textFile: String that stores the name of a text file
    #
    #   textFileLanguage: String that describes the text file language 
    #                      (i.e "english")
    #
    #   num_lines: List that stores the number of lines in a set of text 
    #              documents
    #
    #   blackList: Character vector that stores a list of words to exclude from
    #              from a line corpus
    #
    #   displayStatus: Optional Boolean input that controls whether or not
    #                  text document processing status is printed to the 
    #                  status window
    #
    # Returns:
    #   lineCorpus: tm R package Corpus object that contains a text document 
    #               line corpus
    #-------------------------------------------------------------------------- 
    
    #http://stackoverflow.com/questions/24099098/language-detection-in-r-with-the-textcat-package-how-to-restrict-to-a-few-lang
    profileDb <- TC_byte_profiles[names(TC_byte_profiles) %in% 
                                  c("english",
                                    "french",
                                    "finnish",
                                    "russian-iso8859_5",
                                    "russian-koi8_r",
                                    "russian-windows1251")]
        
    lines_to_read = min(ceiling(num_lines[[textFile]]/10), 10000)
    
    h_conn <- file(file.path(textFilePath, textFile), "r", blocking=FALSE)
    cur_chunk <- readLines(h_conn, lines_to_read, skipNul=TRUE)
    firstChunk <- TRUE
    num_lines_read <- length(cur_chunk)
    
    repeat {
        if (length(cur_chunk) == 0) {
            break
        }
        else {
            if (displayStatus) {
                print(sprintf("# of lines read: %d (out of %d)",
                              num_lines_read, num_lines[[textFile]]))
            }
            
            curChunkLanguage <- textcat(cur_chunk, p = profileDb)

            validLanguageIdx <- 
                which(grepl(paste0(textFileLanguage,"[a-z0-9_]*"),
                                   curChunkLanguage))
            
            cur_chunk <- cur_chunk[validLanguageIdx]
            
            if (length(cur_chunk) == 0) {
                break
            }
            else {
                if (firstChunk) {
                    lineCorpus <- processDocumentChunk(cur_chunk,
                                                       blackList)
                }
                else {
                    # http://www.inside-r.org/packages/cran/tm/docs/c.Corpus
                    lineCorpus <- c(lineCorpus,
                                    processDocumentChunk(cur_chunk,
                                                         blackList))
                }                
            }
        }
        
        cur_chunk <- readLines(h_conn, lines_to_read, skipNul=TRUE)
        num_lines_read <- num_lines_read + length(cur_chunk)
    }
    close(h_conn)
    
    return(lineCorpus)
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

    curChunkCorpus <- tm_map(curChunkCorpus, customRemovePunctuation)

    curChunkCorpus <- tm_map(curChunkCorpus, removeNumbers)
    
    curChunkCorpus <- tm_map(curChunkCorpus, stripWhitespace)
    
    curChunkCorpus <- tm_map(curChunkCorpus, removeWords, blackList)    
    
    return (curChunkCorpus)
}
