tokenizeDocument <- function(textFilePath) {
    #--------------------------------------------------------------------
    # Returns a list that contains the words, numbers, and punctuation
    # contained in a text document
    #
    # Args:
    #   textFilePath: Full path to a text file
    #
    # Returns:
    #   tokens: List that contains the words, numbers, and punctuation
    # contained in a text document
    #--------------------------------------------------------------------
    library(R.utils)
    library(NLP)
    library(stringr)
    
    # http://www.inside-r.org/packages/cran/R.utils/docs/countLines
    h_conn <- file(file.path(textFilePath), "rb")
    num_lines <- countLines(h_conn)
    close(h_conn)
    
    lines_to_read = min(ceiling(num_lines/10), 10000) 
    
    tokens = list()
    tokens$word = character()
    tokens$number = double()
    tokens$punctuation = character()
    
    word_tokenizer = Regexp_Tokenizer("[A-Za-z]+")
    
    h_conn <- file(textFilePath, "r", blocking=FALSE)
    repeat {
        cur_chunk <- readLines(h_conn, lines_to_read, skipNul=TRUE)

        if (length(cur_chunk) == 0) {
            break
        }
        else {
            for (cur_line in cur_chunk) {
                cur_line <- String(str_trim(iconv(cur_line,
                                                  "latin1",
                                                  "ASCII",
                                                  sub="")))

                word_spans <- word_tokenizer(cur_line)
                word_spans_list <- as.list(word_spans)

                if (length(word_spans_list) > 1) {
                    wordTokensFound <- TRUE
                } else if (word_spans_list[[1]]$start > 0) {
                    wordTokensFound <- TRUE
                } else {
                    wordTokensFound <- FALSE
                }
                
                if (wordTokensFound) {
                    cur_word_tokens <- cur_line[word_spans]
                    tokens$word <- append(tokens$word, cur_word_tokens)                    
                }
                
                cur_tokens <- cur_line[wordpunct_tokenizer(cur_line)]
                
                wordsAndNumbersBool <-
                    grepl(paste0("[A-Za-z]+[0-9]+(\\.)*[0-9]*|",
                                 "[0-9]+(\\.)*[0-9]*[A-Za-z]+"), cur_tokens)
                
                wordsAndNumbersIdx <- which(wordsAndNumbersBool)
                
                #http://stackoverflow.com/questions/15422778/r-getting-substrings-and-regular-expressions
                if (length(wordsAndNumbersIdx)) {
                    wordsAndNumbers <- cur_tokens[wordsAndNumbersIdx]
                    
                    tokens$number <- 
                        append(tokens$number,
                               as.numeric(gsub(paste0("[A-Za-z]*([0-9]+",
                                                      "(\\.)*[0-9]*)[A-Za-z]*"),
                                               "\\1",wordsAndNumbers)))
                    
                    cur_tokens <- cur_tokens[!wordsAndNumbersBool]
                }
                
                number_tokens_bool <- grepl("([0-9]+(\\.)*[0-9]*)", cur_tokens)
                number_tokens_idx <- which(number_tokens_bool)
                
                if (length(number_tokens_idx) > 0) {
                    tokens$number <- 
                        append(tokens$number,
                               as.numeric(cur_tokens[number_tokens_idx]))
                }

                cur_punctuation_tokens_bool <-
                    !(grepl("[A-Za-z]+", cur_tokens) | number_tokens_bool)
                
                cur_punctuation_tokens_idx <- 
                    which(cur_punctuation_tokens_bool)
                
                if (length(cur_punctuation_tokens_idx) > 0) {
                    cur_punctuation_tokens <- 
                        cur_tokens[cur_punctuation_tokens_idx]
                    
                    cur_punctuation_tokens <-
                        cur_punctuation_tokens[grepl(".+",
                                                     cur_punctuation_tokens)]

                    if (length(cur_punctuation_tokens)) {
                        tokens$punctuation <-
                            append(tokens$punctuation, cur_punctuation_tokens)                        
                    }
                }
            }
        }
    }
    close(h_conn)
    
    return(tokens)
}
