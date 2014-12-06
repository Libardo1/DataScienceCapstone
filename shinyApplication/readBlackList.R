readBlackList <- function(blackListFile) {
    #--------------------------------------------------------------------------
    # Reads a "black list" file that contains a list profane words that is
    # removed from a corpus via the "removeWords" function
    #
    # Args:
    #   blackListFile: String that stores the full path to a "black list" file
    #
    # Returns:
    #   blackList: Character vector that stores a list of profane words 
    #   contains a list profane words that is removed from a corpus via the 
    #   "removeWords" function
    #
    # Reference:
    #   http://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-
    #       and-how-to-use-facebooks-moderation-tool/
    #--------------------------------------------------------------------------
    blackList <- read.csv(blackListFile,header=FALSE,skip=4)
    blackList <- blackList[,2]
    blackList <- gsub(",","",blackList)
    blackList <- gsub(" ","",blackList)
    blackList <- gsub("[0-9]+","",blackList)
    blackList <- gsub("[\\.-]","",blackList)
    blackList <- blackList[!grepl("^a$",blackList)]
    blackList <- unique(blackList[blackList != ""])
    return(blackList)
}