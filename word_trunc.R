################################################
#               word_trunc                     #
################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools

#Function performs word truncation on each text row in a dataset. 
#This function should be executed prior to DTM.
#This can be developed further.
library(openNLP)
#Packages need to be installed but not loaded. The example below is done on the French data, 
#so language here doesn't appear to be a problem.
#install.packages("openNLPmodels.en")
#install.packages("openNLPmodels.es")

#Arguments
#textcolumn -- the column or vector of text from the data.
#wordtrunc -- A number indicating how many many words are to be kept per record.

#Values
#A returned vector of truncated text.

word_trunc <- function(textcolumn,wordtrunc) {

    Call <- match.call()
    indx <- match(c("textcolumn", "wordtrunc"), names(Call), 
        nomatch = 0)
    if (indx[1] == 0) 
        stop("A character vector must be included")
    if (indx[2] == 0) 
        stop("A number must be included")

    word_token_trunc <- function(textstring, truncation){
    
        tokenwords <- tokenize(textstring)
        token_trunc <- tokenwords[1:truncation]
        backtogether <- paste(token_trunc, collapse=" ")
        return(backtogether)
    }

    newvec <- NULL
    newvec_clean <- NULL
    for (i in 1:length(textcolumn)) { #loop over each text item
        newvec[i] <- word_token_trunc(textcolumn[i],truncation=wordtrunc)
        #Need to delete the NA's that form from text that are blank or less than the truncation specification
        newvec_clean[i] <- gsub("NA","",newvec[i],fixed=TRUE)
    }
    rm(newvec) #clean up
return(newvec_clean)
}

#system.time(
#truncated_words <- word_trunc(da$headline,wordtrunc=4)
#)
#French dataset. 10,183 observations
#   user  system elapsed 
# 541.19   20.11  569.70 
