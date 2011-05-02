word_trunc <-
function(textcolumn,wordtrunc) {

    Call <- match.call()
    indx <- match(c("textcolumn", "wordtrunc"), names(Call), nomatch = 0)
    if (indx[1] == 0) stop("Please include a column of text as a vector.")
    if (indx[2] == 0) stop("Please specify the number of words to truncate to.")

    word_token_trunc <- function(textstring, truncation) {
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

