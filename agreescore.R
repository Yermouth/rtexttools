###############################################################
#                        agreescore                           #
###############################################################
#Loren Collingwood, 3/6/2011, Alpha Version Rtexttools

#Function calculates agreement/consensus score across algorithms. Passes to apply() in score_classify.
#Currently, this works for just the eight build-in algorithms. Will have to change code slightly
#for when maximum entropy is incorporated.
#Arguments
#x is a numeric vector of algorithms labels. This will be the row in the data.
#Values
# A numeric score from 1-8 indicating the algorithmic agreement/consensus.

agreescore <- function (x) {
    
    a <- table(x)
    #LENGTH = 1
    if (length(a) == 1) 
        score <- 8
    #LENGTH = 2
    if (length(a) == 2) {
        if (any(a==7))
            score <- 7
        else if (any(a==6))
            score <- 6
        else if (any(a==5))
            score <- 5
        else if (any(a==4))
            score <- 4
    }
    #LENGTH = 3       
    if (length(a) == 3) {
        if (any(a==6))
            score <- 6
        else if (any(a==5))
            score <- 5
        else if (any(a==4))
            score <- 4
    }
    #LENGTH = 4
    if (length(a) == 4) {
        if (any(a==5))
            score <- 5
        else if (any(a==4))
            score <- 4
        else if (any(a==3))
            score <- 3
        else if (all(a==2))
            score <- 2
    }
    #LENGTH = 5
    if (length(a) == 5) {
        if (any(a==4))
            score <- 4
        else if (any(a==3))
            score <- 3
        else score <- 2
    }
    #LENGTH = 6
    if (length(a) == 6) {
        if(any(a==3))
            score <- 3
        else score <- 2
    }
    
    #LENGTH = 7
    if (length(a) == 7) 
        score <- 2

    #LENGTH = 8
    if (length(a) == 8) 
        score <- 1
    
    return(score)
}     

#see score_classify for example usage
