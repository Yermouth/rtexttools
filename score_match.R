##################################################################
#                          score_match                           #
##################################################################
#Loren Collingwood 3/9/2011 Alpha Version RTextTools
#Function calculate the concensus/agreement level across algorithms on a 1-n scale
#based on the number of algorithms used. In addition, it determines -- through 
#ensemble agreement -- which score is the best fit.

#Arguments
#data -- dataframe object including all the algorithm predictions
#columnnames -- character vector indicating the column names of the predictions (i.e. "glmnet_pred")

#Values
#Outputs a new dataframe object with two column appended.
#best_match_score is the column indicating the most likely label of the observation.
#agree_score is the ensemble agreement ranging from 1-n (1-9 in most cases) depending on the number 
#of algorithms used in training and classifying. The higher the number the greater the consensus.

#setwd("C:\\Users\\Loren\\Documents\\My Dropbox\\RTextTools\\Datasets")

#fscore <- read.csv("scores_french_small_train.csv",header=T)

score_match <- function(data,columnnames) {

    a <- data
    a2 <- as.matrix(a[match(c(columnnames), colnames(a))])

    best_match <- function(x) {
    
        x <- table(x)
        xsort <- sort(x)
        xlarge <- xsort[length(xsort)]
        xlarge_name <- names(xlarge)
        out <- as.numeric(xlarge_name)
        return(out)
    }
        
    best_match_score <- apply(a2,1,best_match)
    a3 <-  data.frame(a,best_match_score)
    

    best_score <- function(x) {
    
        x <- table(x)
        xsort <- sort(x)
        xlarge <- xsort[length(xsort)]
        return(xlarge)
    }

    agree_score <- apply(a2,1,best_score)
    a4 <- data.frame(a3,agree_score)
    invisible(a4)

}
#fscore_scores <-score_match(data=fscore,columnnames=c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
#                        "glmnet_pred","tree_pred","nnet_pred"))
