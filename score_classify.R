####################################################
#                  score_classify                  #
####################################################
#Loren Collingwood, 3/6/2011 Alpha Version Rtexttools

#This function scores each row in the datset for the level of consensus across algorithms
#and adds an additional column onto the dataframe. It uses the agreescore internal Rtexttools
#algorithm

#Arguments
# dataframe -- a dataframe object
# columnnames -- a character vector of columnames from the above dataframe

#Values
# A new dataframe object with a scores column appended. Data can then be subsetted 
# based on this column.
score_classify <- function (dataframe, columnnames) {

    a <- dataframe
    a2 <- as.matrix(a[match(c(columnnames), colnames(a))])
    scores <- apply(a2, 1, agreescore) #agreescore function inserted here
    a3 <- cbind(a, scores)
    invisible(a3)

}

#Create scores for the test_resultsf dataframe. test_resultsf is a dataframe
#scores <- score_classify(test_resultsf,c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
#                        "glmnet_pred","tree_pred","nnet_pred"))
