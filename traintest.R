#######################################################
#                      traintest                      #
#######################################################
#Loren Collingwood 3/6/2011 Alpha Version Rtexttools

#This function takes a document term matrix and produces the appropriate 
#training matrixes and labeling vectors. Requires SparseM package for sparse matrix
#production. Function assumes all data are in a standard n*k dataframe (note: this may change).
#Output is a class object of class "traintest". The train and classify functions take the
#object output as their key arguments.

#Arguments
#dtm -- document term matrix as produced by dtm or some other way
#label -- a vector of the label/code category of the text for training and testing if not on virgin text, or training if on virgin text.
#trainsize -- a sequence (i.e., 1:800) of text itmes used for training.
#testsize -- a sequence (i.e., 801:1000) of text items used for test evaluation. Usually comes from the same dataframe as trainsize.
#extrafeature -- dataframe, matrix, or vector of extra features.
#ef -- TRUE/FALSE logical. Default is false, indicating that no extra features are added.

#Values
#Slot 1: trainmat_sparse -- sparse matrix for training on some algorithms.
#Slot 2: testmat_sparse -- sparse matrix for testing on some algorithms.
#Slot 3: trainpredict -- matrix for training for many algorithms.
#Slot 4: testpredict -- matrix for testing/predicting for many algorithms.
#Slot 5: traincode -- factor vector of training codes/labels.
#Slot 6: test_code -- factor vector of testing/classifying codes/labels.
#Slot 7: train_data_codes -- object of class dataframe, train data plus the labels.
library(SparseM)
traintest <- function(dtm,label,trainsize,testsize,extrafeature,ef=FALSE) {
    
    dtm <-dtm_matrix_clean(dtm) # clean the matrix, get rid of "--aaaab", "ab$.ls", and other non-sensical terms
    train <- dtm[trainsize]
    test <- dtm[testsize]

    train_code <- as.factor(label[trainsize])
    test_code <- as.factor(label[testsize])
   
    if (ef==TRUE) { #Extra Features Branch
    
        if (length(table(complete.cases(extrafeature)))>1) { #Missing Data Issue
            stop ("Extra features cannot have missing data")
            }
        extrafeature_train <- extrafeature[trainsize,]
        extrafeature_test <- extrafeature[testsize,] 
        #Need non-sparse matrix for prediction
        matrix_train_predict <- as.matrix(cbind(as.matrix(train, "matrix.csr"),extrafeature_train))
        matrix_test_predict <- as.matrix(cbind(as.matrix(test, "matrix.csr"),extrafeature_test))
            
    } #Close Extra Features branch
    else { #Regular Matrix Creation Branch
    
    matrix_train_predict <- as.matrix(train, "matrix.csr")
    matrix_test_predict <- as.matrix(test, "matrix.csr")
    } #Close regular matrix branch
    
    #Need sparse matrix for algorithm    
    matrix_train_sparse <- as.matrix.csr(matrix_train_predict)
    matrix_test_sparse <- as.matrix.csr(matrix_test_predict)
    
    #Output Train and Codes
    train_data_codes <- data.frame(matrix_train_predict,train_code)    

    #Create Class for output
    setClass("traintest",representation(trainmat_sparse="matrix.csr",testmat_sparse="matrix.csr",
                                        trainpredict="matrix",testpredict="matrix",
                                        traincode="factor",test_code="factor",
                                        train_data_codes="data.frame"))

    outdata <-  new("traintest", trainmat_sparse=matrix_train_sparse,testmat_sparse=matrix_test_sparse,
                trainpredict=matrix_train_predict,testpredict=matrix_test_predict,
                traincode=train_code,test_code=test_code,train_data_codes=train_data_codes)
    
    return(outdata)   
}


#Regular, No features
#train_test <- traintest(dtm=french_dtm,label=da$Codegros,trainsize=1:400, testsize=401:600)
#names(attributes(train_test))
#Each slot is accessed with "@", example
#a <- train_test@trainpredict
#head(a)
