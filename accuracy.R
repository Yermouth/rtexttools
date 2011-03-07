###################################################################
#                           accuracy                              #
###################################################################
#Loren Collingwood , 3/02/2011, Alpha Version Rtexttools 
#This function calculates the diagonal accuracy of an algorithm against the 
#"true" handcode or pre-existing label. In addition it produces the Kappa Diagonal,
#which corrects for agreement by chance; the rand index, and the corrected rand index.

#prelabel -- the handcode/pre-existing label for the dataset, must be in "character" format
#columnames -- character vector of columnames of the algorithms in the dataset.
#data -- the dataframe from which the labels are stored. So the original data + appended algorithm score columns
library(e1071)

accuracy <- function(prelabel,columnames,data) {

    Call <- match.call()
    indx <- match(c("prelabel", "columnames", "data"), names(Call), 
        nomatch = 0)
    if (indx[1] == 0) 
        stop("A numeric vector must be included")
    if (indx[2] == 0) 
        stop("A character vector must be included")    
    if (indx[3] == 0) 
        stop("A data frame must be included")    

    dat <- data
    
    #Extract just the columns you want from the data
    datmat <- as.matrix(dat[match(c(prelabel,columnames), colnames(dat))])
    out <- NULL
    out2 <- NULL

    if (length(columnames)<2) { #One model training
        #Confusion Matrix Creation Modified from here: http://www.mail-archive.com/r-help@stat.math.ethz.ch/msg94942.html
        tr <- as.numeric(datmat[,1])
        pr<-as.numeric(datmat[,2])

        cat <- confusion_create(tr,pr) #Internal "confusion_create" function used here
        
        out<- classAgreement(cat) #use the classAgreemnt function from e1071 package
        finalout <- c(out$diag,out$kappa, out$rand, out$crand)
        finalout <- data.frame(t(finalout)) #put into data frame for columnaming
     }   #Close One model train
     
     else { #Multiple columns of Vectors/ Multiple models trained
   
        #use classAgreement function to extract diag scores, kappa, rand index, and rand index corrected
        #Note, first column of datmat is "Pre-existing code", that's why we skip it here
   
         for (i in 2:ncol(datmat)) {
      
            #Need to create a confusion matrix to take diagonals. Because not all labels are predicted, 
            #need to add in columns of zeroes.
            tr <- as.numeric(datmat[,1])
            pr<-as.numeric(datmat[,i])
            
            cat <- confusion_create(tr,pr) #Internal "confusion_create" function used here     
            out[[i]]<- classAgreement(cat) #use the classAgreemnt function from e1071 package
            out2[[i]] <- c(out[[i]]$diag,out[[i]]$kappa, out[[i]]$rand, out[[i]]$crand)
        
            }

            #Rbind the rows together with a loop
            finalout <- out2[[2]] # note, first list item is blank (because we skipped it)
            for (i in 3:length(out2)){
                finalrotate <- rbind(finalout, out2[[i]])
                finalout <- finalrotate
            }
        } #end else multiple column vector branch

        colnames(finalout) <- c("Diag_Predict", "Kappa_Diag", "Rand_Index", "C_Rand_Index")
        rownames(finalout) <- columnames
    
        finalout <- finalout[rev(order(finalout[,'Diag_Predict'])),] #sort matrix
        return(round(finalout,3)) #round to 3 decimal points
}

#Examine Accuracy Predictions
#algorithm_predictions <- cbind(SVM_CLASSIFY,NAIVE_CLASSIFY,BOOSTING_CLASSIFY,BAGGING_CLASSIFY,
#                                RF_CLASSIFY,GLMNET_CLASSIFY,TREE_CLASSIFY,NNET_CLASSIFY)
#test_resultsf <- cbind(fr2[401:600,],algorithm_predictions_f) 
#accuracyout <- accuracy("Codegros", c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
#                        "glmnet_pred","tree_pred","nnet_pred"), data=test_results)
#accuracyout #list the results
