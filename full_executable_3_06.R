#Loren Collingwood
#Rtexttools Full Execute Script.
#Replace setwd() with your directory path.

##########################################
#              Set Directory             #
##########################################
setwd("C:/Users/Loren/Documents/My Dropbox/RTextTools/Datasets")
options(stringsAsFactors=FALSE) # Be sure to set each time or at least in some of the functions

##########################################
#     Source Other Functions for now     #
##########################################

functions <-c("accuracy","agreescore","classify_model","confusion_create","cross_validate","datagrab",
              "datainput","dtm","dtm_matrix_clean","multi_text","score_classify",
              "text_to_df","textual_param","train_model","traintest","word_trunc")
#left mysql_in out for now
#still working on cross_validate
for (i in 1:length(functions)) {
    source(paste("C:/Users/Loren/rtexttools/",functions[i],".R",sep=""))
}

##########################################
#       datainput , bring in Data        #
##########################################
da <- datainput("French_news1_csv.csv", type="csv")
#Subset data so it can run on small gb computers, just for testing purposes
fr2 <- da[1:1000,]

##########################################
#          Stopword and Stemming         #
##########################################

textparam_out <- textual_param(textual_vect=fr2$text, language="french")
tm_map(textparam_out, stemDocument)

##########################################
#       Create Document Term Matrix      #
##########################################

french_dtm <- dtm(textparam_out,sparsity=.98) 

##########################################
#       Create train_test object         #
##########################################
#Will train on n=400 documents, test on n=200

#Regular, No features
train_test <- traintest(dtm=french_dtm,label=da$Codegros,trainsize=1:400, testsize=401:600)

#Add in two feature columns
extrafeature <- data.frame(fr2$Code,fr2$Code)
extrafeature[is.na(extrafeature)] <-0 #There is missing data so for now, replace with 0, otherwise won't work
train_test_features <- traintest(dtm=french_dtm,label=da$Codegros,trainsize=1:400, testsize=401:600,extrafeature=extrafeature,ef=TRUE)

##########################################
#           Cross-Validation             #
##########################################
#This process takes some time.
cvsvm <- cross_validate(fnct=svm,train_test=train_test,nfold=3,type="SVM",seed=22)
cvsvm
cvnaive <- cross_validate(fnct=naiveBayes,train_test=train_test,nfold=5,type="NAIVE",seed=423)
cvnaive
cvrf <- cross_validate(fnct=randomForest,train_test=train_test,nfold=3,type="RF",seed=9948)
cvrf
#For glmnet: Folds predictions after the error message "Error in if(outlist$msg!="unknown error") return(outlist): argument is of length zero"
#   are wrong. Ignore those, still trying to figure out how to fix. However, this probably won't happen with large datasets.
cvglmnet <- cross_validate(fnct=glmnet,train_test=train_test,nfold=5,type="GLMNET",seed=2004)
cvglmnet
cvboosting <- cross_validate(fnct=AdaBoostM1,train_test=train_test,nfold=5,type="BOOSTING",seed=199184)
cvboosting
cvbagging <- cross_validate(fnct=Bagging,train_test=train_test,nfold=5,type="BAGGING",seed=1111)
cvbagging
cvtree <- cross_validate(fnct=tree,train_test=train_test,nfold=6,type="TREE",seed=92874)
cvtree
cvnnet <- cross_validate(fnct=nnet,train_test=train_test,nfold=3,type="NNET",seed=3240)
cvnnet

##########################################
#       Train the Models one by one      #
##########################################

#Train the data for each model
SVM <- train_model(train_test=train_test,SVM=TRUE) #sometimes this will produce a warning message
NAIVE <- train_model(train_test=train_test,NAIVE=TRUE)
BOOSTING <- train_model(train_test=train_test,BOOSTING=TRUE)
BAGGING <- train_model(train_test=train_test,BAGGING=TRUE)
RF <- train_model(train_test=train_test,RF=TRUE)
GLMNET <- train_model(train_test=train_test,GLMNET=TRUE)
TREE <- train_model(train_test=train_test,TREE=TRUE)
NNET <- train_model(train_test=train_test,NNET=TRUE)

#Train with Features
SVM_f <- train_model(train_test_features,SVM=TRUE)
NAIVE_f <- train_model(train_test_features,NAIVE=TRUE)
BOOSTING_f <- train_model(train_test_features,BOOSTING=TRUE)
BAGGING_f <- train_model(train_test_features,BAGGING=TRUE)
RF_f <- train_model(train_test_features,RF=TRUE)
GLMNET_f <- train_model(train_test_features,GLMNET=TRUE)
TREE_f <- train_model(train_test_features,TREE=TRUE)
NNET_f <- train_model(train_test_features,NNET=TRUE)

##########################################
#     Classify the Models one by one     #
##########################################

#Classify
SVM_CLASSIFY <- classify_model(train_test=train_test,type="SVM",SVM=SVM)
NAIVE_CLASSIFY <- classify_model(train_test,type="NAIVE",NAIVE=NAIVE)
BOOSTING_CLASSIFY <- classify_model(train_test,type="BOOSTING",BOOSTING=BOOSTING)
BAGGING_CLASSIFY <- classify_model(train_test,type="BAGGING",BAGGING=BAGGING)
RF_CLASSIFY <- classify_model(train_test,type="RF",RF=RF)
GLMNET_CLASSIFY <- classify_model(train_test,type="GLMNET",GLMNET=GLMNET)
TREE_CLASSIFY <- classify_model(train_test,type="TREE",TREE=TREE)
NNET_CLASSIFY <- classify_model(train_test,type="NNET",NNET=NNET)

#Classify with Features
SVM_CLASSIFY_f <- classify_model(train_test=train_test_features,type="SVM",SVM=SVM_f)
NAIVE_CLASSIFY_f <- classify_model(train_test_features,type="NAIVE",NAIVE=NAIVE_f)
BOOSTING_CLASSIFY_f <- classify_model(train_test_features,type="BOOSTING",BOOSTING=BOOSTING_f)
BAGGING_CLASSIFY_f <- classify_model(train_test_features,type="BAGGING",BAGGING=BAGGING_f)
RF_CLASSIFY_f <- classify_model(train_test_features,type="RF",RF=RF_f)
GLMNET_CLASSIFY_f <- classify_model(train_test_features,type="GLMNET",GLMNET=GLMNET_f)
TREE_CLASSIFY_f <- classify_model(train_test_features,type="TREE",TREE=TREE_f)
NNET_CLASSIFY_f <- classify_model(train_test_features,type="NNET",NNET=NNET_f)

##########################################
#       Examine Accuracy Predictions     #
##########################################

algorithm_predictions <- cbind(SVM_CLASSIFY,NAIVE_CLASSIFY,BOOSTING_CLASSIFY,BAGGING_CLASSIFY,
                                RF_CLASSIFY,GLMNET_CLASSIFY,TREE_CLASSIFY,NNET_CLASSIFY)
algorithm_predictions_f <- cbind(SVM_CLASSIFY_f,NAIVE_CLASSIFY_f,BOOSTING_CLASSIFY_f,BAGGING_CLASSIFY_f,
                                 RF_CLASSIFY_f,GLMNET_CLASSIFY_f,TREE_CLASSIFY_f,NNET_CLASSIFY_f)

#Combine into new dataframe with all the old columns of just the tested data.
test_results <- cbind(fr2[401:600,],algorithm_predictions) #fr2 is original data frame
test_resultsf <- cbind(fr2[401:600,],algorithm_predictions_f) 

#Ensemble Consensus
accuracyout <- accuracy("Codegros", c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
                        "glmnet_pred","tree_pred","nnet_pred"), data=test_results)
accuracyout #list the results

accuracyout_f <- accuracy("Codegros", c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
                        "glmnet_pred","tree_pred","nnet_pred"), data=test_resultsf)
accuracyout_f #list the results

#########################################
#  Generate Agreement/Consensus Scores  #
#########################################

scores <- score_classify(test_resultsf,c("svm_pred", "naive_pred", "aboost_pred","bagging_pred","rf_pred",
                        "glmnet_pred","tree_pred","nnet_pred"))
library(gmodels)
#install.packages(gmodels)
CrossTable(scores$scores) #not very much agreement, but extremely small dataset, not surprising.

#Subset the data for just those rows with very high scores. Analysts will need to perform some tests at this point
#to see how accurate the consensus scores are. What is the appropriate cut point for your data? 5 agree, 7 agree?

newdat <- scores[scores$scores>6,]
table(newdat$Codegros,newdat$bagging_pred) # very accurate, all 16 bills agree with original code.

######################################################
#  OUTPUT THE THE DATA CODED DATA WITH PROBABILITIES #
######################################################

write.csv(scores,"scores_french_small_train.csv")
