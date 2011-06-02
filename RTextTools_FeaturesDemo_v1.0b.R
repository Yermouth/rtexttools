# FILE: RTextTools_FeaturesDemo_v1.0b
# AUTHOR(s): Loren Collingwood, Timothy Jurka
# DATE: April 2011
# DESCRIPTION: R demo for the RTextTools library.

library(RTextTools)

# Read your data into R.
data <- read_data("/Users/timjurka/Dropbox/RTextTools/Datasets/SpanishData_MediaForAmber.csv", type="csv")

# Define your sample size, and randomly sample to create a random training and test set.
data_subset <- data[sample(1:30000,size=10000,replace=FALSE),]

# Create a term frequency matrix for the training/classification process.
data_matrix <- create_matrix(data_subset$TITLE, language="spanish", removeNumbers=TRUE, minWordLength=4)

# Create a container that combines your term frequency matrix and the corresponding training codes.
data_container <- create_corpus(data_matrix,data_subset$CODIGO,trainSize=1:7500, testSize=7501:10000)

# Cross-validation of algorithm training on your data subset.
# SVM and MAXENT are the low-memory algorithms.
cvsvm <- cross_validate(fnct=svm,data_container,nfold=3,type="SVM",seed=22)
cvsvm

#cvnaive <- cross_validate(fnct=naiveBayes,data_container,nfold=5,type="NAIVE",seed=423)
#cvnaive

#cvrf <- cross_validate(fnct=randomForest,data_container,nfold=3,type="RF",seed=9948)
#cvrf

#cvglmnet <- cross_validate(fnct=glmnet,data_container,nfold=5,type="GLMNET",seed=2004)
#cvglmnet

#cvboosting <- cross_validate(fnct=AdaBoostM1,data_container,nfold=5,type="BOOSTING",seed=199184)
#cvboosting

#cvbagging <- cross_validate(fnct=Bagging,data_container,nfold=5,type="BAGGING",seed=1111)
#cvbagging

#cvtree <- cross_validate(fnct=tree,data_container,nfold=6,type="TREE",seed=92874)
#cvtree

#cvnnet <- cross_validate(fnct=nnet,data_container,nfold=3,type="NNET",seed=3240)
#cvnnet

cvmaxent <- cross_validate(fnct=maxent,data_container,nfold=3,type="MAXENT",seed=7310)
cvmaxent


# Train each model with the given data.
# For the demo we will run the low-memory algorithms: SVM and MAXENT

SVM <- train_model(data_container,SVM=TRUE)
#NAIVE <- train_model(data_container,NAIVE=TRUE)
#BOOSTING <- train_model(data_container,BOOSTING=TRUE)
#BAGGING <- train_model(data_container,BAGGING=TRUE)
#RF <- train_model(data_container,RF=TRUE)
#GLMNET <- train_model(data_container,GLMNET=TRUE)
#TREE <- train_model(data_container,TREE=TRUE)
#NNET <- train_model(data_container,NNET=TRUE)
MAXENT <- train_model(data_container,MAXENT=TRUE)

# Classify the samples using the models trained above.
SVM_CLASSIFY <- classify_model(data_container,SVM=SVM)
#NAIVE_CLASSIFY <- classify_model(data_container,NAIVE=NAIVE)
#BOOSTING_CLASSIFY <- classify_model(data_container,BOOSTING=BOOSTING)
#BAGGING_CLASSIFY <- classify_model(data_container,BAGGING=BAGGING)
#RF_CLASSIFY <- classify_model(data_container,RF=RF)
#GLMNET_CLASSIFY <- classify_model(data_container,GLMNET=GLMNET)
#TREE_CLASSIFY <- classify_model(data_container,TREE=TREE)
#NNET_CLASSIFY <- classify_model(data_container,NNET=NNET)
MAXENT_CLASSIFY <- classify_model(data_container,MAXENT=MAXENT)


# Combine the results from all the models into one list.
analytics <- create_analytics(data_container,cbind(SVM_CLASSIFY,MAXENT_CLASSIFY))

analytics@document_summary
analytics@topic_summary
analytics@algorithm_summary
analytics@score_summary

#write.csv(analytics@document_summary,"SpanishData_DocumentSummary.csv")
#write.csv(analytics@topic_summary,"SpanishData_TopicSummary.csv")
#write.csv(analytics@algorithm_summary,"SpanishData_AlgorithmSummary.csv")
#write.csv(analytics@score_summary,"SpanishData_ScoreSummary.csv")