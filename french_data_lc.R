#Loren Collingwood
#R TextTools Experiments

setwd("H:\\Loren")

#Libraries
library(tm) # document term matrix
library("e1071") #svm and other algorithms
library(slam) #not sure if we need this one
library(SparseM) #Converting to Sparse Matrices
memory.size(TRUE)
options(stringsAsFactors=FALSE) # Be sure to set each time or at least in some of the functions


##############################################################
#French Data
##############################################################
#Started: 1/20/2011

french <- read.delim("fn.txt",sep="\t", header=TRUE)[,1:7]

#########################################
#           Grab Text Only              #
#########################################

fr <- VectorSource(french$headline)
system.time(
    french_head <- Corpus(fr,readerControl=list(language="french"))
)

french_corp <- french_head[1:10000]
french_corp <- tm_map(french_corp,stripWhitespace)
french_corp <- tm_map(french_corp,tolower)

system.time(
    french_corp <- tm_map(french_corp, removeWords,stopwords("french"))
)
#Had problems here within CSDE-Terminal Server calling rJava and stuff
#system.time(
#    tm_map(billstrain,stemDocument)
#)
#Create Document Term Matrix : Can probably send to C or Python
system.time(
    french_dtm <- DocumentTermMatrix(french_corp)
)

#######################################################
#           Create Train and Test Sets                #
#######################################################

train <- french_dtm[1:8000]
test <- french_dtm[8001:10000]

#Convert to matrix; sparse matrix
train3 <- as.matrix(train, "matrix.csr")
test3 <- as.matrix(test, "matrix.csr")

#####################################################
#               MAJOR TOPIC CODES                   #
#####################################################

code <- as.factor(french$Codegros)[1:10000]
train_code <- code[1:8000]
test_code <- code[8001:10000]

train_nf <- french$Codegros[1:10000]
train_nf_code <- as.double(train_nf[1:8000])
#####################################################
#                 RUN MODEL -- SVM                  #
#####################################################

system.time(
model1 <- svm(x=train3, y=train_code, method="C-classification",  cost=1000, cross=10)
)
#722.16 Elapsed Seconds!!
#1145.92 Elapsed Seconds! with 80-20 split
#May need to do a regular expression on the matrix names and change them to get rid of X. for the few of them

summary(model1)
pred <- fitted(model1)
table(pred,train_code)

#from Venables and Ripley (2002)
table(true=train_code, predicted = predict(model1))

#Analyze output and TEST
system.time(
predtest <- predict(model1,test3)
)

classAgreement(table(test_code,predtest),match.names=F) # Check accuracy, etc.
#OK -- it worked, now just need to figure out how to improve the accuracy via tuning and such

###########################################################
#                   Possible libraries                    #
###########################################################
#Need to investigate further

train_data <- data.frame(cbind(train_code, train3))

library(mboost) #proportional odds option
a <- mboost(train_code~., data=train_data) #Error: evaluated nested too deeply

library(glmnet)
glmnet(train3, train_code, family = "multinomial") #Taking Too Damn Long

#Still working on
library(RWeka) # Need to fix java issues
library(mvpart) #looks like takes multi-class responses
