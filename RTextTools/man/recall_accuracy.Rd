\name{recall_accuracy}
\alias{recall_accuracy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculates the recall accuracy of the classified data.
}
\description{
Given the true labels to compare to the labels predicted by the algorithms, calculates the recall accuracy of each algorithm.
}
\usage{
recall_accuracy(true_labels, predicted_labels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{true_labels}{
A vector containing the true labels, or known values for each document in the classification set.
}
  \item{predicted_labels}{
A vector containing the predicted labels, or classified values for each document in the classification set.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Loren Collingwood <lorenc2@uw.edu>, Timothy P. Jurka <tpjurka@ucdavis.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(RTextTools)
data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv")
data <- data[sample(1:3100,size=1000,replace=FALSE),]
matrix <- create_matrix(cbind(data$Title,data$Subject), language="english", 
removeNumbers=TRUE, stemWords=FALSE, weighting=weightTfIdf)
corpus <- create_corpus(matrix,data$Topic.Code,trainSize=1:750, testSize=751:1000, 
virgin=FALSE)
models <- train_models(corpus, algorithms=c("GLMNET","MAXENT","SVM"))
results <- classify_models(corpus, models)
analytics <- create_analytics(corpus, results)
recall_accuracy(analytics@document_summary$MANUAL_CODE,
analytics@document_summary$GLMNET_LABEL)
recall_accuracy(analytics@document_summary$MANUAL_CODE,
analytics@document_summary$MAXENTROPY_LABEL)
recall_accuracy(analytics@document_summary$MANUAL_CODE,
analytics@document_summary$SVM_LABEL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}