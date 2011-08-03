\name{create_matrix}
\alias{create_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
creates a document-term matrix to be passed into create_corpus().
}
\description{
Creates an object of class DocumentTermMatrix from tm that can be used in the create_corpus() function.
}
\usage{
create_matrix(textColumns, language = "en", minDocFreq = 1, 
minWordLength = 3, removeNumbers = FALSE, removePunctuation = TRUE, 
removeSparseTerms = 0, removeStopwords = TRUE, selectFreqTerms = 0, 
stemWords = TRUE, stripWhitespace = TRUE, toLower = TRUE, 
weighting = weightTf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{textColumns}{
Either character vector (e.g. data$Title) or a cbind() of columns to use for training the algorithms (e.g. cbind(data$Title,data$Subject)).
}
  \item{language}{
The language to be used for stemming the text data.
}
  \item{minDocFreq}{
The minimum number of times a word should appear in a document for it to be included in the matrix. See package tm for more details.
}
  \item{minWordLength}{
The minimum number of letters a word should contain to be included in the matrix. See package tm for more details.
}
  \item{removeNumbers}{
A logical parameter to specify whether to remove numbers.
}
  \item{removePunctuation}{
A logical parameter to specify whether to remove punctuation.
}
  \item{removeSparseTerms}{
See package tm for more details.
}
  \item{removeStopwords}{
A logical parameter to specify whether to remove stopwords using the language specified in language.
}
  \item{selectFreqTerms}{
Select the N most frequent terms in each document to use for training.
}
  \item{stemWords}{
A logical parameter to specify whether to stem words using the language specified in language.
}
  \item{stripWhitespace}{
A logical parameter to specify whether to strip whitespace.
}
  \item{toLower}{
A logical parameter to specify whether to make all text lowercase.
}
  \item{weighting}{
Either weightTf or weightTfIdf. See package tm for more details.
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
Timothy P. Jurka <tpjurka@ucdavis.edu>, Loren Collingwood <lorenc2@uw.edu>
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}