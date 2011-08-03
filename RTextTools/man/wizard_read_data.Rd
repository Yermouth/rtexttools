\name{wizard_read_data}
\alias{wizard_read_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a simplified function for reading data from files.
}
\description{
A simple interface for reading in data from files and creating a corpus all in one step.
}
\usage{
wizard_read_data(filename, tablename = NULL, filetype = "csv", 
virgin=FALSE, textColumns, codeColumn, trainSize, testSize, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character string of the name of the file, include path if the file is not located in the working directory.
}
  \item{tablename}{
Microsoft Access database only. The table name in the database.
}
  \item{filetype}{
Character vector specifying the file type. Options include "csv","tab","accdb","mdb" to denote .csv files, text files, or Access databases.
}
  \item{textColumns}{
The a cbind() of column(s) to use for training the algorithms (e.g. cbind(data$Title)).
}
  \item{codeColumn}{
A factor or vector of labels corresponding to each document in the matrix.
}
  \item{trainSize}{
A range (e.g. 1:1000) specifying the number of documents to use for training the models.
}
  \item{testSize}{
A range (e.g. 1:1000) specifying the number of documents to use for classification.
}
  \item{\dots}{
Other parameters to be passed on to create_matrix().
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A corpus like the one returned by create_corpus() that can be used in train_model(), train_models(), classify_model(), and classify_models().
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Wouter Van Atteveldt <wouter@vanatteveldt.com>, Timothy P. Jurka <tpjurka@ucdavis.edu>
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
corpus <- wizard_read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"), 
textColumns=c("Title","Subject"), codeColumn="Topic.Code", trainSize=1500, 
testSize=400, virgin=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}