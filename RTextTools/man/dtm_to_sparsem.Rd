\name{dtm_to_sparsem}
\alias{dtm_to_sparsem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
converts a tm Document-Term Matrix to a SparseM matrix.csr.
}
\description{
Takes a DocumentTermMatrix() from the tm package and converts it directly as.matrix.csr() from the SparseM package, without the intermediate step of converting as.matrix().
}
\usage{
dtm_to_sparsem(dtm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dtm}{
An object of class DocumentTermMatrix or TermDocumentMatrix from package tm.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an object of class matrix.csr from package SparseM.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Timothy P. Jurka <tpjurka@ucdavis.edu>
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
sparsem <- dtm_to_sparsem(matrix)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}