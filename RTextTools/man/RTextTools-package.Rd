\name{RTextTools-package}
\alias{RTextTools-package}
\docType{package}
\title{RTextTools Machine Learning}
\description{
RTextTools is an R machine learning library for text classification. The goal of RTextTools is to make it easy for social scientists to get started with machine learning, while allowing power-users the freedom to experiment with different settings and algorithm combinations without requiring extensive programming experience.
}
\details{
\tabular{ll}{
Package: \tab RTextTools\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-07-24\cr
License: \tab Gnu Public License\cr
LazyLoad: \tab yes\cr
}
Using RTextTools can be broken down into five simple steps. First, read your data into R as a data frame using the included read_data() function or any other method. Next, create the document term matrix from your textual documents using create_matrix(), and create a container of these sparse matrices and labels with create_corpus(). This object will then be input to both train_model() and classify_model(), which respectively train and classify the textual data. Alternatively, you may use train_models() and classify_models() to train and classify using multiple algorithms at once. You may use print_algorithms() to see a list of available algorithms. Last, use create_analytics() to analyze the results and determine accuracy rates as well as to prepare the ensemble agreement.
}
\author{
Timothy P. Jurka, Loren Collingwood, Wouter Van Attevelt

Maintainer: <tpjurka@ucdavis.edu>, <lorenc2@uw.edu>, <wouter@vanatteveldt.com>

}
\references{

}

\examples{
%~~ simple examples of the most important functions ~~
}
