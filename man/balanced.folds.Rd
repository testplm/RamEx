% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbcs.R
\name{balanced.folds}
\alias{balanced.folds}
\title{Generate balanced folds for cross-validation}
\usage{
balanced.folds(y, nfolds = 10)
}
\arguments{
\item{y}{A vector containing the class labels.}

\item{nfolds}{The number of folds to generate.}
}
\value{
A vector containing the fold assignments for each sample.
}
\description{
This function generates balanced folds for cross-validation, ensuring an equal number of samples from each class in each fold.
}
