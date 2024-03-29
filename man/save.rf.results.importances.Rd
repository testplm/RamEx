% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbcs.R
\name{save.rf.results.importances}
\alias{save.rf.results.importances}
\title{Save feature importance scores of random forest classification results}
\usage{
save.rf.results.importances(
  result,
  feature.ids,
  filename = "feature_importance_scores.xls",
  outdir
)
}
\arguments{
\item{result}{The result object obtained from random forest classification}

\item{feature.ids}{A vector of feature identifiers}

\item{filename}{The name of the output file}

\item{outdir}{The path to the output directory}
}
\value{
None
}
\description{
This function saves the feature importance scores of random forest classification results to a file.
}
\examples{
save.rf.results.importances(result, feature.ids, filename = 'feature_importance_scores.xls', outdir)

}
