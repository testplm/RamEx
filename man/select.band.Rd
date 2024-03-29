% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_value.R
\name{select.band}
\alias{select.band}
\title{Select a range of Raman intensity values for a given wavenumber range}
\usage{
select.band(object, waves)
}
\arguments{
\item{object}{A Ramanome object.}

\item{waves}{A vector with two values representing the lower and upper bounds of the wavenumber range.}
}
\value{
The Raman intensity values within the given wavenumber range.
}
\description{
This function selects a range of Raman intensity values for a given wavenumber range from a Ramanome object.
}
