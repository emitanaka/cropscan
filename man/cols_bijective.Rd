% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality-checks.R
\name{cols_bijective}
\alias{cols_bijective}
\title{Identify bijective (one-to-one correspondence) columns}
\usage{
cols_bijective(data)
}
\arguments{
\item{data}{The data frame}
}
\value{
A list of character vectors of bijective columns.
}
\description{
Any values that have all unique values will not be included in the
output.
}
