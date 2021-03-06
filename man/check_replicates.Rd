% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_replicates.R
\name{check_replicates}
\alias{check_replicates}
\title{Checks for replicates in a nested design}
\usage{
check_replicates(df, ...)
}
\arguments{
\item{df}{a data frame}

\item{...}{variables of the data frame specifying the survey design. Should
be the combination of strata first, then clusters from outer to inner level
(i.e. primary sampling unit, then secondary sampling unit, etc...)}
}
\description{
Check for replicates in a nested design where the different levels can be
strata and/or clusters.
}
\details{
All the strata combinations should be combined into one stratum.
}
\examples{
data(tz7)
## in this data set the combination of strata is defined by the variable v023,
## and the primary and secondary sampling units are defined by the variables
## v001 and v002 respectively.
check_replicates(tz7, v023, v001, v002)
## we can see here that the primary sampling unit 142 of the combination of
## strata 13 lacks a replication of secondary sampling unit.

}
\author{
Marc Choisy
}
