% Generated by roxygen2 (4.0.2): do not edit by hand
\name{stratifyByPsAndCovariates}
\alias{stratifyByPsAndCovariates}
\title{Stratify persons by propensity score and other covariates}
\usage{
stratifyByPsAndCovariates(data, numberOfStrata = 5, cohortData, covariateIds)
}
\arguments{
\item{data}{A data frame with the three columns described below}

\item{numberOfStrata}{Into how many strata should the propensity score be divided? The boundaries
of the strata are automatically defined to contain equal numbers of treated persons.}

\item{cohortData}{An object of type \code{cohortData} as generated using \code{getDbCohortData}.}

\item{covariateIds}{One or more covariate IDs in the \code{cohortData} object on which subjects
should also be stratified.}
}
\value{
Returns a date frame with the same columns as the input data plus one extra column: stratumId.
}
\description{
\code{stratifyByPsAndCovariates} uses the provided propensity scores and covariatesto stratify
persons.
}
\details{
The data frame should have the following three columns:
\tabular{lll}{
  \verb{rowId}              \tab(integer) \tab A unique identifier for each row (e.g. the person ID) \cr
  \verb{treatment}           \tab(integer) \tab Column indicating whether the person is in the treated (1) or comparator (0) group  \cr
  \verb{propensityScore}    \tab(real)    \tab Propensity score \cr
}
}
\examples{
#todo
}
