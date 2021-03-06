% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Likelihood.R
\name{Likelihood}
\alias{Likelihood}
\alias{Print}
\title{Rasch Model Likelihood}
\usage{
Likelihood(raschObj = "Rasch", theta = "numeric")
}
\arguments{
\item{raschObj}{an object of class Rasch}

\item{theta}{A numeric object}
}
\value{
A numeric
 \item{likelihood}{The likelihood of a respond having a proposed ability score given their answers}
}
\description{
Calculates the Likelihood of a proposed "ability" given the subjects answers
}
\note{
Theta value can be manually chosen.
}
\examples{

testRasch = new("Rasch", name = "Benny", difficulty = 1:10, answers = sample(0:1, 10, replace=TRUE))
Likelihood(raschObj = testRasch, theta = .5)
}
\seealso{
\code{\link{raschClass}}
}
\author{
Kalen Davison: \email{davison@wustl.edu}
}
