% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/termination.r
\docType{data}
\name{termination}
\alias{termination}
\title{Termination decrement tables.}
\format{Data frame with 1 row per age per entry age per tablename.
\describe{
\item{tablename}{Unique name identifying the termination table - ordinarily will select on this, character}
\item{age}{Attained age, integer}
\item{ea}{Entry age, integer}
\item{qxt}{Rate of termination at age x and given entry age, numeric}
}}
\source{
Winklevoss Table 2-3, plus other sources (TO COME)\cr
}
\usage{
termination
}
\description{
Selected decrement tables.
}
\examples{
library(dplyr)
glimpse(termination)
count(termination, tablename)
}
\keyword{datasets}

