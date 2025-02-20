% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slurmSubmit.R
\name{clean.remote}
\alias{clean.remote}
\title{Clean slurmSubmit directory}
\usage{
clean.remote(address = "", target.dir = "~/", sub = Submission())
}
\arguments{
\item{address}{Address of remote to clean}

\item{target.dir}{Path to the parent directory of .slurmsubmit/}

\item{sub}{\linkS4class{Submission} type object containing remote address.}
}
\description{
This function is a helper function to delete the \code{.slurmSubmit} directory created when the
\code{\link{submit}} is run.
}
\details{
Argument \code{address} takes precedence over \code{sub} if specified.
}
\seealso{
\code{\link{submit}}
}
