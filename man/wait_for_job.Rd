% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobs.R
\name{wait_for_job}
\alias{wait_for_job}
\title{Attend la fin d'un job d'intégration}
\usage{
wait_for_job(data)
}
\arguments{
\item{data}{l'id du job}
}
\value{
les données du job
}
\description{
Attend la fin d'un job d'intégration
}
\examples{
job <- list_jobs()[1, ] \%>\%
  wait_for_job()
}
\keyword{internal}
