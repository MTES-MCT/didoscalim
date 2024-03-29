% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dido-s3.R
\name{get_attachment_rid}
\alias{get_attachment_rid}
\title{Retourne l'attachment id de l'objet}
\usage{
get_attachment_rid(data)
}
\arguments{
\item{data}{un objet dido_attachment ou une chaine}
}
\value{
le rid de l'attachment
}
\description{
Retourne l'attachment id de l'objet
}
\examples{
at <- list_attachments()[1, ]
get_attachment_rid(at)
}
\keyword{internal}
