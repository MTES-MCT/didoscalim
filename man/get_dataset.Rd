% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-dataset.R
\name{get_dataset}
\alias{get_dataset}
\title{Récupère les métadonnées d'un dataset}
\usage{
get_dataset(data)
}
\arguments{
\item{data}{la sortie d'une commande \code{list_datasets()}, un objet
\code{dido_dataset()}, \code{dido_job()} ou \code{dido_datafile()} ou l'identifiant d'un
dataset}
}
\value{
un objet \code{dido_dataset()}
}
\description{
Permet de récupérer les métadonnées d'un dataset en utilisant la sortie de la
fonction \code{list_datasets()} ou un des objets \code{dido_datafile()},
\code{dido_dataset()}, \code{dido_job()}
}
\details{
Lève une exception si \code{list_datasets()} fourni en entrée retourne plus d'un
dataset.
}
\examples{
library(dplyr, warn.conflicts = FALSE)

ds <- list_datasets() \%>\%
  slice(1) \%>\%
  get_dataset()
}
\seealso{
Other dataset: 
\code{\link{add_dataset}()},
\code{\link{add_or_update_dataset}()},
\code{\link{dido_dataset}()},
\code{\link{update_dataset}()}
}
\concept{dataset}
