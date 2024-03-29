% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-attachment.R
\name{add_attachment}
\alias{add_attachment}
\title{Ajoute un fichier annexe à un dataset}
\usage{
add_attachment(
  dataset,
  title,
  description,
  file_name = NULL,
  remote_url = NULL,
  type = "documentation",
  published = format(Sys.time(), "\%Y-\%m-\%d \%H:\%M:\%S")
)
}
\arguments{
\item{dataset}{l'id d'un dataset de rattachement, un objet \code{dido_dataset()},
\code{dido_datafile()} ou \code{dido_job()}}

\item{title}{le titre du fichier annexe}

\item{description}{la description du fichier annexe}

\item{file_name, remote_url}{le nom du fichier à verser ou l'url de l'attachement.}

\item{type}{le type de fichier versé. Peut-être \code{documentation} ou
\code{historical_data}. Par défaut \code{documentation}}

\item{published}{la date/heure de publication du fichier, si non précisée, prend la
date/heure du moment.

Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
préférez). Si la timezone n'est pas précisée, la timezone de l'ordinateur
local est utilisée.}
}
\value{
un objet \code{dido_attachment()}
}
\description{
Ajoute un fichier annexe à un dataset
}
\examples{
library(dplyr, warn.conflicts = FALSE)

dataset <- add_or_update_dataset(
  title = "Des données statistiques",
  description = "Description des données statistiques",
  topic = "Transports",
  frequency = "unknown",
)

dataset \%>\% add_attachment(
  title = "title",
  description = "description",
  file_name = dido_example("attachment.txt"),
  type = 'historical_data'
)

dataset \%>\% add_attachment(
  title = "title",
  description = "Un attachment sous forme de lien externe",
  remote_url = "https://www.lemonde.fr/"
)
}
\seealso{
Other attachment: 
\code{\link{add_or_update_attachment}()},
\code{\link{delete_attachment}()},
\code{\link{dido_attachment}()},
\code{\link{get_attachment}()},
\code{\link{list_attachments}()},
\code{\link{replace_attachment}()},
\code{\link{update_attachment}()}
}
\concept{attachment}
