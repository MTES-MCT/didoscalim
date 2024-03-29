% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-or-update-attachment.R
\name{add_or_update_attachment}
\alias{add_or_update_attachment}
\title{Ajoute ou modifie un fichier annexe dans un dataset}
\usage{
add_or_update_attachment(
  dataset,
  title,
  description,
  file_name = NULL,
  remote_url = NULL,
  published = format(Sys.time(), "\%Y-\%m-\%d \%H:\%M:\%S"),
  check_file_date = FALSE,
  type = "documentation"
)
}
\arguments{
\item{dataset}{l'id d'un dataset de rattachement, un objet \code{dido_dataset()},
\code{dido_datafile()} ou \code{dido_job()}}

\item{title}{le titre du fichier annexe}

\item{description}{la description du fichier annexe}

\item{file_name, remote_url}{le nom du fichier à verser ou l'url de l'attachement.}

\item{published}{la date/heure de publication du fichier, si non précisée, prend la
date/heure du moment.

Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
préférez). Si la timezone n'est pas précisée, la timezone de l'ordinateur
local est utilisée.}

\item{check_file_date}{TRUE/FALSE, Si TRUE met à jour l'attachment uniquement
si le fichier est plus récent que le last_modified de l'attachment}

\item{type}{le type de fichier versé. Peut-être \code{documentation} ou
\code{historical_data}. Par défaut \code{documentation}}
}
\value{
un objet \code{dido_attachment()} ou \code{NULL} si aucune création/mise à jour
n'a eu lieu
}
\description{
met à jour l'attachement avec le même titre s'il existe sinon ajoute un attachement.

Si vous devez mettre à jour des attachments, il est conseillé d'utiliser la
fonction \code{didoscalim_update_only()}, cf
\code{vignette("complements-sur-les-mises-a-jour")}
}
\examples{
library(dplyr, warn.conflicts = FALSE)

dataset <- add_or_update_dataset(
  title = "Un dataset pour les attachement",
  description = "Description des données statistiques",
  topic = "Transports",
  frequency = "unknown",
)

add_or_update_attachment(
  dataset = dataset,
  title = "title",
  description = "description",
  file_name = dido_example("attachment.txt")
)

add_or_update_attachment(
  dataset = dataset,
  title = "title",
  description = "description",
  remote_url = "https://www.lemonde.fr"
)
}
\seealso{
Other attachment: 
\code{\link{add_attachment}()},
\code{\link{delete_attachment}()},
\code{\link{dido_attachment}()},
\code{\link{get_attachment}()},
\code{\link{list_attachments}()},
\code{\link{replace_attachment}()},
\code{\link{update_attachment}()}
}
\concept{attachment}
