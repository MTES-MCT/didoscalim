% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update-attachment.R
\name{update_attachment}
\alias{update_attachment}
\title{Mettre à jour une pièce jointe.}
\usage{
update_attachment(attachment)
}
\arguments{
\item{attachment}{un objet attachment retourné par \code{get_attachment()} modifié
par l'utilisateur}
}
\value{
un objet \code{dido_attachment()}
}
\description{
Mettre à jour une pièce jointe.
}
\examples{
att <- list_attachments()[1, ]
dataset <- att$id
attachment <- get_attachment(list_attachments()[1, ], dataset = dataset)
attachment$title <- "un nouveau titre"
update_attachment(attachment)
}
\seealso{
Other attachment: 
\code{\link{add_attachment}()},
\code{\link{add_or_update_attachment}()},
\code{\link{delete_attachment}()},
\code{\link{dido_attachment}()},
\code{\link{get_attachment}()},
\code{\link{list_attachments}()},
\code{\link{replace_attachment}()}
}
\concept{attachment}
