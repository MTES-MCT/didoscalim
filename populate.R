library(devtools)
load_all()

set_work_env()

dataset <- add_or_update_dataset(
  title = "Un jeu de données de test",
  description = "Un jeu de données de test",
  frequency = "unknown",
  topic = "Transports"
)

datafile <- add_or_update_datafile(
  dataset = dataset,
  title = "Un fichier de données de test",
  description = "Un fichier de données de test",
  file_name = dido_example("augmente.csv"),
  millesime = "2021-12",
  on_existing_millesime = "skip"
)

attachment <- add_or_update_attachment(
  dataset = dataset,
  title = "Un fichier annexe",
  description = "Un fichier annexe",
  file_name = dido_example("attachment.txt")
)

attachment <- add_or_update_attachment(
  dataset = dataset,
  title = "Un autre fichier annexe",
  description = "Un fichier annexe",
  file_name = dido_example("augmente.csv")
)

add_or_update_dataset(
  title = "Un dataset à supprimer",
  description = "Un dataset à supprimer",
  topic = "Transports",
  frequency = "unknown"
)

add_or_update_dataset(
  title = "Données de consommation fictive",
  description = "Données de consommation fictive",
  frequency = "unknown",
  topic = "Transports"
)
