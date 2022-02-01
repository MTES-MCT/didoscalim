library(devtools)
load_all()

set_work_env()

dataset <- add_dataset(
  title = "Un jeu de données de test",
  description = "Un jeu de données de test",
  frequency = "unknown",
  topic = "Transports"
)

datafile <- add_datafile(
  dataset = dataset,
  title = "Un fichier de données de test",
  description = "Un fichier de données de test",
  file_name = dido_example("augmente.csv"),
  millesime = "2021-12"
)

datafile <- add_datafile(
  dataset = dataset,
  title = "Un fichier de données de test",
  description = "Un fichier de données de test",
  file_name = dido_example("augmente.csv"),
  millesime = "2021-12",
  date_diffusion = "2030-02-02T07:00:02+0100",
  temporal_coverage_start = "2020-01-01",
  temporal_coverage_end = "2020-02-01"
)


attachment <- add_attachment(
  dataset = dataset,
  title = "Un fichier annexe",
  description = "Un fichier annexe",
  file_name = dido_example("attachment.txt")
)

dataset <- add_dataset(
  title = "Un dataset à supprimer",
  description = "Un dataset à supprimer",
  topic = "Transports",
  frequency = "unknown"
)

ds <- add_dataset(
  title = "Données de consommation fictive",
  description = "Données de consommation fictive",
  frequency = "unknown",
  topic = "Transports"
)

