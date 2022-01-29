library(devtools)
load_all()

set_work_env()

dataset <- add_dataset(
  title = "didoscalim base dataset",
  description = "didoscalim base dataset",
  frequency = "unknown",
  topic = "Transports"
)

datafile <- add_datafile(
  dataset = dataset,
  title = "didoscalim base datafile",
  description = "didoscalim base datafile",
  file_name = dido_example("augmente.csv")
)

attachment <- add_attachment(
  dataset = dataset,
  title = "didoscalim base attachment",
  description = "didoscalim base attachment",
  file_name = dido_example("attachment.txt")
)
