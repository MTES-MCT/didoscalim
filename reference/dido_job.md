# L'objet dido_job

L'objet dido_job est retourné par les fonctions
[`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md),
[`add_millesime()`](https://mtes-mct.github.io/didoscalim/reference/add_millesime.md)
et
[`replace_millesime()`](https://mtes-mct.github.io/didoscalim/reference/replace_millesime.md),
il peut être utilisé en argument pour les fonctions
[`add_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_dataset.md),
[`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md)

Il a les champs suivant :

- `owner` : l'id du propriétaire du job

- `state` l'état du job. Devrait être "complete" ou "failed"

- `data` les informations liées à la tâche, le champ `dataset_id`

- `result` : le résultat qui comprend lui même les champs :

  - `rid` l'identifiant du fichier de données

  - `duration` le temps de traitement
