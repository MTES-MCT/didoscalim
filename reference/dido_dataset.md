# L'objet dido_dataset

L'objet dido_dataset est retourné par
[`get_dataset()`](https://mtes-mct.github.io/didoscalim/reference/get_dataset.md)
et
[`add_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_dataset.md)
et utilisé en paramètre par
[`update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/update_dataset.md),
[`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md)
et `create_attachment()`.

Il a les champs suivants :

- `id` l'identifiant du dataset

- `title`

- `description`

- `organization`

- `topic` le thème

- `license`

- `frequency` la fréquence

- `frequency_date` prochaine mise à jour

- `tags` une liste de mots clefs

- `spatial` une liste avec des champs, la `granularity` et `zones` une
  liste de zones

- `temporal_coverage` un liste avec deux champs `start` et `end`

- `created_at` la date de création

- `last_modified`

- `last_update`

- `attachments` une liste des attachments

- `datafiles` une liste des fichiers de données

les deux derniers champs sont supprimés par
[`clean_metadata()`](https://mtes-mct.github.io/didoscalim/reference/clean_metadata.md)

## Voir également

[`add_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_dataset.md)

Other dataset:
[`add_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_dataset.md),
[`add_or_update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_dataset.md),
[`get_dataset()`](https://mtes-mct.github.io/didoscalim/reference/get_dataset.md),
[`update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/update_dataset.md)
