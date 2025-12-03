# L'objet dido_datafile

L'objet dido_datafile et retourné par
[`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md)
et peut-être utilisé en paramètre par les fonctions de manipulation de
dataset, datafile, millésime et attachement.

Il a les champs suivants :

- `rid` l'identifiant du datafile

- `title`

- `description`

- `millesimes` le nb de millésimes dans le datafile

- `created_at`

- `last_modified`

- `published`

- `url`

- `temporal_coverage` un liste avec deux champs `start` et `end`

- `legal_notice` la licence s'appliquant au fichier de données

- `millesimes_info` comprenant une liste des millésimes rattachés

le dernier champ est supprimé par
[`clean_metadata()`](https://mtes-mct.github.io/didoscalim/reference/clean_metadata.md)

L'objet a également un attribut `id` avec l'identifiant du dataset
auquel il appartient

## Voir également

Other datafile:
[`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md),
[`add_or_update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_datafile.md),
[`delete_datafile()`](https://mtes-mct.github.io/didoscalim/reference/delete_datafile.md),
[`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md),
[`list_datafiles()`](https://mtes-mct.github.io/didoscalim/reference/list_datafiles.md),
[`update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/update_datafile.md)
