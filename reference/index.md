# Index du paquet

## Créer des fichiers CSV augmenté

- [`dido_read_delim()`](https://mtes-mct.github.io/didoscalim/reference/dido_read_delim.md)
  : Lit un fichier CSV
- [`dido_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_csv.md)
  : Génère les lignes d'entête du CSV augmenté
- [`dido_write_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_write_csv.md)
  : Enregistre le fichier CSV augmenté généré

## Les fonctions liées aux datasets

- [`add_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_dataset.md)
  : Ajoute un dataset
- [`add_or_update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_dataset.md)
  : Ajoute ou met à jour un dataset
- [`delete_dataset()`](https://mtes-mct.github.io/didoscalim/reference/delete_dataset.md)
  : Supprime un dataset
- [`dido_dataset`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md)
  : L'objet dido_dataset
- [`get_dataset()`](https://mtes-mct.github.io/didoscalim/reference/get_dataset.md)
  : Récupère les métadonnées d'un dataset
- [`list_datasets()`](https://mtes-mct.github.io/didoscalim/reference/list_datasets.md)
  : Récupère la liste des datasets
- [`update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/update_dataset.md)
  : Met à jour les metadonnées d'un dataset

## Les fonctions liées aux datafiles

- [`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md)
  : Ajoute un datafile à un dataset
- [`add_or_update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_datafile.md)
  : Ajoute ou modifie un datafile
- [`delete_datafile()`](https://mtes-mct.github.io/didoscalim/reference/delete_datafile.md)
  : Supprime un datafile.
- [`dido_datafile`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)
  : L'objet dido_datafile
- [`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md)
  : Récupère un datafile
- [`list_datafiles()`](https://mtes-mct.github.io/didoscalim/reference/list_datafiles.md)
  : Liste les datafiles
- [`update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/update_datafile.md)
  : Mettre à jour un les métadonnées d'un datafile.

## Les fonctions liées aux millésimes

- [`add_millesime()`](https://mtes-mct.github.io/didoscalim/reference/add_millesime.md)
  : Ajoute un millésime à un datafile
- [`delete_millesime()`](https://mtes-mct.github.io/didoscalim/reference/delete_millesime.md)
  : Supprimer un millésime
- [`list_millesimes()`](https://mtes-mct.github.io/didoscalim/reference/list_millesimes.md)
  : Liste les millesimes
- [`replace_millesime()`](https://mtes-mct.github.io/didoscalim/reference/replace_millesime.md)
  : Remplacer les données d'un millésime

## Les fonctions liées aux fichiers annexes

- [`add_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_attachment.md)
  : Ajoute un fichier annexe à un dataset
- [`add_or_update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_attachment.md)
  : Ajoute ou modifie un fichier annexe dans un dataset
- [`delete_attachment()`](https://mtes-mct.github.io/didoscalim/reference/delete_attachment.md)
  : Supprime un attachment
- [`dido_attachment`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md)
  : L'objet dido_attachment
- [`get_attachment()`](https://mtes-mct.github.io/didoscalim/reference/get_attachment.md)
  : Récupère les métadonnées d'un attachment
- [`list_attachments()`](https://mtes-mct.github.io/didoscalim/reference/list_attachments.md)
  : Liste les attachments
- [`replace_attachment()`](https://mtes-mct.github.io/didoscalim/reference/replace_attachment.md)
  : Remplace un fichier annexe
- [`update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/update_attachment.md)
  : Mettre à jour une pièce jointe.

## Les fonctions utilitaires

- [`check_import_file()`](https://mtes-mct.github.io/didoscalim/reference/check_import_file.md)
  : Vérifie un fichier CSV
- [`clean_metadata()`](https://mtes-mct.github.io/didoscalim/reference/clean_metadata.md)
  : Prépare un objet pour l'envoyer à DiDo
- [`my_organization()`](https://mtes-mct.github.io/didoscalim/reference/my_organization.md)
  : Récupère l'ID de l'organisation de l'utilisateur.
- [`list_alerts()`](https://mtes-mct.github.io/didoscalim/reference/list_alerts.md)
  : Récupère les datasets devant être mis à jour
- [`didoscalim_verbosity()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_verbosity.md)
  : Voir et modifier le niveau de message de didoscalim
- [`didoscalim_update_only()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_update_only.md)
  : Bloquer les créations de datasets, datafiles et attachments

## Gérer votre environnement

- [`check_envs()`](https://mtes-mct.github.io/didoscalim/reference/check_envs.md)
  : Vérifier l'accès aux différents environnements
- [`get_work_env()`](https://mtes-mct.github.io/didoscalim/reference/get_work_env.md)
  : Récupère l'environnement utilisé
- [`list_envs()`](https://mtes-mct.github.io/didoscalim/reference/list_envs.md)
  : Liste les environnements configurés
- [`load_envs()`](https://mtes-mct.github.io/didoscalim/reference/load_envs.md)
  : Recharge les environnements
- [`set_work_env()`](https://mtes-mct.github.io/didoscalim/reference/set_work_env.md)
  : Fixe l'environnement DiDo à utiliser

## Les fonctions liées aux jobs

- [`dido_job`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)
  : L'objet dido_job
- [`get_job()`](https://mtes-mct.github.io/didoscalim/reference/get_job.md)
  : Récupére un job en particulier
- [`list_jobs()`](https://mtes-mct.github.io/didoscalim/reference/list_jobs.md)
  : Récupère tous les jobs liés à un utilisateur

## Les fonctions de bas niveau et/ou internes

- [`local_didoscalim_verbosity()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_log.md)
  [`with_didoscalim_verbosity()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_log.md)
  [`didoscalim_debug()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_log.md)
  [`didoscalim_info()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_log.md)
  [`didoscalim_abort()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_log.md)
  : Journalisation et niveau de verbosité
- [`dido_example()`](https://mtes-mct.github.io/didoscalim/reference/dido_example.md)
  : Retourne le chemin du fichier exemple données ou le chemin des
  fichiers exemple
