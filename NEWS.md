# didoscalim devel

documentation :

* ajout d'un passage sur comment limiter le temps de la méthode `dido_csv` quand
  on utilise de fichier source volumineux dans la vignette 
  `vignettes/csv-augmente.Rmd`

# didoscalim 0.1.3.9000

Cette version ajoute une option `replace` au paramètre `on_existing_millesime`
de la méthode `add_or_update_datafile` qui permet de remplacer le millésime en
conservant le même identifiant de millésime.

évolutions :

* ajout d'une option `replace` au paramètre `on_existing_millesime`

bugfixes :

* corrige des incohérences sur les paramètres de type date/times (`published` et `date_diffusion`).

  Ces dates times sont maintenant supportés en "AAAA-MM-JJ HH:MM:SS" par défaut
  même si tous les formats ISO8601 sont supportés.
* supprime les messages d'avertissement de conversion de time zone

documentation :

* supprime une vignette obsolète.

divers :

* utilise une expectation expect_datetime pour vérifier les datetimes
* ajoute une barre de progression pour l'upload d'un fichier

# didoscalim 0.1.2.9000

bugfix :

* correction d'un bug qui empèche l'installation quand l'environnement n'est pas configuré

# didoscalim 0.1.1.9000

bugfixes :

* correction d'un bug sur les mises à jour de zones et de granularité.

# didoscalim 0.1.0.9000

Cette nouvelle version ajoute les méthodes de haut niveau :

* `add_or_update_dataset()`, `add_or_update_datafile()` et
`add_or_update_attachment()` qui permettent de mettre à jour respectivement un
dataset, datafile ou un attachment, s'il existe et de l'ajouter s'il n'existe
pas.
* `check_import_file()` qui permet de tester rapidement si un fichier de données
est valide au sens de DiDo.

évolutions :

* ajout des méthodes add_or_update_*
* ajout d'une méthode `check_import_file` pour valider un fichier
* `dido_csv()` permet de changer le nom de la variable
* ajout des méthodes delete-*

bugfixes :

* correction d'un bug sur les noms de variables avec une espace
* correction d'un bug dans les tests des temporal_coverage retournés par l'API
* ajout de retry des requêtes en cas d'erreur 500
* erreur d'argument dans replace_attachment
* bug sur des champs supplémentaires dans la route update de l'API

divers :

* ajout de la version de didoscalim dans le user agent
* refactor: utilise les options pour l'environnement utilisé
* refactor: ajout de helpers : abort_if_not_one_line, check_mandatory_arguments
* ajout de tests

# didoscalim 0.0.3.9000

* bugfix dans la gestion de la timezone de la date de diffusion
* refactor la gestion de dates + styler
* documentation

# didoscalim 0.0.2.9000

* ajout des paramètres expressions rationnelles dans `dido_csv()`
* ajout de l'évaluation `{}` dans le champ `description`
* documentation de toutes les fonctions
* tous les exemples sont testés
* bugfix: erreur quand le dataframe passée à dido_csv contient des colonnes autre que chr

# didoscalim 0.0.1.9000

Première version publiée
