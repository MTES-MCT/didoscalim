# Ajoute un dataset

Ajoute un dataset

## Utilisation

``` r
add_dataset(
  title,
  description,
  topic,
  frequency,
  tags = NULL,
  frequency_date = NULL,
  granularity = NULL,
  zones = NULL,
  organization = my_organization(),
  license = "fr-lo",
  temporal_coverage_start = NULL,
  temporal_coverage_end = NULL,
  caution = NULL
)
```

## Arguments

- title:

  le titre du dataset

- description:

  la description du dataset

- topic:

  le thème du dataset. Doit être dans "Environnement", "Énergie",
  "Transports", "Logement", "Changement climatique"

- frequency:

  la fréquence de publication des données. Les fréquences les plus
  utilisées sont `annual`, `monthly`, `ponctual`, `unknown`. [la liste
  complète des
  fréquences](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/metadata/frequencies)

- tags:

  la liste des mots clefs. Si le fichier est déjà publié sur le site
  SDES, on reprend les mêmes. La liste des [mots clefs
  recensés](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/referentiels/tags/csv?withColumnName=true&withColumnDescription=false).

- frequency_date:

  la date de prochaine publication. optionnel seulement la `frequency`
  est `unknown`.

- granularity:

  la granularité du jeu de données. Les plus utilisés sont "fr:region",
  "fr:departement", "fr:epci", "fr:commune", "fr:iris" et pour les
  données à l'adresse "poi". [La liste complète des
  granularités](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/metadata/granularities)

- zones:

  la zone couverte par le jeu de données. `country:fr` pour France
  entière, `country-subset:fr:metro` pour la métropole et
  \`country-subset:fr:drom“ pour les DROM

- organization:

  l'id de l'organisation sous laquelle vous souhaitez publier le
  dataset, si vous n'appartenez qu'à une seule organisation, vous n'avez
  pas à remplir ce champ, didoscalim la prendra par défaut.

  Si ça n'est pas le cas, vous devez préciser la fonction
  [`my_organization()`](https://mtes-mct.github.io/didoscalim/reference/my_organization.md)

- license:

  la licence des données. "fr-lo" par défaut

- temporal_coverage_start:

  optionnel, la date de début de couverture du jeux de données au format
  AAAA-MM-JJ

- temporal_coverage_end:

  optionnel, la date de fin de couverture du jeux de données au format
  AAAA-MM-JJ

- caution:

  Les précautions à prendre avec ce jeu de données. Par exemple :
  "certaines données peuvent être secrétisées"

## Valeur de retour

un objet
[`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md).
Ce dernier sert pour créer par la suite les datafiles et les millésimes.

## Voir également

La liste des [mots clefs
recensés](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/referentiels/tags/csv?withColumnName=true&withColumnDescription=false)

Other dataset:
[`add_or_update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_dataset.md),
[`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md),
[`get_dataset()`](https://mtes-mct.github.io/didoscalim/reference/get_dataset.md),
[`update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/update_dataset.md)

## Exemples

``` r
dataset <- add_dataset(
  title = "Données logements",
  description = "Données logement en date réelle",
  topic = "Transports",
  frequency = "unknown"
)
#> dataset `Données logements` créé

dataset <- add_dataset(
  title = "Données parc de véhicule",
  description = "Données parc de véhicule",
  topic = "Transports",
  tags = list("transport", "vehicule", "circulation"),
  frequency = "annual",
  organization = my_organization(),
  frequency_date = "2022-12-31",
  temporal_coverage_start = "2021-01-01",
  temporal_coverage_end = "2021-12-31",
  granularity = "fr:region",
  zones = "country:fr",
  caution = "caution",
  license = "ODbL-1.0"
)
#> dataset `Données parc de véhicule` créé
```
