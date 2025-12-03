# Compléments sur les mises à jour

``` r
library(didoscalim)
#> ✖ Environnement DiDo actif : DEV
```

## Comment fonctionne les fonctions `add_or_update_*`

Les fonctions `add_or_update_*` recherche l’objet en comparant le
paramètre `title` que vous passez avec les titres des objets.

Avant comparaison, le paramètre `title` et les titres sont passés en
minuscule en conservant uniquement les caractères alphanumériques (y
compris accentués). La chaine “UN É & UN À.” deviendra donc “unéunà”.

``` r
dataset1 <- add_or_update_dataset(
  title = "données sur l'évolution des eaux de surface",
  description = "Un jeu de données de test",
  frequency = "unknown",
  topic = "Transports"
)
#> dataset `données sur l'évolution des eaux de surface` créé

dataset2 <- add_or_update_dataset(
  title = "Données sur l'évolution des eaux de surface.",
  description = "Un jeu de données de test",
  frequency = "unknown",
  topic = "Logement"
)
#> dataset "Données sur l'évolution des eaux de surface." modifié
dataset1$id == dataset2$id
#> [1] TRUE

dataset1$title
#> [1] "données sur l'évolution des eaux de surface"
dataset1$topic
#> [1] "Transports"

dataset2$title
#> [1] "Données sur l'évolution des eaux de surface."
dataset2$topic
#> [1] "Logement"
```

## Méthodes avancées

La fonction de comparaison de base ne permet de modifier le titre d’un
objet qu’à la marge (transformer une minuscule en majuscule, ajouter ou
supprimer une ponctuation…).

Si, pour une raison quelconque, vous avez besoin de modifier ce titre de
façon plus importante (par exemple pour corriger un titre), vous pouvez
remplacer temporairement la méthode de base par votre propre méthode de
comparaison en fixant l’option `didoscalim_title_comparison`.

La méthode s’applique sur la chaine “nettoyée” (passage en minuscule et
conservation des caractères alphanumériques).

En exemple, la fonction suivante, qui s’appuie sur la librairie
{stringdist}, permet de rechercher les titres qui ont une différence de
3 caractères maximum :

``` r
library(withr)
library(stringdist)

withr::local_options(list(didoscalim_title_comparison = function(column, string) {
  stringdist(column, string) <= 3
}))

dataset1 <- add_or_update_dataset(
  title = "données sur l'évolution des espèce",
  description = "Un jeu de données de test",
  frequency = "unknown",
  topic = "Transports"
)
#> dataset `données sur l'évolution des espèce` créé

dataset2 <- add_or_update_dataset(
  title = "Données sur l'évolution des espèces.",
  description = "Un jeu de données de test",
  frequency = "unknown",
  topic = "Transports"
)
#> dataset "Données sur l'évolution des espèces." modifié

dataset1$id == dataset2$id
#> [1] TRUE
dataset1$title
#> [1] "données sur l'évolution des espèce"
dataset2$title
#> [1] "Données sur l'évolution des espèces."
```

Dans ce cas, il est **fortement** conseillé d’utiliser l’option
`didoscalim_update_only` afin d’éviter de créer des objets par erreur.

``` r

didoscalim_update_only(TRUE)
#> ℹ Création de nouveaux objets : interdite
                    
dataset <- add_or_update_dataset(
  title = "Données sur l'évolution des écosystèmes.",
  description = "Un jeu de données de test",
  frequency = "unknown",
  topic = "Transports"  
)
#> Error in `abort_update_only()`:
#> ! aucun dataset ne correspond au titre 'Données sur l'évolution des écosystèmes.'
#> ℹ l'option `didoscalim_update_only` est positionnée à `TRUE`.
```
