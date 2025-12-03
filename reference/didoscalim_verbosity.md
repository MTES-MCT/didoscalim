# Voir et modifier le niveau de message de didoscalim

`didoscalim_verbosity()` retourne l'option nommée
"didoscalim_verbosity", qui détermine la verbosité générale de
didoscalim. Il y a trois niveaux possibles :

- "debug": Niveau pour débugger

- "info" (défaut): Niveau normal d'information utile à l'utilisateur.

- "silent": Aucun message sauf les erreurs.

Si vous souhaitez changer le niveau par défaut, il suffit d'utiliser
`option` :

    options(
      didoscalim_verbosity = "silent",
    )

## Utilisation

``` r
didoscalim_verbosity()
```

## Exemples

``` r
didoscalim_verbosity()
#> [1] "info"
options(
  didoscalim_verbosity = "silent"
)
didoscalim_verbosity()
#> [1] "silent"
```
