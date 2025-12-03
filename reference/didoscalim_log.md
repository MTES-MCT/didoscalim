# Journalisation et niveau de verbosité

Des méthodes liées à la gestion de la verbosité et à la journalisation

## Utilisation

``` r
local_didoscalim_verbosity(level, env = parent.frame())

with_didoscalim_verbosity(level, code)

didoscalim_debug(text, .envir = parent.frame())

didoscalim_info(text, .envir = parent.frame())

didoscalim_abort(
  message = NULL,
  ...,
  class = NULL,
  .envir = parent.frame(),
  call = caller_env()
)
```

## `local_didoscalim_verbosity`

change le niveau de verbosité dans la suite du code au même scope

## `with_didoscalim_verbosity`

change le niveau de verbosité dans le bloc de code

## `didoscalim_debug()`

affiche les messages de debug

## `didoscalim_info()`

affiche les messages d'info

## `didoscalim_abort`

affiche le message au niveau le plus elevé et lève une exception.
