.onLoad <- function(libname, pkgname) { # nolint # nocov start
  load_envs()
  tryCatch(
      set_work_env(),
    error = function(error) {
      local_didoscalim_verbosity("info")
      didoscalim_info(c(
        "x" = error$message
      ))
      return(FALSE)
    }
  )
} # nocov end
