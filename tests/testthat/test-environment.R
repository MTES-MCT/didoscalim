# FIXME: dirty hack for environment

clean_env <- function() {
  env <- c()
  env_names <- names(Sys.getenv())
  for (name in env_names[grep("DIDOSCALIM_.*", env_names)]) {
    env[[name]] <- NA
  }
  return(env)
}

test_that("load_envs works when env is empty", {
  withr::local_environment(.didoscalim_env)
  withr::local_envvar(clean_env())
  load_envs()
  expect_output(list_envs(), "Vous avez 0 environnement\\(s) configuré\\(s).*")
})

test_that("set_work_env errors if env is empty", {
  withr::local_environment(.didoscalim_env)
  withr::local_envvar(clean_env())
  load_envs()

  err <- expect_error(set_work_env(), class = "env_error")
  expect_match(err$message, "Aucun environnement n'est configuré dans votre Renviron")
})

test_that("load_envs works", {
  withr::local_environment(.didoscalim_env)
  withr::local_envvar(
    c(clean_env(),
      DIDOSCALIM_BASE_PATH_DEV = "truc",
      DIDOSCALIM_API_KEY_DEV = "truc"
    )
  )
  load_envs()
  expect_output(list_envs(), "Vous avez 1 environnement\\(s) configuré\\(s).*")
})

test_that("set_work_env errors if env doesn't exists", {
  withr::local_environment(.didoscalim_env)
  withr::local_envvar(
    c(clean_env(),
      DIDOSCALIM_BASE_PATH_DEV = "truc",
      DIDOSCALIM_API_KEY_DEV = "truc"
    )
  )
  load_envs()
  err <- expect_error(set_work_env("PROD"), class = "env_error")
  expect_match(err$message, " n'est pas un environnement reconnu")
})


test_that("load_envs works", {
  withr::local_environment(.didoscalim_env)
  withr::local_envvar(
    c(clean_env(),
      DIDOSCALIM_BASE_PATH_DEV = "dev",
      DIDOSCALIM_API_KEY_DEV = "dev",
      DIDOSCALIM_BASE_PATH_ECOLE = "ecole",
      DIDOSCALIM_API_KEY_ECOLE = "ecole"
    )
  )

  expect_equal(get_work_env(), "DEV")
})

test_that("load_envs works", {
  withr::local_environment(.didoscalim_env)
  withr::local_envvar(
    c(clean_env(),
      DIDOSCALIM_BASE_PATH_DEV = "dev2",
      DIDOSCALIM_API_KEY_DEV = "dev2",
      DIDOSCALIM_BASE_PATH_ECOLE = "ecole2",
      DIDOSCALIM_API_KEY_ECOLE = "ecole2",
      DIDOSCALIM_BASE_PATH_WITH_UNDERSCORE = "other2",
      DIDOSCALIM_API_KEY_WITH_UNDERSCORE = "other2"
    )
  )
  load_envs()

  expect_equal(set_work_env("ECOLE"), "DEV")
  expect_equal(get_work_env(), "ECOLE")

  expect_equal(set_work_env("WITH_UNDERSCORE"), "ECOLE")
  expect_equal(get_work_env(), "WITH_UNDERSCORE")
})

test_that("load_envs fails on incomplete configuration", {
  withr::local_environment(.didoscalim_env)
  withr::local_envvar(
    c(clean_env(),
      DIDOSCALIM_BASE_PATH_DEV = "dev2"
    )
  )
  expect_error(load_envs(), "est incomplète")

  withr::local_envvar(
    c(clean_env(),
      DIDOSCALIM_API_KEY_DEV = "dev2"
    )
  )
  expect_error(load_envs(), "est incomplète")
})


test_that("load_envs errors when environment doesn't exist", {
  withr::with_environment(
    .didoscalim_env,
    code = {
      withr::with_envvar(
        c(clean_env(),
          DIDOSCALIM_BASE_PATH_DEV = "dev3",
          DIDOSCALIM_API_KEY_DEV = "dev3",
          DIDOSCALIM_BASE_PATH_ECOLE = "ecole3",
          DIDOSCALIM_API_KEY_ECOLE = "ecole3"
        ),
        code = {
          load_envs()
          # L'environnement PROD n'existe pas."
          expect_error(set_work_env("PROD"), class = "env_error")
        }
      )
    }
  )
  load_envs()
})
