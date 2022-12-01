expect_datetime <- function(object, n, delta = 0) {
  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  act$n <- lubridate::ymd_hms(act$val, tz = Sys.timezone(), quiet = TRUE)
  dt_difference <- abs(time_length(act$n - lubridate::ymd_hms(n, tz = Sys.timezone(), quiet = TRUE), unit = "seconds"))
  testthat::expect(
    (dt_difference <= delta),
    sprintf('%s (%s) is not equal to %s (delta %is greater than %is).\n\n`actual`:   %s\n`expected`: "%s"',
            act$lab, act$val, n, dt_difference, delta, act$lab, n)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}
