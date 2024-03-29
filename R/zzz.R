n_cores <- function(auto = FALSE) {
  if (auto) {
    message("Using auto detection of the number of cores")
    options("mc.cores" = parallel::detectCores())
  } else {
    cmptr_name <- system("uname -n", intern = TRUE)
    if (identical(cmptr_name, "francois-laptop") ||
          identical(cmptr_name, "francois-XPS-15-9560")) {
      options("mc.cores" = 8L)
    } else if (identical(cmptr_name, "ryanlab.whitney.ufl.edu")) {
      options("mc.cores" = 12L)
    } else if (grepl("compute", cmptr_name)) {
      options("mc.cores" = 8L)
    } else {
      options("mc.cores" = 1L)
      message("no parallelization")
    }
  }
  message("number of cores ", getOption("mc.cores"))
  getOption("mc.cores")
}

init <- function() {
  extrafont::loadfonts(quiet = TRUE)
  n_cores()
  if (!is.null(test_cleanup(TRUE))) {
    stop("test_cleanup(TRUE) is broken")
  }
  if (!is.null(test_cleanup(FALSE))) {
    stop("test cleanup(FALSE) is broken")
  }
  ## code is evaluated in ./R/ folder
  if (!dir.exists("../data-validation")) {
    dir.create("../data-validation")
  }
  ## for some reasons these 2 keys in the OBIS fail consistently
  ## so we are removing them everytime to force fetching them
  store_obis_occurrences("../data/storr_obis_occurrences")$del(c("391707", "220659"))
}

## control output level based on verbosity level

options(verbose_level = 3L)
verbose <- function(..., level) {
  stopifnot(
    identical(length(level), 1L),
    is.integer(level)
  )

  verbose_level <- getOption("verbose_level")

  if (is.null(verbose_level)) {
    verbose_level <- 1L
  }

  if (level <= verbose_level) {
    message(...)
  }
}

v1 <- function(...) verbose(..., level = 1L)
v2 <- function(...) verbose(..., level = 2L)
v3 <- function(...) verbose(..., level = 3L)

init()
