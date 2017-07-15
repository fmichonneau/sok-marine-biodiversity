n_cores <- function() {
    cmptr_name <- system("uname -n", intern = TRUE)
    if (identical(cmptr_name, "francois-laptop") ||
        identical(cmptr_name, "francois-XPS-15-9560")) {
        options("mc.cores" = 8)
    } else if (identical(cmptr_name, "ryanlab.whitney.ufl.edu")) {
        options("mc.cores" = 12)
    } else {
        options("mc.cores" = 1)
        message("no parallelization")
    }
    message("number of cores ", getOption("mc.cores"))
}

init <- function() {
    extrafont::loadfonts(quiet = TRUE)
    n_cores()
    if (!is.null(test_cleanup(TRUE)))
        stop("test_cleanup(TRUE) is broken")
    if (!is.null(test_cleanup(FALSE)))
        stop("test cleanup(FALSE) is broken")
    if (!is.null(test_id_level()))
        stop("test_id_level() is broken")
    ## code is evaluated in ./R/ folder
    if (!dir.exists("../data-validation"))
        dir.create("../data-validation")
}


init()
