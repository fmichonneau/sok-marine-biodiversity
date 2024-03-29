cleanup_species_names <- function(nm, rm_subgenus = FALSE) {
  ## remove /
  nm <- gsub("/", " ", nm)
  ## remove author/year
  nm <- vapply(nm, function(x) {
    if (grepl("[0-9]{4}", x)) {
      gsub("([a-z]+)\\s([a-z]+)?\\s?.+", "\\1 \\2", x)
    } else {
      x
    }
  }, character(1), USE.NAMES = FALSE)
  ## remove higher taxa
  nm <- gsub("^.+:", "", nm)
  ## remove words that contain numbers
  nm <- gsub("\\S*\\d\\S*", "", nm)
  ## remove extra spaces
  nm <- gsub("\\s{2, }", " ", nm)
  ## remove sp(p).
  nm <- gsub("\\bspp?\\b(\\.|\\,)?\\s?(\\bnov\\b\\.?)?.*$", "", nm)
  ## remove words fully capitalized
  nm <- gsub("\\b[A-Z]+\\b", "", nm)
  ## remove cf., aff. and ?
  nm <- gsub("\\s?(cf\\.|f\\.|aff\\.|\\?|(ex\\.? gr\\.))\\s?", " ", nm)
  ## remove varieties
  nm <- gsub("var. .+$", "", nm)
  ## remove square brackets
  nm <- gsub("\\[(.+)\\]", "\\1", nm)
  ## remove leading and trailing spaces
  nm <- gsub("\\s+$", "", nm)
  nm <- gsub("^\\s+", "", nm)
  ## remove subgenus
  if (rm_subgenus) {
    ## get the form: Genus (subgenus) species
    nm <- gsub("\\s?\\([^)]*\\)\\s?", " ", nm)
    ## and: Genus Subgenus species
    nm <- gsub("([a-zA-Z]*)\\s(\\b[A-Z][a-z]*\\b)", "\\1", nm)
  }
  nm
}


remove_subspecies <- function(nm) {
  gsub("([A-Z][a-z]+)\\s([a-z]+)\\s([a-z]+)", "\\1 \\2", nm)
}


## This gets run everytime by init()
test_cleanup <- function(rm_subgenus) {
  df <- tibble::tribble(
    ~input, ~rm_subgenus, ~no_rm_subgenus,
    "enchytraeus sp. nov.", "enchytraeus", "enchytraeus",
    "echytraeus sp.", "echytraeus", "echytraeus",
    "echytraeus sp, ", "echytraeus", "echytraeus",
    "echytraeus sp ", "echytraeus", "echytraeus",
    "echytraeus sp", "echytraeus", "echytraeus",
    "echytraeus sp. nov. a", "echytraeus", "echytraeus",
    "nereis pelagica linnaeus, 1758", "nereis pelagica", "nereis pelagica",
    "nereis (nereis) zonata", "nereis zonata", "nereis (nereis) zonata",
    "Holothuriidea: holothuria impatiens", "holothuria impatiens", "holothuria impatiens",
    "clepsine ornata var. d.", "clepsine ornata", "clepsine ornata",
    "mereditha spinifera", "mereditha spinifera", "mereditha spinifera",
    "Genus Subgenus species", "Genus species", "Genus Subgenus species",
    "[test]", "test", "test",
    "Abra longicallus americana", "Abra longicallus americana", "Abra longicallus americana"
  )
  if (rm_subgenus) {
    var <- "rm_subgenus"
  } else {
    var <- "no_rm_subgenus"
  }
  df$output <- cleanup_species_names(df$input, rm_subgenus)
  tc <- df$output == df[[var]]
  if (!all(tc)) {
    df[!tc, c("input", var, "output")]
  }
}

is_binomial <- function(nm) {
  nm <- cleanup_species_names(nm)
  vapply(
    strsplit(nm, " "), function(x) {
      length(x) == 2 &&
        vapply(
          x[[1]],
          function(y) all(nchar(gsub("[a-zA-Z]", "", x)) == 0),
          logical(1)
        )
    },
    logical(1)
  )
}

capitalize <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
      {
        s <- substring(s, 2)
        if (strict) tolower(s) else s
      },
      sep = "",
      collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


####
### By Scott Chamberlain http://r.789695.n4.nabble.com/Geographic-distance-between-lat-long-points-in-R-td3442338.html
## Convert degrees to radians
deg2rad <- function(deg) {
  return(deg * pi / 180)
}

## Calculates the geodesic distance between two points specified by
## radian latitude/longitude using the Haversine formula
gcd.hf <- function(long1, lat1, long2, lat2, warn = TRUE) {
  if (warn) {
    warning("Did you convert the latitudes in radian first?!")
  }
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta.long / 2)^2
  c <- 2 * asin(min(1, sqrt(a)))
  d <- R * c
  return(d) # Distance in km
}

format_output <- function(x, ...) {
  sprintf("%.0f", x)
}

render <- function(file, ...) {
  rmarkdown::render(file, ...)
}

## US coordinates
## pretty generous, but easier
us_coords <- function() {
  list(
    lon = c(-128, -60),
    lat = c(22, 51)
  )
}

split_by_n <- function(x, n, ...) {
  split(x, ceiling(seq_along(x) / n), ...)
}

convert_to_feather <- function(file, feather_out) {
  res <- readr::read_csv(file)
  feather::write_feather(res, path = feather_out)
}

is_lower_case <- function(x) {
  if (!identical(tolower(x), x)) {
    stop(x, " is not lowercase.")
  }
}


filter_raw_records <- function(db) {
  if (inherits(db, "character")) {
    db <- feather::read_feather(db)
    names(db) <- tolower(names(db))
  }

  if (any(duplicated(db$uuid))) {
    stop("duplicated UUID values!")
  }

  db %>%
    filter(!is.na(decimallatitude) | !is.na(decimallongitude))
}

first_5 <- function(x) {
  n <- ifelse(length(x) > 5, 5, length(x))
  paste(x[seq_len(n)], collapse = ", ")
}


combine_plots <- function(..., output) {
  files <- c(...)
  check_prog_exists("pdfjam", "error", "pdfjam is needed to generate some figures.")
  if (length(files) > 2) stop("can't deal with this now.")
  if (all(file.exists(files))) {
    cmd <- paste(
      "pdfjam ", paste(files, collapse = "  "),
      "--nup 2x1 --papersize '{8.5in,4.5in}' --outfile",
      output
    )
    system(cmd)
  }
}


annotate_pdf <- function(infile, text, geometry = c("topleft", "topright"),
                         offset = 10, font = "Helvetica", fonsize = 13, output) {
  check_prog_exists("cpdf", "warning", "Skipping annotations.")
  ## annotate
  cmd <- paste0(
    "cpdf -add-text \"", text, "\"",
    " -", geometry, " ", offset,
    " -font \"", font, "\" ",
    infile,
    " -o ", output
  )
  system(cmd)
}


check_prog_exists <- function(prog, outcome = c("warning", "error"), ...) {
  outcome <- match.arg(outcome)
  ## find if cpdf is installed
  wc_chk <- system(paste("which", prog),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )

  outcome <- switch(outcome,
    warning = warning,
    error = stop
  )

  if (identical(wc_chk, 1L)) {
    outcome(paste(prog, "not found.", paste(..., collapse = " ")))
    return(NULL)
  }
}


combine_upsetr_plots <- function(gom_plot, pnw_plot, output) {
  gom_annot <- file.path(tempdir(), paste0("annot-", basename(gom_plot)))
  pnw_annot <- file.path(tempdir(), paste0("annot-", basename(pnw_plot)))

  annotate_pdf(gom_plot,
    text = "A. Gulf of Mexico",
    geometry = "topright", output = gom_annot
  )
  annotate_pdf(pnw_plot,
    text = "B. Pacific Northwest",
    geometry = "topright", output = pnw_annot
  )

  if (!(file.exists(gom_annot) && file.exists(pnw_annot))) {
    stop("something went wrong with pdf annotations")
  }

  combine_plots(gom_annot, pnw_annot, output = output)
}


##' convert column from data frame to character
int_sok_as_character <- function(.d, colname) {
  stopifnot(rlang::is_scalar_character(colname))
  if (exists(colname, .d)) {
    .d[[colname]] <- as.character(.d[[colname]])
  }
  .d
}

sok_as_character <- function(.d, colnames) {
  stopifnot(rlang::is_character(colnames) && length(colnames) > 0)
  for (i in seq_along(colnames)) {
    .d <- int_sok_as_character(.d, colnames[i])
  }
  .d
}
