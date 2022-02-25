
rescue_store <- function(store) {
  all_content <- store$list()
  is_missing <- vapply(
    all_content,
    function(x) {
      res <- try(store$get(x), silent = TRUE)
      inherits(res, "try-error")
    }, logical(1)
  )
  message(sum(is_missing), " records need to be rescued...")

  to_redo <- all_content[is_missing]
  sapply(to_redo, function(x) store$del(x))
  sapply(to_redo, function(x) store$get(x))
}
