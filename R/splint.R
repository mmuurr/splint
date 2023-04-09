#' @include utils.R

## All splints are functions that must have at least this signature: f(x, ...).
## `x` is what's being splinted.
## `...` allows splints to recurse (if needed).
##
## All splint functions:
## * Inherit from the "splint.splint" (S3) class.
## * Possess a `missing` attribute which controls behavior when the item is missing.
##
## The {splint} package verifies that a splint is a function, but doesn't _validate_ that it works; this is left to the programmer in the context of {splint}'s use.

#' @export
splint_simple <- function(f = identity, missing = NULL) {
  checkmate::assert_function(f)
  force(f)
  f_splint <- function(x, ...) f(x)
  structure(
    f_splint,
    missing = missing,
    class = c("splint.splint_simple", "splint.splint")
  )
}


#' @param splints A list of splints, each of which must one inherit from any of "splint.splint_simple", "splint.splint_dict", "splint.splint_tbl".
#' @export
splint_dict <- function(splints = list()) {
  checkmate::assert_list(splints, names = "unique")
  purrr::walk(splints, function(splint) {
    checkmate::assert_multi_class(
      splint,
      c("splint.splint_simple", "splint.splint_dict", "splint.splint_tbl")
    )
  })

  f_splint <- function(x, ..., keep_all = FALSE) {
    x <- as.list(x)
    checkmate::assert_list(x, names = "unique")

    ## present, missing, extra names w.r.t. x
    present_names <- intersect(names(x), names(splints))
    missing_names <- setdiff(names(splints), names(x))
    extra_names <- setdiff(names(x), names(splints))

    new_x <- 
      purrr::imap(splints, function(splint, key) {
        plck(x, key, .default = splint %@% missing) |> splint(keep_all = keep_all, ...)
      })

    if (isTRUE(keep_all)) new_x <- c(new_x, x[extra_names])

    new_x
  }

  structure(
    f_splint,
    missing = NULL,
    class = c("splint.splint_dict", "splint.splint")
  )
}


#' @export
splint_tbl <- function(splints = list()) {
  checkmate::assert_list(splints, names = "unique")
  purrr::walk(splints, function(splint) {
    checkmate::assert_multi_class(
      splint,
      c("splint.splint_simple", "splint.splint_dictcol")
    )
  })

  ptype_tbl <-
    purrr::map(splints, function(splint) {
      splint(splint %@% missing) |> vctrs::vec_slice(0)
    }) |>
    tibble::as_tibble()

  f_splint <- function(x, ..., keep_all = FALSE) {
    x <- tibble::as_tibble(x)
    checkmate::assert_data_frame(x, col.names = "unique")

    ## present, missing, extra names w.r.t. x
    present_names <- intersect(names(x), names(splints))
    missing_names <- setdiff(names(splints), names(x))
    extra_names <- setdiff(names(x), names(splints))

    ## any cols to remove?:
    if (!isTRUE(keep_all)) x <- x[present_names]

    ## splint the present names:
    for (colname in present_names) {
      x[[colname]] <- splints[[colname]](x[[colname]], keep_all = keep_all, ...)
    }

    ## row-bind with ptype (ptype comes first to enfore col-ordering):
    x <- dplyr::bind_rows(ptype_tbl, x)

    ## now splint the missing cols that were just added.
    ## this step is mostly only beneficial for dictcols, which lost type details when zero-lengthed (becoming an ordinary empty list).
    for (colname in missing_names) {
      x[[colname]] <- splints[[colname]](x[[colname]], keep_all = keep_all, ...)
    }

    x
  }

  structure(
    f_splint,
    missing = NULL,
    ptype_tbl = ptype_tbl,
    class = c("splint.splint_tbl", "splint.splint")
  )
}


#' @export
splint_dictcol <- function(splint = splint_dict()) {
  checkmate::assert_class(splint, "splint.splint_dict")
  force(splint)
  
  f_splint <- function(x, ..., keep_all = TRUE) {
    x <- as.list(x)
    purrr::map(x, \(x) splint(x, keep_all = keep_all, ...))
  }

  structure(
    f_splint,
    missing = NULL,
    class = c("splint.splint_dictcol", "splint.splint")
  )
}


#' @export
splint_tbl_ptype <- function(splint) {
  checkmate::assert_class(splint, "splint.splint_tbl")
  splint %@% ptype_tbl
}
