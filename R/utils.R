`%||%` <- rlang::`%||%`
`%@%`  <- rlang::`%@%`
`%@%<-`  <- rlang::`%@%<-`

na_lgl <- NA
na_int <- NA_integer_
na_dbl <- NA_real_
na_chr <- NA_character_
na_date <- as.Date(na_lgl)
na_datetime <- as.POSIXct(na_lgl)

plck <- purrr::pluck


#' @title Prepend classname(s) to an object's S3 class attribute.
#'
#' @details
#' When `obj` is an object with reference semantics (refsem) (e.g. an {R6} object), this function will not only return the new object, but will modify `obj` _in-place_.
#' This is a by-product of how `structure()` works and is tough to avoid with refsem objects.
#' The stepwise alternative (setting `class(obj) <- something` inside the function does the same: mutates the original object).
#' A future version of this function may warn when called on a refsem object (and that warning will be suppressable).
#' 
#' @param obj A simple (or S3) object.
#' @param cls A character vector of classnames (in appropriate S3 inheritance order)
#'
#' @return `obj` with the `cls` prepended to the classnames.
prepend_class <- function(obj, cls) {
  if (length(cls) == 0) {
    obj
  } else {
    structure(obj, class = c(unique(cls), class(obj)))
  }
}


#' @title Returns the (first) argument (safely).
#' 
#' @details
#' Unlike `base::identity`, if `x` is missing here, `NULL` is returned.
#'
#' @param x The argument to return (unmodified).
#' @param ... Ignored.
#'
#' @return `x`; if `x` is missing, `NULL`.
identity <- function(x, ...) {
  if (missing(x)) NULL else x
}


