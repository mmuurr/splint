#' @include utils.R


## Takes a function and formalizes it as a splint.
splintify <- function(f, klass = character(0), force = TRUE) {
  if (isTRUE(force)) checkmate::assert_function(f)  ## base::force(f)

  splint <- function(x, ...) {
    if (missing(x)) x <- NULL
    retval <-
      if (checkmate::test_class(x, "splint.missing_val")) {
        if (isTRUE(x$exactly)) x$val else f(x$val)
      } else {
        f(x)
      }
    retval |> prepend_class(klass)
  }
  splint |> prepend_class("splint.splint")
}


splint_missing_val <- function(val, exactly = FALSE) {
  structure(
    list(val = val, exactly = isTRUE(exactly)),
    class = "splint.missing_val"
  )
}


#' Explicit missingness instruction
#' 
#' This wraps a splint in higher-order splint structures (lists and tbls) and specifies what to do if the element defined by `splint` itself is missing.
#' If the element is missing, what value do we pass to the constituent splint?
#' Here we declare that value, `val`.
#' * If `exactly` is `FALSE`, then `val` is passed to the `splint`.
#' * If `exactly` is `TRUE`, then the `splint` is bypassed entirely and `val` is simply used in its stead.
#'
#' @param splint the splint to wrap.
#' @param val the value to use when the splint element is missing.
#' @param exactly should `val` be used _exactly_ (as-is), or should it be passed to `splint()`?
#' 
#' @export
splint_if_missing <- function(splint, val, exactly = FALSE) {
  checkmate::assert_class(splint, "splint.splint")
  structure(
    splint,
    missing_val = splint_missing_val(val, exactly)
  )
}


#' Simple splint
#'
#' Wraps a function as a simple, atomic splint.
#' While the resultant splint itself is 'atomic', `f()` can accept whatever type argument you like.
#' 
#' When called (as a function), a simple splint will look at the single passed argument `x`.
#' * If `x` is missing, set `x` to `NULL`.
#' * If `x` is a missing-val sentinel (with `val` and `exactly` properties):
#'   * If `exactly` is `TRUE`, return `val`.
#'   * Else return `f(val)`.
#' * Else return `f(x)`.
#'
#' @param f the underlying splint function.
#' @param klass the classname(s) to be prepended to the splint's output.
#' 
#' @export
splint_simple <- function(f = identity, klass = character(0), force = TRUE) {
  splintify(f, klass, force) |> prepend_class("splint.simple_splint")
}  


#' @export
splint_dict <- function(splints = list(), keep_all = FALSE, klass = character(0)) {
  ## splints should itself be a dict, each prop of which should be a splint:
  assert_dict(splints)  ## force(splints)
  purrr::walk(splints, \(splint) checkmate::assert_class(splint, "splint.splint"))

  checkmate::assert_flag(keep_all)  ## force(keep_all)
  checkmate::assert_character(klass)  ## force(klass)

  f <- function(x = list()) {
    ## x should be a dict:
    x <- as.list(x)
    assert_dict(x)
    
    venn <- venn(names(splints), names(x))

    new_dict <- purrr::imap(splints, \(splint, name) {
      plck(x, name, .default = splint %@% missing_val) |> splint()
    })

    if (isTRUE(keep_all)) {
      new_dict <- c(new_dict, x[venn$r_only])
    }

    new_dict
  }

  splintify(f, klass) |> prepend_class("splint.dict_splint")
}


#' @export
splint_tbl <- function(splints = list(), keep_all = FALSE, klass = character(0)) {
  ## splints should itself be a dict, each prop of which should be either a
  ## * simple_splint,
  ## * tbl_splint, or
  ## * map_splint.
  assert_dict(splints)  ## force(splints)
  purrr::walk(splints, \(splint) checkmate::assert_multi_class(splint, c("splint.simple_splint", "splint.tbl_splint", "splint.map_splint")))

  checkmate::assert_flag(keep_all)  ## force(keep_all)
  checkmate::assert_character(klass)  ## force(klass)

  ptype_tbl <-
    purrr::map(splints, \(splint) {
      splint(splint %@% missing_val) |> vctrs::vec_slice(0)
    }) |>
    tibble::as_tibble()  ## bind_cols() seems more natural, but: https://github.com/tidyverse/dplyr/issues/6814

  f <- function(x = data.frame()) {
    ## x should be a tbl:
    x <- tibble::as_tibble(x)
    checkmate::assert_tibble(x, col.names = "unique")

    ## short-circuit for speed:
    if (ncol(x) == 0) return(ptype_tbl)
    
    venn <- venn(names(splints), names(x))

    ## here we keep `x` as a DF throughout each step.

    ## remove extra cols if needed (by keeping only those cols present in venn$both), else leave all cols:
    if (!isTRUE(keep_all)) x <- x[venn$both]

    ## splint the present columns that have defined splints:
    for (colname in venn$both) {
      x[[colname]] <- splints[[colname]](x[[colname]])
    }

    ## row-bind with ptype, adding any missing cols.
    ## ptype comes first to also re-order cols according to splint def.
    ## if we're keep_all is TRUE and some DF cols are in the splint def, they'll be moved to the right side of the tbl.
    x <- dplyr::bind_rows(ptype_tbl, x)

    ## finally, splint any of the new cols that were just added.
    ## this step allows for recursing into, e.g., list-cols, which are missing type details in the ptype, since they're zero-length 'base' lists.
    for (colname in venn$l_only) {
      x[[colname]] <- splints[[colname]](x[[colname]])
    }

    x
  }

  splintify(f, klass) |> prepend_class("splint.tbl_splint")
}


#' @description
#' Applies the specified `splint` to every element in an arbitrary-length list.
#' Most useful as a wrapper around `splint_dict`s for list-cols in a tbl, i.e. for a (list-)col of dicts.
#' @export
splint_map <- function(splint = splint_simple()) {
  checkmate::assert_class(splint, "splint.splint")  ## force(splint)

  f <- function(x) {
    purrr::map(x, splint)
  }

  splintify(f) |> prepend_class("splint.map_splint")
}


#' @description
#' For container splints, return the contained splints.
#' Only applies to `tbl_splint`s, `dict_splint`s, and `map_splint`s.
#' Note that for `map_splint`s, this will always return a list of length one containing the wrapped splint (as opposed to the wrapped splint itself), just for uniformity's sake.
#' @export
splint_get_splints <- function(splint) {
  if (checkmate::test_multi_class(splint, c("splint.dict_splint", "splint.tbl_splint"))) {
    environment(environment(splint)$f)$splints
  } else if (checkmate::test_multi_class(splint, c("splint.map_splint"))) {
    list(environment(environment(splint)$f)$splint)
  } else {
    NULL
  }
}


#' @export
splint_tbl_extend <- function(tbl_splint, splints, keep_all = NULL) {
  checkmate::assert_class(tbl_splint, "splint.tbl_splint")
  splints <- c(splint_get_splints(tbl_splint), splints)
  keep_all <- if (is.null(keep_all)) rlang::get_env(rlang::get_env(tbl_splint)$f)$keep_all else keep_all
  splint_tbl(splints, keep_all)
}

