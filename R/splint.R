#' @include utils.R


venn <- function(l, r) {
  list(
    both = intersect(l, r),
    l_only = setdiff(l, r),
    r_only = setdiff(r, l)
  )
}


#' @export
splint_simple <- function(f = identity, missing = NULL) {
  checkmate::assert_function(f)
  force(f)
  new_splint <- function(x, ...) f(x)
  structure(
    new_splint,
    missing = missing,
    class = c("splint.simple_splint", "splint.splint"),
    f = f
  )
}


#' @export
splint_dict <- function(splints = list(), keep_all = FALSE) {
  checkmate::assert_list(splints, names = "unique")
  purrr::walk(splints, \(splint) checkmate::assert_class(splint, "splint.splint"))

  force(splints)
  force(keep_all)
  
  new_splint <- function(x = list(), ...) {
    x <- as.list(x)
    checkmate::assert_list(x, names = "unique")

    venn <- venn(names(splints), names(x))

    new_list <- purrr::imap(splints, function(splint, name) {
      plck(x, name, .default = splint %@% missing) |> splint()
    })

    if (isTRUE(keep_all)) {
      new_list <- append(new_list, x[venn$r_only])
    }

    new_list
  }

  structure(
    new_splint,
    missing = NULL,
    splints = splints,
    class = c("splint.dict_splint", "splint.splint")
  )
}


#' @export
splint_tbl <- function(splints = list(), keep_all = FALSE) {
  checkmate::assert_list(splints, names = "unique")
  purrr::walk(splints, \(splint) checkmate::assert_multi_class(splint, c("splint.simple_splint", "splint.tbl_splint", "splint.map_splint")))

  force(splints)
  force(keep_all)

  ptype_tbl <-
    purrr::map(splints, \(splint) {
      splint(splint %@% missing) |> vctrs::vec_slice(0)
    }) |>
    tibble::as_tibble()  ## bind_cols() seems more natural, but: https://github.com/tidyverse/dplyr/issues/6814
  
  new_splint <- function(x = data.frame(), ...) {
    x <- tibble::as_tibble(x)
    checkmate::assert_tibble(x, col.names = "unique")

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
    x <- dplyr::bind_rows(ptype_tbl, x)

    ## finally, splint any of the new cols that were just added.
    ## this step allows for recursing into, e.g., list-cols, which are missing type details in the ptype, since they're zero-length 'base' lists.
    for (colname in venn$l_only) {
      x[[colname]] <- splints[[colname]](x[[colname]])
    }

    x
  }

  structure(
    new_splint,
    missing = NULL,
    splints = splints,
    ptype_tbl = ptype_tbl,
    class = c("splint.tbl_splint", "splint.splint")
  )
}


#' @export
splint_map <- function(splint = splint_simple()) {
  checkmate::assert_class(splint, "splint.splint")
  force(splint)

  new_splint <- function(x, ...) {
    purrr::map(x, splint)
  }

  structure(
    new_splint,
    missing = NULL,
    splint = splint,
    class = c("splint.map_splint", "splint.splint")
  )
}


#' @export
splint_get_splints <- function(splint) {
  if (inherits(splint, "splint.tbl_splint")) {
    splint %@% splints
  } else if (inherits(splint, "splint.dict_splint")) {
    splint %@% splints
  } else if (inherits(splint, "splint.map_splint")) {
    list(splint %@% splint)
  } else {
    NULL
  }
}


#' @export
splint_get_ptype_tbl <- function(tbl_splint) {
  tbl_splint %@% ptype_tbl
}


