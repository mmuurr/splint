na_lgl <- murlib.core::na_lgl
na_int <- murlib.core::na_int
na_dbl <- murlib.core::na_dbl
na_chr <- murlib.core::na_chr


`%||%` <- murlib.infix::`%||%`
`%|%`  <- murlib.infix::`%|%`
`%0%`  <- murlib.infix::`%0%`
`%@%`  <- murlib.infix::`%@%`


prepend_class <- murlib.core::prepend_class
identity      <- murlib.core::itself


plck <- purrr::pluck


venn <- function(l, r) {
  list(
    both = intersect(l, r),
    l_only = setdiff(l, r),
    r_only = setdiff(r, l)
  )
}


#' @export
dict <- murlib.core::dict

check_dict <- function(x) {
  check_res <- checkmate::check_list(x, names = "unique")
  if (isTRUE(check_res)) return(TRUE)
  sprintf("not a dict: %s", check_res)
}
test_dict <- checkmate::makeTestFunction(check_dict)
assert_dict <- checkmate::makeAssertionFunction(check_dict)
