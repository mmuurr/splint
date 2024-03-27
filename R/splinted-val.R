#' @export
splinted_val <- function(splint = splint_simple(identity), val = NULL) {
  val <- splint(val)
  env <- rlang::current_env()
  get <- function() val
  set <- function(val) {
    assign("val", splint(val), envir = env)
    get()
  }
  structure(
    list(get = get, set = set, as = splint),
    class = c("splint.splinted_val")
  )
}
