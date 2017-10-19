#' tgutil.
#'
#' @name tgutil
#' @docType package
#' @useDynLib tgutil
#' @import tibble
#' @importFrom Rcpp sourceCpp

########################################################################
.onUnload <- function (libpath) {
  library.dynam.unload("tgutil", libpath)
}
