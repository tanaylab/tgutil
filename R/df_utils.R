########################################################################
#' @export
rowMaxs <- function (x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...)
{
    UseMethod('rowMaxs')
}


########################################################################
#' @export
rowMaxs.numeric <- function(x, ...)
{
    return(matrixStats::rowMaxs(x, ...))
}


########################################################################
#' @export
rowMaxs.data.frame <- function(x, ...)
{
    x <- dplyr::select_if(x, is.numeric)
    return(matrixStats::rowMaxs(as.matrix(x), ...))
}


########################################################################
#' @export
rowMins <- function (x, rows = NULL, cols = NULL, na.rm = FALSE, dim. = dim(x), ...)
{
    UseMethod('rowMins')
}


########################################################################
#' @export
rowMins.numeric <- function(x, ...)
{
    return(matrixStats::rowMins(x, ...))
}


########################################################################
#' @export
rowMins.data.frame <- function(x, ...)
{
    x <- dplyr::select_if(x, is.numeric)
    return(matrixStats::rowMins(as.matrix(x), ...))
}


########################################################################
#' @export
clip_vals <- function(x, min_val=NULL, max_val=NULL)
{
    UseMethod('clip_vals')
}


########################################################################
#' @export
clip_vals.numeric <- function(x, min_val=NULL, max_val=NULL)
{
    # Avoid pmin and pmax as they fail once the number of elements exceeds
    # 2**31 - 1
    if (!is.null(min_val)) {
        x[x<min_val] <- min_val
    }
    if (!is.null(max_val)) {
        x[x>max_val] <- max_val
    }
    return(x)
}


########################################################################
#' @export
clip_vals.data.frame <- function(x, min_val=NULL, max_val=NULL)
{
    cols <- sapply(as.list(x), is.numeric)
    x[cols] <- clip_vals.numeric(x[cols], min_val, max_val)
    return(x)
}
