#' A wrapper for matrixStats::rowMaxs()
#' Returns the maximal numeric value per row in a matrix or a dataframe
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
#' A wrapper for matrixStats::rowMins()
#' Returns the minimal numeric value per row in a matrix or a dataframe
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
#' Clips numeric values so that they lie in the range [min_val, max_val]
#' Works on both numeric arrays and dataframes
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
    if (length(cols) > 0) {
        x[cols] <- clip_vals.numeric(x[cols], min_val, max_val)
    }
    return(x)
}


########################################################################
#' Segment a datafrane according to consecutive identical values of a
#' column expression.
#' @export
segment_by <- function(df, column, var='segment')
{
    rle_rep <- function(vals)
    {
        enc <- rle(vals)
        return(rep(1:length(enc$lengths), enc$lengths))
    }

    column <- rlang::enquo(column)
    
    temp_df <- tibble(group=dplyr::group_indices(df), 
                      col=df %>% dplyr::mutate(.temp_col_=!!column) %>% dplyr::pull(.temp_col_))
    temp_df <- temp_df %>%
               dplyr::mutate(segment=dplyr::if_else(!is.na(col), rle_rep(col), 0L)) %>%
               dplyr::group_by(group, segment)

    df[,var] <- dplyr::dense_rank( dplyr::if_else(!is.na(temp_df$col), dplyr::group_indices(temp_df), 0L) ) - 1

    return(df)
}
