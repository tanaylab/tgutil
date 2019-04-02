########################################################################
#' A wrapper for matrixStats::colMaxs()
#' Returns the minimal numeric value per column in a matrix or a dataframe
#' @inheritDotParams matrixStats::rowMaxs
#' @export
colMaxs <- function(x, ...)
{
    UseMethod('colMaxs')
}


########################################################################
#' @export
colMaxs.numeric <- function(x, ...)
{
    return(matrixStats::colMaxs(x, ...))
}


########################################################################
#' @export
colMaxs.data.frame <- function(x, rows=NULL, cols=NULL, ...)
{
    if (!is.null(cols)) {
        x <- x[,cols]
    }
    mask <- unlist(lapply(x, is.numeric))
    rc <- rep(NA_real_, ncol(x))
    if (any(mask)) {
        rc[mask] <- matrixStats::colMaxs(as.matrix(x[,mask]), rows=rows, cols=NULL, ...)
    }
    return(rc)
}


########################################################################
#' @export
colMaxs.dgCMatrix <- function(x, rows=NULL, cols=NULL)
{
    x <- .prune_matrix(x, rows, cols)
    val <- qlcMatrix::colMax(x, ignore.zero=FALSE)
    return(as.vector(val))
}
setMethod('colMaxs', 'dgCMatrix', colMaxs.dgCMatrix)


########################################################################
#' A wrapper for matrixStats::colMins()
#' Returns the minimal numeric value per column in a matrix or a dataframe
#' @inheritDotParams matrixStats::rowMaxs
#' @export
colMins <- function(x, ...)
{
    UseMethod('colMins')
}


########################################################################
#' @export
colMins.numeric <- function(x, ...)
{
    return(matrixStats::colMins(x, ...))
}


########################################################################
#' @export
colMins.data.frame <- function(x, rows=NULL, cols=NULL, ...)
{
    if (!is.null(cols)) {
        x <- x[,cols]
    }
    mask <- unlist(lapply(x, is.numeric))
    rc <- rep(NA_real_, ncol(x))
    if (any(mask)) {
        rc[mask] <- matrixStats::colMins(as.matrix(x[,mask]), rows=rows, cols=NULL, ...)
    }
    return(rc)
}


########################################################################
#' @export
colMins.dgCMatrix <- function(x, rows=NULL, cols=NULL)
{
    x <- .prune_matrix(x, rows, cols)
    val <- qlcMatrix::colMin(x, ignore.zero=FALSE)
    return(as.vector(val))
}
setMethod('colMins', 'dgCMatrix', colMins.dgCMatrix)


########################################################################
#' A wrapper for matrixStats::rowMaxs()
#' Returns the maximal numeric value per row in a matrix or a dataframe
#' @inheritDotParams matrixStats::rowMaxs
#' @export
rowMaxs <- function (x, ...)
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
rowMaxs.data.frame <- function(x, rows=NULL, cols=NULL, ...)
{
    if (!is.null(cols)) {
        x <- x[,cols]
    }
    x <- dplyr::select_if(x, is.numeric)
    if (ncol(x) == 0) {
        return(numeric())
    }
    return(matrixStats::rowMaxs(as.matrix(x), rows=rows, cols=NULL, ...))
}


########################################################################
#' @export
rowMaxs.dgCMatrix <- function(x, rows=NULL, cols=NULL)
{
    x <- .prune_matrix(x, rows, cols)
    val <- qlcMatrix::rowMax(x, ignore.zero=FALSE)
    return(as.vector(val))
}
setMethod('rowMaxs', 'dgCMatrix', rowMaxs.dgCMatrix)


########################################################################
#' A wrapper for matrixStats::rowMins()
#' Returns the minimal numeric value per row in a matrix or a dataframe
#' @inheritDotParams matrixStats::rowMaxs
#' @export
rowMins <- function (x, ...)
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
rowMins.data.frame <- function(x, rows=NULL, cols=NULL, ...)
{
    if (!is.null(cols)) {
        x <- x[,cols]
    }
    x <- dplyr::select_if(x, is.numeric)
    if (ncol(x) == 0) {
        return(numeric())
    }
    return(matrixStats::rowMins(as.matrix(x), rows=rows, cols=NULL, ...))
}


########################################################################
#' @export
rowMins.dgCMatrix <- function(x, rows=NULL, cols=NULL)
{
    x <- .prune_matrix(x, rows, cols)
    val <- qlcMatrix::rowMin(x, ignore.zero=FALSE)
    return(as.vector(val))
}
setMethod('rowMins', 'dgCMatrix', rowMins.dgCMatrix)


########################################################################
.prune_matrix <- function(x, rows, cols)
{
    if (!is.null(rows) && !is.null(cols)) {
        x <- x[rows, cols]
    }
    else if (!is.null(rows)) {
        x <- x[rows,]
    }
    else if (!is.null(cols)) {
        x <- x[, cols]
    }

    return(x)
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
#' Segment a dataframe according to consecutive identical values of a
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


########################################################################
# Convert a matrix into the tidy format of x, y, val
#' @export
gather_matrix <- function(mtrx, x='x', y='y', val='val')
{
    cnames <- colnames(mtrx)
    rnames <- rownames(mtrx)
    mtrx <- tibble::as_tibble(mtrx)

    if (is.null(cnames)) {
        colnames(mtrx) <- 1:ncol(mtrx)
    }
    else {
        colnames(mtrx) <- paste0('_', cnames)
    }

    if (is.null(rnames)) {
        mtrx[[y]] <- 1:nrow(mtrx)
    }
    else {
        mtrx[[y]] <- rnames
    }

    mtrx <- tidyr::gather(mtrx, !!x, !!val, -!!y)

    if (is.null(cnames)) {
        mtrx[[x]] <- as.integer(mtrx[[x]])
    }
    else {
        mtrx[[x]] <- substring(mtrx[[x]], 2)
    }

    return(mtrx %>% dplyr::select(!!x, !!y, !!val))
}
