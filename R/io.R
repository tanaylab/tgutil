########################################################################
argpack <- function(...)
{

	args <- list(...)
	arg_names <- sapply(substitute(list(...))[-1], deparse)
	if (is.null(names(args))) {
		names(args) <- arg_names
	} else {
        names(args) <- ifelse((names(args) == ''), arg_names, names(args))
    }

	return(args)
}


########################################################################
#' @export
fread_rownames <- function(..., row.var='rowname')
{
	params <- list(...)
	header <- strsplit(readLines(params[[1]], n=1, warn=FALSE), '\t', fixed=TRUE)[[1]]

	params$header = FALSE;
    params$skip = 1;
    params$col.names = c(row.var, header)

	return(do.call(data.table::fread, params))
}


########################################################################
#' @export
fwrite_sparse <- function(x, fname)
{
	ext <- tools::file_ext(fname)
	fname <- tools::file_path_sans_ext(fname)

    if (!is.null(rownames(x))) {
        rows <- tibble(rowname=rownames(x))
        data.table::fwrite(rows, paste0(fname, '.rownames.', ext), row.names=FALSE, col.names=TRUE)
        rownames(x) <- NULL
    }

    if (!is.null(colnames(x))) {
        cols <- tibble(colname=colnames(x))
        data.table::fwrite(cols, paste0(fname, '.colnames.', ext), row.names=FALSE, col.names=TRUE)
        colnames(x) <- NULL
    }

	dims <- tibble(dims=dim(x))
	data.table::fwrite(dims, paste0(fname, '.dims.', ext), row.names=FALSE, col.names=TRUE)

    x <- broom::tidy(x)
    if (any(colnames(x) != c('row', 'column', 'value'))) {
        stop("Failed to convert argumant 'x' into a row-column-value format")
    }

    data.table::fwrite(x, paste0(fname, '.', ext), sep='\t', row.names=FALSE, col.names=TRUE)
}


########################################################################
#' @export
fread_sparse <- function(fname)
{
	ext <- tools::file_ext(fname)
	fname <- tools::file_path_sans_ext(fname)

    x <- data.table::fread(paste0(fname, '.', ext), sep='\t', header=TRUE, data.table=FALSE)
	dims <- data.table::fread(paste0(fname, '.dims.', ext), sep='\t', header=TRUE, data.table=FALSE)

    if (any(colnames(x) != c('row', 'column', 'value'))) {
        stop("File '", fname, "' does not match sparse matrix format")
    }
    x <- Matrix::sparseMatrix(i=x$row, j=x$column, x=x$value, dims=dims$dims)

    rows_fname <- paste0(fname, '.rownames.', ext)
    if (file.exists(rows_fname)) {
        rows <- data.table::fread(rows_fname, sep='\t', header=TRUE, data.table=FALSE)
        rownames(x) <- rows$rowname
    }

    cols_fname <- paste0(fname, '.colnames.', ext)
    if (file.exists(cols_fname)) {
        cols <- data.table::fread(cols_fname, sep='\t', header=TRUE, data.table=FALSE)
        colnames(x) <- cols$colname
    }

    return(x)
}


########################################################################
#' @export
h5_write_sparse <- function(x, fname)
{
	data <- list()
	if (!is.null(rownames(x))) {
		data$rownames <- rownames(x)
        rownames(x) <- NULL
    }

    if (!is.null(colnames(x))) {
		data$colnames <- colnames(x)
        colnames(x) <- NULL
    }

	data$dims <- dim(x)

    x <- broom::tidy(x)
    if (any(colnames(x) != c('row', 'column', 'value'))) {
        stop("Failed to convert argumant 'x' into a row-column-value format")
    }

	data$i <- x$row
	data$j <- x$column
	data$x <- x$value

	h5_write_flat(data, fname)
}


########################################################################
#' @export
h5_read_sparse <- function(fname)
{
	data <- h5_read_flat(fname, c('dims', 'i', 'j', 'x', 'rownames', 'colnames'))
	if (is.null(data$i) || is.null(data$j) || is.null(data$x) || is.null(data$dims)) {
		stop("File '", fname, "' does not contain a sparse matrix")
	}

	x <- Matrix::sparseMatrix(i=data$i, j=data$j, x=data$x, dims=data$dims)

	if (!is.null(data$rownames)) {
		rownames(x) <- data$rownames
	}
	if (!is.null(data$colnames)) {
		colnames(x) <- data$colnames
	}

	return(x)
}
