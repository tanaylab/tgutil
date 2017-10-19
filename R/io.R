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
fwrite_sparse <- function(x, fname)
{
    if (!is.null(rownames(x))) {
        rows <- tibble(rowname=rownames(x))
        data.table::fwrite(rows, paste0(fname, ".rownames"), row.names=FALSE, col.names=TRUE)
        rownames(x) <- NULL
    }

    if (!is.null(colnames(x))) {
        cols <- tibble(colname=colnames(x))
        data.table::fwrite(cols, paste0(fname, ".colnames"), row.names=FALSE, col.names=TRUE)
        colnames(x) <- NULL
    }

    x <- broom::tidy(x)
    if (any(colnames(x) != c('row', 'column', 'value'))) {
        stop("Failed to convert argumant 'x' into a row-column-value format")
    }

    data.table::fwrite(x, fname, sep='\t', row.names=FALSE, col.names=TRUE)
}


########################################################################
fread_sparse <- function(fname)
{
    x <- data.table::fread(fname, sep='\t', header=TRUE, data.table=FALSE)
    if (any(colnames(x) != c('row', 'column', 'value'))) {
        stop("File '", fname, "' does not match sparse matrix format")
    }
    x <- Matrix::sparseMatrix(i=x$row, j=x$column, x=x$value)

    rows_fname <- paste0(fname, '.rownames')
    if (file.exists(rows_fname)) {
        rows <- data.table::fread(rows_fname, sep='\t', header=TRUE, data.table=FALSE)
        if (ncol(rows) != 1) {
            stop("Row names in '", rows_fname, "' have an invalid format")
        }
        if (colnames(rows) != 'rowname') {
            stop("Row names in '", rows_fname, "' have an invalid format")
        }
        if (nrow(rows) != nrow(x)) {
            stop("Number of row names in '", rows_fname, "' does not match matrix")
        }
        rownames(x) <- rows$rowname
    }
    rm(rows)

    cols_fname <- paste0(fname, '.colnames')
    if (file.exists(cols_fname)) {
        cols <- data.table::fread(cols_fname, sep='\t', header=TRUE, data.table=FALSE)
        if (ncol(cols) != 1) {
            stop("Column names in '", cols_fname, "' have an invalid format")
        }
        if (colnames(cols) != 'colname') {
            stop("Column names in '", cols_fname, "' have an invalid format")
        }
        if (nrow(cols) != ncol(x)) {
            stop("Number of Column names in '", cols_fname, "' does not match matrix")
        }
        colnames(x) <- cols$colname
    }

    return(x)
}


########################################################################
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
