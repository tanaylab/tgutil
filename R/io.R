########################################################################
#' @export
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
#' Wrapper for data.table::fread. Returns a data.frame instead of a data.table
#' @export
#' @inheritDotParams data.table::fread -data.table -key
fread <- function(...) {
	return(data.table::fread(..., data.table=FALSE))
}


########################################################################
#' Use `fread()` to read a csv/tsv with row names (e.g. one created with `read.table()`)
#' @export
#' @param row.var Name of column that will hold row names.
#'                Setting this parameter to NULL will maintain row names as row names.
#' @inheritDotParams data.table::fread -data.table -key -header -skip -col.names
fread_rownames <- function(..., row.var='rowname')
{
	use_rownames = FALSE
	if (is.null(row.var)) {
		use_rownames = TRUE
		row.var = '__rowname__'
	}

	params <- list(...)
	header <- strsplit(readLines(params[[1]], n=1, warn=FALSE), '\t', fixed=TRUE)[[1]]

	params$header = FALSE;
    params$skip = 1;
    params$col.names = c(row.var, header)

	data <- do.call(fread, params)
	if (use_rownames) {
		data <- tibble::column_to_rownames(data, '__rowname__')
	}

	return(data)
}


########################################################################
#' Wrapper for data.table::fwrite
#' @export
#' @inheritParams data.table::fwrite
#' @inheritDotParams data.table::fwrite
fwrite <- function(x, ...) {
	data.table::fwrite(x, ...)
}


########################################################################
#' @export
fwrite_mm <- function(x, fname, sep=' ', row.names=TRUE, col.names=TRUE)
{
	rows <- NULL
	cols <- NULL

	dims <- dim(x)
	if (length(dims) != 2) {
		stop("fwrite_mm() can only write two-dimensional metrices")
	}

	if (!is.null(rownames(x)) && ((row.names==TRUE) || is.character(row.names))) {
		if (row.names==TRUE) {
			row.names <- paste0(fname, '.rownames')
		}
        rows <- tibble(rowname=rownames(x))
        rownames(x) <- NULL
    }

    if (!is.null(colnames(x)) && ((col.names==TRUE) || is.character(col.names))) {
		if (col.names == TRUE) {
			col.names <- paste0(fname, '.colnames')
		}
        cols <- tibble(colname=colnames(x))
        # data.table::fwrite(cols, col.names, row.names=FALSE, col.names=FALSE)
        colnames(x) <- NULL
    }

	x <- broom::tidy(x)

	if (any(colnames(x) != c('row', 'column', 'value'))) {
        stop("Failed to convert argumant 'x' into a row-column-value format")
    }
	if (mode(x$column) != 'numeric') {
		stop("fwrite_mm() can only write numerical matrices")
	}

	x <- arrange(x, row, column)

	output <- file(fname, open='w')
	writeLines('%%MatrixMarket matrix coordinate real general', output)
	writeLines(paste(dims[1], dims[2], nrow(x), sep=sep), output)
	close(output)

	data.table::fwrite(x, fname, sep=sep, row.names=FALSE, col.names=FALSE, append=TRUE)

	if (!is.null(rows)) {
		data.table::fwrite(rows, row.names, row.names=FALSE, col.names=FALSE)
	}
	if (!is.null(cols)) {
		data.table::fwrite(cols, col.names, row.names=FALSE, col.names=FALSE)
	}
}


########################################################################
#' @export
fread_mm <- function(fname, sep=' ', row.names=TRUE, col.names=TRUE)
{
	if (row.names==TRUE) {
		row.names <- paste0(fname, '.rownames')
	}
	if (col.names == TRUE) {
		col.names <- paste0(fname, '.colnames')
	}

	input <- file(fname, open='r')

	header <- readLines(input, n=1, warn=FALSE)
	if (sep == ' ') {
		header <- strsplit(header, ' +')[[1]]
	}
	else {
		header <- strsplit(header, sep, fixed=TRUE)[[1]]
	}
	if (header[1] != '%%MatrixMarket') {
		close(input)
		stop("File does not contain a MatrixMarket header")
	}
	if ((header[2] != 'matrix') || (header[3] != 'coordinate') || (header[5] != 'general')) {
		close(input)
		stop("fread_mm() can only read matrices in general, coordinate format")
	}

	VALUE_TYPES <- c('real'='numeric', 'integer'='integer')
	value_type <- VALUE_TYPES[header[4]]
	names(value_type) <- NULL
	if (is.na(value_type)) {
		close(input)
		stop("File contains an unsupported value type: ", header[4])
	}

	skip <- 1
	while(TRUE) {
		line = readLines(input, n=1, warn=FALSE)
		skip <- skip + 1
		if (!startsWith(line, '%')) {
			break
		}
	}

	if (sep == ' ') {
		dims <- strsplit(line, ' +')[[1]]
	} else {
		dims <- strsplit(line, sep, fixed=TRUE)[[1]]
	}
	dims <- as.numeric(dims)
	if (length(dims) != 3) {
		close(input)
		stop("Could not parse matrix dimenstions line")
	}
	if (any(as.integer(dims) != dims)) {
		close(input)
		stop("Dimensions line contains non-integer values")
	}
	close(input)

	x <- data.table::fread(fname, sep=sep, header=FALSE, skip=skip, colClasses=c('integer', 'integer', value_type), data.table=FALSE)
	if (nrow(x) != dims[3]) {
		stop("Number of data lines in file does not match dimensions line")
	}

	x <- Matrix::sparseMatrix(i=x[,1], j=x[,2], x=x[,3], dims=dims[1:2])

	if (file.exists(row.names)) {
        rows <- data.table::fread(row.names, sep='\t', header=FALSE, data.table=FALSE)
        rownames(x) <- rows[,1]
    }

    if (file.exists(col.names)) {
        cols <- data.table::fread(col.names, sep='\t', header=FALSE, data.table=FALSE)
        colnames(x) <- cols[,1]
    }

	return(x)
}
