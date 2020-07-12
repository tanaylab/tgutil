tgutil_message <- function(...){
    if (getOption("tgutil.verbose")){
        message(...)    
    }    
}


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
#' Use `fread()` to read a csv/tsv with row names (e.g. one created with `write.table()`)
#' @export
#' @param row.var Name of column that will hold row names.
#'                Setting this parameter to NULL will maintain row names as row names.
#' @inheritDotParams data.table::fread -data.table -key -header -skip -col.names
fread_rownames <- function(..., row.var='rowname')
{
	withCallingHandlers({
		data <- fread(...)
	},
	warning = function(w) {
		if ((as.character(w$call[1]) == 'data.table::fread') &&
		    grepl("Added 1 extra default column name for the first column", w$message, fixed=TRUE)) {
			invokeRestart('muffleWarning')
		}
	})

	if (is.null(row.var)) {
		data <- tibble::column_to_rownames(data, colnames(data)[1])
	}
	else {
		colnames(data)[1] <- row.var
	}

	return(data)
}


########################################################################
#' Wrapper for data.table::fwrite with NA's saved as "NA" instad of "".
#' @export
#' @inheritDotParams data.table::fwrite -na
fwrite <- function(x, ...) {
	data.table::fwrite(x, na = "NA", ...)
}


########################################################################
#' Efficiently write Matrix Market format
#' 
#' @param x sparse matrix
#' @param fname file name
#' @param sep separator for output file
#' @param row.names save row names of the matrix
#' @param col.names save column names of the matrix
#' 
#' @return None
#' 
#' 
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

	fwrite(x, fname, sep=sep, row.names=FALSE, col.names=FALSE, append=TRUE)

	if (!is.null(rows)) {
		fwrite(rows, row.names, row.names=FALSE, col.names=FALSE)
	}
	if (!is.null(cols)) {
		fwrite(cols, col.names, row.names=FALSE, col.names=FALSE)
	}
}


########################################################################
#' Efficiently read Matrix Market format
#' 
#' @param fname name of the file to load from
#' @param sep separator of the input file
#' @param row.names load row names 
#' @param col.names load column names
#' 
#' @return sparse matrix
#' 
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

	x <- fread(fname, sep=sep, header=FALSE, skip=skip, colClasses=c('integer', 'integer', value_type))
	if (nrow(x) != dims[3]) {
		stop("Number of data lines in file does not match dimensions line")
	}

	x <- Matrix::sparseMatrix(i=x[,1], j=x[,2], x=x[,3], dims=dims[1:2])

	if (file.exists(row.names)) {
        rows <- fread(row.names, sep='\t', header=FALSE)
        rownames(x) <- rows[,1]
    }

    if (file.exists(col.names)) {
        cols <- fread(col.names, sep='\t', header=FALSE)
        colnames(x) <- cols[,1]
    }

	return(x)
}


########################################################################
load_vector <- function(fname)
{
    x <- fread(fname, sep='\t', header=FALSE)
    if (ncol(x) == 1) {
        x <- x[,1]
    }
    else if (ncol(x) == 2) {
        x <- structure(x[,2], names=x[,1])
    }
    else {
        stop("File ", fname, " must have one or two columns")
    }

    return(x)
}


########################################################################
#' Read a numeric vector from a file created by save_numeric
#' 
#' @param fname name of file to load from
#' 
#' @export
load_numeric <- function(fname)
{
    return(as.numeric(read_vector(fname)))
}


########################################################################
#' Read a character vector from a file created by save_character
#' 
#' @param fname name of file to load from
#' 
#' @export
load_character <- function(fname)
{
    return(as.character(read_vector(fname)))
}


########################################################################
#' Read a dataframe from a file created by save_dataframe
#' 
#' @param fname name of file to load from
#' 
#' @export
load_dataframe <- function(fname)
{
    x <- fread(fname, sep='\t', header=TRUE)

    if (colnames(x)[1] == '__rownames__') {
        x <- tibble::column_to_rownames(x, '__rownames__')
    }

    return(x)
}


########################################################################
#' Read a matrix from a file created by save_matrix
#' 
#' @param fname name of file to load from
#' 
#' @export
load_matrix <- function(fname)
{
    x <- fread(fname, sep='\t', header=TRUE)

    if (colnames(x)[1] == '__rownames__') {
        x <- tibble::column_to_rownames(x, '__rownames__')
    }

    x <- as.matrix(x)

    if (all(startsWith(colnames(x), '__null_'))) {
        colnames(x) <- NULL
    }

    return(x)
}


########################################################################
save_vector <- function(x, fname)
{
    stopifnot(is.vector(x))

    if (is.null(names(x))) {
        x <- tibble::tibble(x=x)
    }
    else {
        x <- tibble::tibble(names=names(x), x=x)
    }

    fwrite(x, fname, sep='\t', row.names=FALSE, col.names=FALSE)
}


########################################################################
#' Write a numeric vector to a file, preserving cell names.
#' @export
save_numeric <- function(x, fname)
{
    stopifnot(is.numeric(x))
    return(write_vector(x, fname))
}


########################################################################
#' Write a character vector to a file, preserving cell names.
#' @export
save_character <- function(x, fname)
{
    stopifnot(is.character(x))
    return(write_vector(x, fname))
}


########################################################################
#' Writes a dataframe to a file, preserving row names.
#' @export
save_dataframe <- function(x, fname)
{
    stopifnot(is.data.frame(x))
    if (!is.null(rownames(x)) &&
        !all(rownames(x) == 1:nrow(x))) {
        x <- tibble::rownames_to_column(x, '__rownames__')
    }

    fwrite(x, fname, sep='\t', col.names=TRUE, row.names=FALSE)
}


########################################################################
#' Writes a matrix to a file, preserving row and column names.
#' @export
save_matrix <- function(x, fname)
{
    stopifnot(is.matrix(x))

    if (is.null(colnames(x))) {
        colnames(x) <- paste0('__null_', 1:ncol(x), '__')
    }

    x <- as.data.frame(x)

    if (!is.null(rownames(x))) {
        x <- tibble::rownames_to_column(x, '__rownames__')
    }

    fwrite(x, fname, sep='\t', col.names=TRUE, row.names=FALSE)
}


########################################################################
#' Tries reading a cached matrix result from the filename on the right
#' side. If the file is missing, generate the matrix using the function
#' call on the left side and write the result to the file. If "tgutil.cache"
#' option is FALSE, right side would be always recumputed.
#' 
#' @param call function call
#' @param fname filename
#' 
#' 
#' @examples
#' temp_file <- tempfile(fileext = ".tsv")
#'   calc_mtcars_cyl <- function(cyl) {
#'       message("calculating")
#'       as.matrix(mtcars[mtcars$cyl == cyl, ])
#'   }
#'
#'   # Call "calc_mtcars_cyl" function
#'   res1 <- calc_mtcars_cyl(6) %cache_matrix% temp_file
#'
#'   # Load cached file
#'   res2 <- calc_mtcars_cyl(6) %cache_matrix% temp_file
#'
#' @export
`%cache_matrix%` <- function(call, fname)
{
	if (!getOption("tgutil.cache")){
		return(call %fcache_matrix% fname)
	}
	
    if (file.exists(fname)) {
        tgutil_message("Using cached matrix from '", fname, "'")
        return(load_matrix(fname))
    }

    result <- call
    save_matrix(result, fname)
    return(result)
}

########################################################################
#' cache_matrix that force recreating the cached file
#' 
#' @param call function call
#' @param fname filename
#' 
#' 
#' @examples
#' temp_file <- tempfile(fileext = ".tsv")
#'   calc_mtcars_cyl <- function(cyl) {
#'       message("calculating")
#'       as.matrix(mtcars[mtcars$cyl == cyl, ])
#'   }
#'
#'   # Call "calc_mtcars_cyl" function
#'   res1 <- calc_mtcars_cyl(6) %cache_matrix% temp_file
#'
#'   # Force re-calling the function
#'   res2 <- calc_mtcars_cyl(6) %fcache_matrix% temp_file
#'
#' @export
`%fcache_matrix%` <- function(call, fname)
{
    result <- call
    save_matrix(result, fname)
    return(result)
}



########################################################################
#' Tries reading a cached dataframe result from the filename on the right
#' side. If the file is missing, generate the dataframe using the
#' function call on the left side and write the result to the file. If "tgutil.cache"
#' option is FALSE, right side would be always recumputed.
#' 
#' @param call function call
#' @param fname filename
#' 
#' 
#' @examples
#' temp_file <- tempfile(fileext = ".tsv")
#'   calc_mtcars_cyl <- function(cyl) {
#'       message("calculating")
#'       as.data.frame(mtcars[mtcars$cyl == cyl, ])
#'   }
#'
#'   # Call "calc_mtcars_cyl" function
#'   res1 <- calc_mtcars_cyl(6) %cache_df% temp_file
#'
#'   # Load cached file
#'   res2 <- calc_mtcars_cyl(6) %cache_df% temp_file
#'
#' @export
`%cache_df%` <- function(call, fname)
{
	if (!getOption("tgutil.cache")){
		return(call %fcache_df% fname)
	}
	
    if (file.exists(fname)) {
        tgutil_message("Using cached dataframe from '", fname, "'")
        return(load_dataframe(fname))
    }

    result <- call
    save_dataframe(result, fname)
    return(result)
}

########################################################################
#' cache_df that force recreating the cached file
#' 
#' @param call function call
#' @param fname filename
#' 
#' 
#' @examples
#' temp_file <- tempfile(fileext = ".tsv")
#'   calc_mtcars_cyl <- function(cyl) {
#'       message("calculating")
#'       as.data.frame(mtcars[mtcars$cyl == cyl, ])
#'   }
#'
#'   # Call "calc_mtcars_cyl" function
#'   res1 <- calc_mtcars_cyl(6) %cache_df% temp_file
#'
#'   # Force re-calling the function
#'   res2 <- calc_mtcars_cyl(6) %fcache_df% temp_file
#'
#' @export
`%fcache_df%` <- function(call, fname)
{
    result <- call
    save_dataframe(result, fname)
    return(result)
}

########################################################################
#' Tries reading a cached dataframe result from the filename on the right
#' side. If the file is missing, generate the dataframe using the
#' function call on the left side and write the result to the file. If "tgutil.cache"
#' option is FALSE, right side would be always recumputed.
#' 
#' @param call function call
#' @param fname filename
#' 
#' 
#' @examples
#' temp_file <- tempfile(fileext = "rds")
#'   calc_mtcars_cyl <- function(cyl) {
#'       message("calculating")
#'       mtcars[mtcars$cyl == cyl, ]
#'   }
#'
#'   # Call "calc_mtcars_cyl" function
#'   res1 <- calc_mtcars_cyl(6) %cache_rds% temp_file
#'
#'   # Load cached file
#'   res2 <- calc_mtcars_cyl(6) %cache_rds% temp_file
#'
#' @export
`%cache_rds%` <- function(call, fname)
{
	if (!getOption("tgutil.cache")){
		return(call %fcache_rds% fname)
	}
	
    if (file.exists(fname)) {
        tgutil_message("Using cached rds from '", fname, "'")
        return(readr::read_rds(fname))
    }

    result <- call
    readr::write_rds(result, fname)
    return(result)
}

########################################################################
#' cache_rds that force recreating the cached file
#' 
#' @param call function call
#' @param fname filename
#' 
#' 
#' @examples
#' temp_file <- tempfile(fileext = "rds")
#'   calc_mtcars_cyl <- function(cyl) {
#'       message("calculating")
#'       mtcars[mtcars$cyl == cyl, ]
#'   }
#'
#'   # Call "calc_mtcars_cyl" function
#'   res1 <- calc_mtcars_cyl(6) %cache_rds% temp_file
#'
#'   # Force re-calling the function
#'   res2 <- calc_mtcars_cyl(6) %fcache_rds% temp_file
#'
#' @export
`%fcache_rds%` <- function(call, fname)
{
    result <- call
    readr::write_rds(result, fname)
    return(result)
}
