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
