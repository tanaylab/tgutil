test_that("fread_rownames() can read the output of write.table", {
    DIM_LEN <- 10
    data <- round(runif(DIM_LEN**2, min=0, max=1000)) / 1000.
    dim(data) <- c(DIM_LEN,DIM_LEN)
    data <- as.data.frame(data)

    dimnames <- list()
    for (i in 1:ceiling(log10(DIM_LEN))) {
        dimnames <- c(dimnames, list(LETTERS[1:10]))
    }
    dimnames <- do.call(expand.grid, c(dimnames, list(stringsAsFactors=FALSE)))
    dimnames <- do.call(paste0, rev(dimnames))[1:DIM_LEN]

    colnames(data) <- paste0('c', dimnames)
    rownames(data) <- paste0('r', dimnames)

    fname <- tempfile('fread_test.')
    write.table(data, fname, quote=FALSE, sep='\t', row.names=TRUE, col.names=TRUE)

    result <- tgutil::fread_rownames(fname, row.var=NULL)

    expect_identical(data, result)
})
