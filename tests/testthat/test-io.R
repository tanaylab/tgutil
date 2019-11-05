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

test_that("%cache_matrix% returns the same output when called twice", {
    temp_file <- tempfile(fileext = ".tsv")
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        as.matrix(mtcars[mtcars$cyl == cyl, ])
    }

    # Call "calc_mtcars_cyl" function
    res1 <- calc_mtcars_cyl(6) %cache_matrix% temp_file

    # Load cached file
    res2 <- calc_mtcars_cyl(6) %cache_matrix% temp_file

    expect_equal(res1, res2)
})

test_that("%cache_rds% returns the same output when called twice", {
    temp_file <- tempfile(fileext = ".tsv")
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        mtcars[mtcars$cyl == cyl, ]
    }

    # Call "calc_mtcars_cyl" function
    res1 <- calc_mtcars_cyl(6) %cache_rds% temp_file

    # Load cached file
    res2 <- calc_mtcars_cyl(6) %cache_rds% temp_file

    expect_equal(res1, res2)
})

test_that("%cache_df% returns the same output when called twice", {
    temp_file <- tempfile(fileext = ".tsv")
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        as.data.frame(mtcars[mtcars$cyl == cyl, ])
    }

    # Call "calc_mtcars_cyl" function
    res1 <- calc_mtcars_cyl(6) %cache_df% temp_file

    # Load cached file
    res2 <- calc_mtcars_cyl(6) %cache_df% temp_file

    expect_equal(res1, res2)
})

test_that("%cache_df% returns the correct cached file when called twice", {
    temp_file <- tempfile(fileext = ".tsv")
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        as.data.frame(mtcars[mtcars$cyl == cyl, ])
    }

    # Call "calc_mtcars_cyl" function
    res1 <- calc_mtcars_cyl(6) %cache_df% temp_file

    # Load cached file
    res2 <- calc_mtcars_cyl(6) %cache_df% temp_file

    res3 <- tgutil::fread_rownames(temp_file)
    res3 <- tibble::column_to_rownames(res3, "rowname") 

    expect_true(file.exists(temp_file))
    expect_equal(res2, res3)    
})

test_that("%cache_rds% returns the correct cached file when called twice", {
    temp_file <- tempfile(fileext = ".tsv")
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        mtcars[mtcars$cyl == cyl, ]
    }

    # Call "calc_mtcars_cyl" function
    res1 <- calc_mtcars_cyl(6) %cache_rds% temp_file

    # Load cached file
    res2 <- calc_mtcars_cyl(6) %cache_rds% temp_file

    res3 <- readr::read_rds(temp_file)    

    expect_true(file.exists(temp_file))
    expect_equal(res2, res3)    
})

test_that("%fcache_rds% calls the function twice", {
    temp_file <- tempfile(fileext = ".rds")
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        mtcars[mtcars$cyl == cyl, ]
    }

    # Call "calc_mtcars_cyl" function
    res1 <- calc_mtcars_cyl(6) %cache_rds% temp_file

    expect_message(res2 <- calc_mtcars_cyl(6) %fcache_rds% temp_file, "calculating")
})

test_that("%fcache_matrix% calls the function twice", {
    temp_file <- tempfile(fileext = ".tsv")
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        as.matrix(mtcars[mtcars$cyl == cyl, ])
    }

    # Call "calc_mtcars_cyl" function
    res1 <- calc_mtcars_cyl(6) %cache_matrix% temp_file

    expect_message(res2 <- calc_mtcars_cyl(6) %fcache_matrix% temp_file, "calculating")
})

test_that("%fcache_df% calls the function twice", {
    temp_file <- tempfile(fileext = ".tsv")
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        as.data.frame(mtcars[mtcars$cyl == cyl, ])
    }

    # Call "calc_mtcars_cyl" function
    res1 <- calc_mtcars_cyl(6) %cache_df% temp_file

    expect_message(res2 <- calc_mtcars_cyl(6) %fcache_df% temp_file, "calculating")
})


