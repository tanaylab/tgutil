test_that("get_csv wrappd function returns the same output when called twice", {
    temp_file <- tempfile(fileext = ".csv")
 
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        as.data.frame(mtcars[mtcars$cyl == cyl, ])
    }
     
    get_mtcars_cyl <- get_csv(calc_mtcars_cyl, temp_file, row.names=TRUE)

    # Call "calc_mtcars_cyl" function
    res1 <- get_mtcars_cyl(6)

    # Load cached file
    res2 <- get_mtcars_cyl(6)

    expect_equal(res1, res2)
})

test_that("get_csv wrappd function returns the same output when called twice and no rownames", {
    temp_file <- tempfile(fileext = ".csv")
 
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        res <- mtcars[mtcars$cyl == cyl, ]
        rownames(res) <- NULL
        as.data.frame(res)
    }
     
    get_mtcars_cyl <- get_csv(calc_mtcars_cyl, temp_file, row.names=FALSE)

    # Call "calc_mtcars_cyl" function
    res1 <- get_mtcars_cyl(6)

    # Load cached file
    res2 <- get_mtcars_cyl(6)

    expect_equal(res1, res2)
})


test_that("get_csv wrappd function returns the correct cached file when called twice", {
    temp_file <- tempfile(fileext = ".csv")
 
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        as.data.frame(mtcars[mtcars$cyl == cyl, ])
    }
     
    get_mtcars_cyl <- get_csv(calc_mtcars_cyl, temp_file, row.names=TRUE)

    # Call "calc_mtcars_cyl" function
    res1 <- get_mtcars_cyl(6)

    # Load cached file
    res2 <- get_mtcars_cyl(6)    
    res3 <- tgutil::fread_rownames(temp_file)
    res3 <- tibble::column_to_rownames(res3, "rowname") 

    expect_true(file.exists(temp_file))
    expect_equal(res2, res3)    
})


test_that("get_csv wrappd function is re-called when recalc is TRUE ", {
    temp_file <- tempfile(fileext = ".csv")
 
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        as.data.frame(mtcars[mtcars$cyl == cyl, ])
    }
     
    get_mtcars_cyl <- get_csv(calc_mtcars_cyl, temp_file, row.names=TRUE)

    # Call "calc_mtcars_cyl" function
    res1 <- get_mtcars_cyl(6)

    # Load cached file
    res2 <- get_mtcars_cyl(6)

     # Force re-calculating
    expect_message(get_mtcars_cyl(6, recalc=TRUE))
})

test_that("get_rds wrappd function returns the same output when called twice", {
    temp_file <- tempfile(fileext = ".rds")
 
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        mtcars[mtcars$cyl == cyl, ]
    }
     
    get_mtcars_cyl <- get_rds(calc_mtcars_cyl, temp_file)

    # Call "calc_mtcars_cyl" function
    res1 <- get_mtcars_cyl(6)

    # Load cached file
    res2 <- get_mtcars_cyl(6)

    expect_equal(res1, res2)
})


test_that("get_rds wrappd function returns the correct cached file when called twice", {
    temp_file <- tempfile(fileext = ".csv")
 
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        mtcars[mtcars$cyl == cyl, ]
    }
     
    get_mtcars_cyl <- get_rds(calc_mtcars_cyl, temp_file)

    # Call "calc_mtcars_cyl" function
    res1 <- get_mtcars_cyl(6)

    # Load cached file
    res2 <- get_mtcars_cyl(6)    
    res3 <- readr::read_rds(temp_file)    

    expect_true(file.exists(temp_file))
    expect_equal(res2, res3)    
})


test_that("get_rds wrappd function is re-called when recalc is TRUE ", {
    temp_file <- tempfile(fileext = ".csv")
 
    calc_mtcars_cyl <- function(cyl) {
        message("calculating")
        mtcars[mtcars$cyl == cyl, ]
    }
     
    get_mtcars_cyl <- get_rds(calc_mtcars_cyl, temp_file)

    # Call "calc_mtcars_cyl" function
    res1 <- get_mtcars_cyl(6)

    # Load cached file
    res2 <- get_mtcars_cyl(6)

     # Force re-calculating
    expect_message(get_mtcars_cyl(6, recalc=TRUE))
})