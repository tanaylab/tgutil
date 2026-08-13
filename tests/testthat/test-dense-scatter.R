has_colour_scale <- function(p) {
    any(vapply(p$scales$scales, function(s) "colour" %in% s$aesthetics, logical(1)))
}

test_that("density is higher in the crowded cluster than in the sparse one", {
    set.seed(60427)
    x <- c(rnorm(2000, sd = 0.1), rnorm(200, mean = 5, sd = 2))
    y <- c(rnorm(2000, sd = 0.1), rnorm(200, mean = 5, sd = 2))

    d <- dense_scatter_density(x, y)

    expect_length(d, length(x))
    expect_gt(mean(d[1:2000]), mean(d[2001:2200]))
})

test_that("non-finite points get NA density instead of an error", {
    d <- dense_scatter_density(c(rnorm(100), NA, Inf), c(rnorm(100), 0, 0))

    expect_true(all(is.na(d[101:102])))
    expect_false(anyNA(d[1:100]))
})

test_that("legend = TRUE adds a colour scale, and is off by default", {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())

    x <- rnorm(500)
    y <- x + rnorm(500, sd = 0.5)

    expect_false(has_colour_scale(plot_dense_scatter(x, y)))
    expect_true(has_colour_scale(plot_dense_scatter(x, y, legend = TRUE)))
})
