#' @noRd
StatDenseScatter <- ggplot2::ggproto("StatDenseScatter", ggplot2::Stat,
    required_aes = c("x", "y"),
    setup_params = function(data, params) {
        if (is.null(params$pal)) {
            params$pal <- c("darkgray", "blue3", "red", "yellow")
        }
        params
    },
    compute_group = function(data, scales, pal) {
        # Calculate density pal
        p_coldens <- grDevices::densCols(
            x = data$x,
            y = data$y,
            colramp = grDevices::colorRampPalette(pal)
        )

        # Add color to the data
        data$colour <- p_coldens
        data
    }
)

#' Create a density-colored scatter plot
#'
#' @description
#' This geom creates a scatter plot where points are colored by their local density,
#' using a custom color gradient.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}
#' @param data The data to be displayed in this layer
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed
#' @param show.legend logical. Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics are mapped
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them
#' @param pal Color palette. A vector of colors to use for the density gradient, from lowest to highest density
#' @param size Point size
#' @param alpha Point alpha/transparency
#' @param ... Other arguments passed on to \code{\link[ggplot2]{layer}}
#'
#' @return A ggplot2 layer that can be added to a plot
#'
#' @examples
#' # Create large dataset with multiple clusters
#' library(ggplot2)
#' set.seed(60427)
#' n <- 1e4
#' df <- data.frame(
#'     x = c(rnorm(n * 0.5), rnorm(n * 0.3, 3, 0.5), rnorm(n * 0.2, 0, 2)),
#'     y = c(rnorm(n * 0.5), rnorm(n * 0.3, 3, 0.5), rnorm(n * 0.2, 0, 2))
#' )
#'
#' # Basic usage with default settings
#' ggplot(df, aes(x, y)) +
#'     geom_dense_scatter()
#'
#' # Custom color palette to highlight density variations
#' ggplot(df, aes(x, y)) +
#'     geom_dense_scatter(
#'         pal = c("lightblue", "darkblue", "purple", "red"),
#'         size = 0.5
#'     )
#'
#' # Create large dataset with non-linear relationship
#' x <- runif(n, -3, 3)
#' df2 <- data.frame(
#'     x = x,
#'     y = sin(x) * 2 + rnorm(n, 0, 0.5)
#' )
#'
#' # Visualize non-linear relationship with density
#' ggplot(df2, aes(x, y)) +
#'     geom_dense_scatter(
#'         pal = c("gray90", "gray50", "orange", "red"),
#'         size = 0.4,
#'         alpha = 0.8
#'     ) +
#'     labs(title = "Non-linear Pattern with Density Coloring")
#'
#' # Create large dataset with varying spread
#' x <- rnorm(n)
#' df3 <- data.frame(
#'     x = x,
#'     y = x * rnorm(n, mean = 1, sd = abs(x) / 2)
#' )
#'
#' # Visualize heteroscedastic pattern
#' ggplot(df3, aes(x, y)) +
#'     geom_dense_scatter(
#'         pal = c("#F5F5F5", "#4169E1", "#FF4500"),
#'         size = 0.3
#'     ) +
#'     theme_minimal() +
#'     labs(title = "Varying Spread Pattern")
#' @export
geom_dense_scatter <- function(
    mapping = NULL,
    data = NULL,
    stat = "DenseScatter",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    pal = NULL,
    size = 0.8,
    alpha = 1,
    ...) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = "point",
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            pal = pal,
            size = size,
            alpha = alpha,
            ...
        )
    )
}


#' Create Density-Colored Scatter Plots with ggplot2
#'
#' @description
#' Creates a scatter plot where points are colored by their local density using ggplot2.
#' This function provides a convenient wrapper around geom_dense_scatter that mimics
#' the behavior of base R's plot function while providing modern visualization features.
#'
#' @param x A vector of x coordinates, or a matrix/data frame with 2 columns where
#'          the first column is used as x coordinates.
#' @param y A vector of y coordinates. Optional if x is a matrix/data frame.
#' @param xlab Character string for x-axis label. If NULL, the name of x is used.
#' @param ylab Character string for y-axis label. If NULL, the name of y is used.
#' @param main Character string for the plot title.
#' @param pal Color palette vector. Colors to use for density gradient from lowest
#'            to highest density. Default is c("darkgray", "blue3", "red", "yellow").
#' @param size Numeric value for point size. Default is 0.8.
#' @param alpha Numeric value between 0 and 1 for point transparency. Default is 1.
#' @param ... Additional arguments passed to geom_dense_scatter.
#'
#'
#' @details
#' The function automatically handles different input types and provides appropriate
#' axis labels based on the input variable names. It uses geom_dense_scatter for
#' the actual plotting, which colors points based on their local density.
#'
#' The density coloring helps visualize patterns in large datasets by highlighting
#' areas of high point concentration. The color gradient can be customized using
#' the pal parameter.
#'
#' @section Input Handling:
#' The function accepts inputs in several formats:
#' * Two vectors of equal length for x and y coordinates
#' * A matrix or data frame with 2 columns (first for x, second for y)
#' * A matrix or data frame with x parameter only (uses first two columns)
#'
#' @examples
#' # Basic usage with vectors
#' x <- rnorm(1e4)
#' y <- x + rnorm(1e4, sd = 0.5)
#' plot_dense_scatter(x, y)
#'
#' # Using a data frame
#' df <- data.frame(
#'     x = rnorm(1e4),
#'     y = rnorm(1e4)
#' )
#' plot_dense_scatter(df$x, df$y,
#'     xlab = "X axis",
#'     ylab = "Y axis",
#'     main = "My Plot"
#' )
#'
#' # Using matrix-like input
#' plot_dense_scatter(df)
#'
#' # Customizing appearance
#' plot_dense_scatter(x, y,
#'     pal = c("gray90", "blue", "red"),
#'     size = 0.5,
#'     alpha = 0.8,
#'     main = "Custom Dense Scatter Plot"
#' )
#'
#' # Visualizing non-linear relationships
#' x <- runif(1e4, -3, 3)
#' y <- sin(x) * 2 + rnorm(1e4, 0, 0.5)
#' plot_dense_scatter(x, y,
#'     pal = c("gray90", "gray50", "orange", "red"),
#'     size = 0.4,
#'     main = "Non-linear Pattern with Density Coloring"
#' )
#'
#' @seealso
#' \code{\link{geom_dense_scatter}} for the underlying plotting function
#'
#'
#' @export
plot_dense_scatter <- function(x, y,
                               xlab = NULL,
                               ylab = NULL,
                               main = NULL,
                               pal = c("darkgray", "blue3", "red", "yellow"),
                               size = 0.8,
                               alpha = 1,
                               ...) {
    # Handle different input types
    if (is.matrix(x) || is.data.frame(x)) {
        if (missing(y)) {
            y <- x[, 2]
            x <- x[, 1]
        }
    }

    # Create data frame
    df <- data.frame(x = x, y = y)

    # Handle labels
    if (is.null(xlab)) {
        xlab <- deparse(substitute(x))
    }
    if (is.null(ylab)) {
        ylab <- deparse(substitute(y))
    }

    # Create the plot
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        geom_dense_scatter(pal = pal, size = size, alpha = alpha, ...) +
        ggplot2::labs(x = xlab, y = ylab, title = main) +
        ggplot2::theme_classic()

    print(p)
}
