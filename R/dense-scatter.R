# 2D kernel density at each point - the guts of grDevices::densCols(), which
# returns colors only. We need the density itself so it can be mapped to a
# scale, and hence get a legend.
#' @noRd
dense_scatter_density <- function(x, y, nbin = 128) {
    dens <- rep(NA_real_, length(x))
    ok <- is.finite(x) & is.finite(y)
    xy <- cbind(x[ok], y[ok])

    bandwidth <- diff(apply(xy, 2, stats::quantile, probs = c(0.05, 0.95), na.rm = TRUE, names = FALSE)) / 25
    bandwidth[bandwidth == 0] <- 1
    map <- KernSmooth::bkde2D(xy, bandwidth = bandwidth, gridsize = c(nbin, nbin))

    mkBreaks <- function(u) u - diff(range(u)) / (length(u) - 1) / 2
    d <- map$fhat[cbind(
        cut(xy[, 1], mkBreaks(map$x1), labels = FALSE),
        cut(xy[, 2], mkBreaks(map$x2), labels = FALSE)
    )]
    d[is.na(d)] <- 0

    dens[ok] <- d
    dens
}

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
        data$density <- dense_scatter_density(data$x, data$y)

        # Same equal-width binning densCols() uses, so the points look identical
        # whether or not a legend is drawn
        n <- nrow(data)
        data$colour <- grDevices::colorRampPalette(pal)(n)[cut(data$density, n, labels = FALSE)]
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
#' @section Computed variables:
#' \describe{
#'   \item{\code{density}}{2D kernel density estimate at each point.}
#' }
#' The layer colors points directly and therefore draws no legend. To get one,
#' map the density and add a matching color scale:
#' \preformatted{
#' ggplot(df, aes(x, y)) +
#'     geom_dense_scatter(aes(colour = after_stat(density))) +
#'     scale_colour_gradientn(colours = c("darkgray", "blue3", "red", "yellow"))
#' }
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
#' @param legend If \code{TRUE}, draw a density color bar. Labelled "low"/"high",
#'        since absolute kernel-density values are not interpretable. Default is \code{FALSE}.
#' @param legend_title Title of the color bar. Default is "Density".
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
                               abline = FALSE,
                               intercept = 0,
                               slope = 1,
                               show_r2 = FALSE,
                               xlim = NULL,
                               ylim = NULL,
                               legend = FALSE,
                               legend_title = "Density",
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

    if (show_r2) {
        fit <- lm(y ~ x)
        r2 <- summary(fit)$r.squared
        subtitle <- bquote(R^2 == .(round(r2, 3)))
    } else {
        subtitle <- NULL
    }

    # Create the plot
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
        geom_dense_scatter(
            mapping = if (legend) ggplot2::aes(colour = ggplot2::after_stat(density)),
            pal = pal, size = size, alpha = alpha, ...
        ) +
        ggplot2::labs(x = xlab, y = ylab, title = main, subtitle = subtitle) +
        ggplot2::theme_classic()

    if (legend) {
        # Absolute kernel-density values mean nothing to a reader, so the bar is
        # labelled low -> high
        p <- p + ggplot2::scale_colour_gradientn(
            colours = pal,
            name = legend_title,
            breaks = function(lims) lims,
            labels = c("low", "high")
        )
    }

    if (!is.null(xlim)) {
        p <- p + ggplot2::xlim(xlim)
    }

    if (!is.null(ylim)) {
        p <- p + ggplot2::ylim(ylim)
    }

    if (abline) {
        p <- p + ggplot2::geom_abline(intercept = intercept, slope = slope)
    }

    print(p)
}
