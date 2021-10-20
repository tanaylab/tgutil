########################################################################
#' Easily plot a matrix as an heatmap using ggplot2
#' @param mtrx The matrix to be plotted
#' @param col_names The names that will be used as the X-axis labels.
#'                  A NULL (the defualt) indicates that the original matrix colnames are used.
#'                  A FALSE indicates that no X-axis labels should be displayed.
#' @param row_names The names that will be used as the Y-axis labels.
#'                  A NULL (the defualt) indicates that the original matrix rownames are used.
#'                  A FALSE indicates that no Y-axis labels should be displayed.
#' @param xlab The X-axis label
#' @param ylab The Y-axis label
#' @param plot_top Whether to put the X-axis labels at the top of the heatmap (as well as at the bottom)
#' @param plot_right Whether to put the Y-axis labels at the right side of the heatmap (as well as at the left hand side)
#' @param interleave Plot the odd Y-Axis labels on the left side and the even on the right side
#' @param col_names_orient Orientation of the X-axis label
#'
#' @export
tgplot_heatmap <- function(mtrx, col_names = NULL, row_names = NULL, xlab = NULL, ylab = NULL,
                           plot_top = TRUE, plot_right = TRUE, interleave = FALSE,
                           col_names_orient = c("horizontal", "vertical", "slanted")) {
    col_names_orient <- match.arg(col_names_orient)

    if (is.null(col_names)) {
        col_names <- colnames(mtrx)
    }
    else if ((length(col_names) == 1) && !col_names) {
        col_names <- NULL
    }
    if (!is.null(col_names) && (length(col_names) != ncol(mtrx))) {
        stop("Length of col_names must equal the number of columns")
    }

    if (is.null(row_names)) {
        row_names <- rownames(mtrx)
    }
    else if ((length(row_names) == 1) && !row_names) {
        row_names <- NULL
    }
    if (!is.null(row_names) && (length(row_names) != nrow(mtrx))) {
        stop("Length of row_names must equal the number of rows")
    }

    colnames(mtrx) <- NULL
    rownames(mtrx) <- NULL
    ggp <- ggplot2::ggplot(gather_matrix(mtrx), ggplot2::aes(x = x, y = y, fill = val)) +
        ggplot2::geom_raster() +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.ticks = ggplot2::element_blank())

    if (!is.null(xlab)) {
        ggp <- ggp + ggplot2::xlab(xlab)
    }
    else {
        ggp <- ggp + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }
    if (!is.null(ylab)) {
        ggp <- ggp + ggplot2::ylab(ylab)
    }
    else {
        ggp <- ggp + ggplot2::theme(axis.title.y = ggplot2::element_blank())
    }

    if (!is.null(col_names)) {
        if (plot_top) {
            sec.axis_top <- ggplot2::dup_axis()
        } else {
            sec.axis_top <- ggplot2::waiver()
        }

        ggp <- ggp +
            ggplot2::scale_x_continuous(breaks = 1:ncol(mtrx), labels = col_names, expand = c(0, 0), sec.axis = sec.axis_top) +
            switch(col_names_orient,
                "horizontal" = ggplot2::theme(),
                "vertical" = ggplot2::theme(
                    axis.text.x.top = element_text(angle = -90, hjust = 1, vjust = 0.5),
                    axis.text.x.bottom = element_text(angle = -90, hjust = 0, vjust = 0.5)
                ),
                "slanted" = ggplot2::theme(
                    axis.text.x.top = element_text(angle = -45, hjust = 1, vjust = 0),
                    axis.text.x.bottom = element_text(angle = -45, hjust = 0, vjust = 1)
                )
            )
    }
    else {
        ggp <- ggp +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::theme(axis.text.x = ggplot2::element_blank())
    }

    if (!is.null(row_names)) {
        if (interleave){            
            sec.axis_right <- ggplot2::dup_axis(
                breaks = seq(2, nrow(mtrx), 2), 
                labels = row_names[seq(2, nrow(mtrx), 2)])
            ggp <- ggp +
                ggplot2::scale_y_continuous(
                    breaks = seq(1, nrow(mtrx), 2), 
                    labels = row_names[seq(1, nrow(mtrx), 2)], 
                    expand = c(0, 0), 
                    sec.axis = sec.axis_right)
        } else {
            if (plot_right) {            
                sec.axis_right <- ggplot2::dup_axis()
            } else {
                sec.axis_right <- ggplot2::waiver()
            }

            ggp <- ggp +
                ggplot2::scale_y_continuous(breaks = 1:nrow(mtrx), labels = row_names, expand = c(0, 0), sec.axis = sec.axis_right)
        }
    } else {
        ggp <- ggp +
            ggplot2::scale_y_continuous(expand = c(0, 0)) +
            ggplot2::theme(axis.text.y = ggplot2::element_blank())
    }


    return(ggp)
}


########################################################################
#' @export
tgplot_add_axis_annotation <- function(heatmap, annotation, position = "bottom", size = 0.02) {
    position <- char.expand(position, c("top", "bottom", "left", "right"))
    if (!is(annotation, "gg")) {
        if (position %in% c("top", "bottom")) {
            ggdata <- tibble::tibble(x = 1:length(annotation), y = 1)
        }
        else {
            ggdata <- tibble::tibble(x = 1, y = 1:length(annotation))
        }

        annotation <- ggplot2::ggplot(ggdata, aes(x = x, y = y)) +
            ggplot2::geom_raster(fill = annotation) +
            ggplot2::coord_cartesian(expand = FALSE) +
            ggplot2::theme_void() +
            ggplot2::theme(panel.border = element_rect(size = 0.2, color = "black", fill = NA))
    }

    if (position %in% c("top", "bottom")) {
        heatmap <- cowplot::insert_xaxis_grob(heatmap, annotation, grid::unit(size, "null"), position = position)
    }
    else {
        heatmap <- cowplot::insert_yaxis_grob(heatmap, annotation, grid::unit(size, "null"), position = position)
    }

    return(heatmap)
}


########################################################################
#' @export
scale_fill_gradientn_abs <- function(..., colors, values, abs_range) {
    if (length(colors) != length(values)) {
        stop("colors and values must have equal lengths")
    }

    abs_range <- range(abs_range, na.rm = TRUE)
    low <- abs_range[1]
    high <- abs_range[2]

    colortab <- tibble(value = values, color = colors)
    inter <- colortab %>%
        dplyr::filter(value > low, value < high) %>%
        dplyr::mutate(value = scales::rescale(value, to = c(0, 1), from = c(low, high)))
    low_interp <- dplyr::bind_rows(
        colortab %>% dplyr::filter(value <= low) %>% tail(1),
        colortab %>% dplyr::filter(value > low) %>% head(1)
    )
    high_interp <- dplyr::bind_rows(
        colortab %>% dplyr::filter(value < high) %>% tail(1),
        colortab %>% dplyr::filter(value >= high) %>% head(1)
    )
    low_color <- colorRamp(low_interp$color, space = "Lab")(scales::rescale(low, to = c(0, 1), from = low_interp$value))
    high_color <- colorRamp(high_interp$color, space = "Lab")(scales::rescale(high, to = c(0, 1), from = high_interp$value))

    values <- c(0, inter$value, 1)
    colors <- c(rgb(low_color / 255), inter$color, rgb(high_color / 255))

    return(ggplot2::scale_fill_gradientn(..., colors = colors, values = values))
}



#' Plot using preview instead of plotting to X device
#'
#' @description uses \code{\link[ggplot2]{ggsave}} to save a ggplot object to file and then opens a new device and displays it.
#'
#' @inheritParams ggplot2::ggsave
#' @inheritDotParams ggplot2::ggsave
#'
#' @seealso \code{\link[ggplot2]{ggsave}}
#'
#' @examples
#'
#' \dontrun{
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point() +
#'     ggpreview()
#' }
#'
#' @export
ggpreview <- function(plot = ggplot2::last_plot(), filename = tempfile(fileext = ".png"), ...) {
    plot <- ggplot2::ggsave(plot, filename = filename, ...)
    grid::grid.newpage()
    grid::grid.raster(png::readPNG(filename))
    invisible()
}


#' Rasterize ggplot panel area
#'
#' @description Draws the panel area of a ggplot to a png file, and then re-draws it as raster to the current device. There is also an option to save the result to a file (if \code{filename} is not NULL).
#'
#' @inheritParams ggplot2::ggsave
#'
#' @seealso \code{\link[ggplot2]{ggsave}}
#'
#' @examples
#'
#' \dontrun{
#' p <- ggplot(mtcars, aes(mpg, wt)) +
#'     geom_point()
#' ggrasterize(p)
#'
#' # save to a pdf file
#' ggrasterize(p, "myfile.pdf")
#' }
#'
#' @export
ggrasterize <- function(plot = ggplot2::last_plot(), filename = NULL, device = NULL, path = NULL, scale = 1, width = NA, height = NA, dpi = 300, units = c("in", "cm", "mm"), limitsize = TRUE, ...) {
    dpi <- ggplot2:::parse_dpi(dpi)
    dim <- suppressMessages(ggplot2:::plot_dim(c(NA, NA),
        scale = 1, units = c("in", "cm", "mm"),
        limitsize = FALSE
    ))

    # plot the panel and other elements separately
    gt <- cowplot::as_gtable(plot)

    # Plot the panel to png
    gt_panel <- gt
    gt_panel$grobs <- gt$grobs %>% purrr::modify_at(grep("panel", gt$layout$name, invert = TRUE), ~ grid::nullGrob())

    old_dev <- grDevices::dev.cur()
    temp_fn <- tempfile(fileext = ".png")
    temp_dev <- ggplot2:::plot_dev(NULL, temp_fn, dpi = dpi)
    temp_dev(filename = temp_fn, width = dim[1], height = dim[2], ...)
    grid::grid.draw(gt_panel)
    grDevices::dev.off()

    # Plot other elements as vector graphics
    gt_other <- gt
    gt_other$grobs <- gt$grobs %>% purrr::modify_at(grep("panel", gt$layout$name), ~ grid::nullGrob())
    gt_other$grobs <- gt_other$grobs %>% purrr::modify_at(grep("background", gt$layout$name), ~ grid::nullGrob())


    if (!is.null(filename)) {
        if (!is.null(path)) {
            filename <- file.path(path, filename)
        }

        dim <- ggplot2:::plot_dim(c(width, height),
            scale = scale, units = units,
            limitsize = limitsize
        )
        dev <- ggplot2:::plot_dev(device, filename, dpi = dpi)
        dev(filename = filename, width = dim[1], height = dim[2], ...)
        on.exit(utils::capture.output({
            grDevices::dev.off()
            if (old_dev > 1) grDevices::dev.set(old_dev)
        }))
        grid::grid.raster(png::readPNG(temp_fn))
        grid::grid.draw(gt_other)
    } else {
        utils::capture.output({
            if (old_dev > 1) grDevices::dev.set(old_dev)
        })
        grid::grid.raster(png::readPNG(temp_fn))
        grid::grid.draw(gt_other)
    }
}


#' Scale x axis by log2 (similar to scale_x_log10) 
#' 
#' @inheritDotParams ggplot2::scale_x_log10
#' 
#' @export
scale_x_log2 <- function(...){
    scale_x_continuous(..., trans = scales::log2_trans())
}

#' Scale y axis by log2 (similar to scale_y_log10) 
#' 
#' @inheritDotParams ggplot2::scale_y_log10
#' 
#' @export
scale_y_log2 <- function(...){
    scale_y_continuous(..., trans = scales::log2_trans())
}