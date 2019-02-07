########################################################################
#' @export
tgplot_heatmap <- function(mtrx, col_names=NULL, row_names=NULL, xlab=NULL, ylab=NULL)
{
    if (is.null(col_names)) {
        col_names <- colnames(mtrx)
    }
    else if ((length(col_names)==1) && !col_names) {
        col_names <- NULL
    }
    if (!is.null(col_names) && (length(col_names) != ncol(mtrx))) {
        stop("Length of col_names must equal the number of columns")
    }

    if (is.null(row_names)) {
        row_names <- rownames(mtrx)
    }
    else if ((length(row_names)==1) && !row_names) {
        row_names <- NULL
    }
    if (!is.null(row_names) && (length(row_names) != nrow(mtrx))) {
        stop("Length of row_names must equal the number of rows")
    }

    colnames(mtrx) <- NULL
    rownames(mtrx) <- NULL
    ggp <- ggplot2::ggplot(gather_matrix(mtrx), ggplot2::aes(x=x, y=y, fill=val)) +
        ggplot2::geom_raster() +
        ggplot2::theme(axis.ticks=ggplot2::element_blank())

    if (!is.null(xlab)) {
        ggp <- ggp + ggplot2::xlab(xlab)
    }
    else {
        ggp <- ggp + ggplot2::theme(axis.title.x=ggplot2::element_blank())
    }
    if (!is.null(ylab)) {
        ggp <- ggp + ggplot2::ylab(ylab)
    }
    else {
        ggp <- ggp + ggplot2::theme(axis.title.y=ggplot2::element_blank())
    }

    if (!is.null(col_names)) {
        ggp <- ggp +
               ggplot2::scale_x_continuous(breaks=1:ncol(mtrx), labels=col_names, expand=c(0,0), sec.axis=ggplot2::dup_axis())
    }
    else {
        ggp <- ggp +
               ggplot2::scale_x_continuous(expand=c(0,0)) +
               ggplot2::theme(axis.text.x=ggplot2::element_blank())
    }

    if (!is.null(row_names)) {
        ggp <- ggp +
               ggplot2::scale_y_continuous(breaks=1:nrow(mtrx), labels=row_names, expand=c(0,0), sec.axis=ggplot2::dup_axis())
    }
    else {
        ggp <- ggp +
               ggplot2::scale_y_continuous(expand=c(0,0)) +
               ggplot2::theme(axis.text.y=ggplot2::element_blank())
    }


    return(ggp)
}


########################################################################
#' @export
scale_fill_gradientn_abs <- function(..., colors, values, abs_range)
{
    if (length(colors) != length(values)) {
        stop("colors and values must have equal lengths")
    }

    abs_range <- range(abs_range, na.rm=TRUE)
    low  <- abs_range[1]
    high <- abs_range[2]

    colortab <- tibble(value=values, color=colors)
    inter <- colortab %>%
             dplyr::filter(value>low, value<high) %>%
             dplyr::mutate(value=scales::rescale(value, to=c(0,1), from=c(low, high)))
    low_interp <- dplyr::bind_rows(colortab %>% dplyr::filter(value<=low) %>% tail(1),
                                   colortab %>% dplyr::filter(value>low) %>% head(1))
    high_interp <- dplyr::bind_rows(colortab %>% dplyr::filter(value<high) %>% tail(1),
                                    colortab %>% dplyr::filter(value>=high) %>% head(1))
    low_color <- colorRamp(low_interp$color, space='Lab')(scales::rescale(low, to=c(0,1), from=low_interp$value))
    high_color <- colorRamp(high_interp$color, space='Lab')(scales::rescale(high, to=c(0,1), from=high_interp$value))

    values <- c(0, inter$value, 1)
    colors <- c(rgb(low_color/255), inter$color, rgb(high_color/255))

    return(ggplot2::scale_fill_gradientn(..., colors=colors, values=values))
}
