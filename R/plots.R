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
#' @param col_names_orient Orientation of the X-axis label
#' 
#' @export
tgplot_heatmap <- function(mtrx, col_names=NULL, row_names=NULL, xlab=NULL, ylab=NULL,
                           plot_top=TRUE, plot_right=TRUE, col_names_orient=c('horizontal', 'vertical', 'slanted'))
{
    col_names_orient <- match.arg(col_names_orient)

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
        ggplot2::theme_bw() +
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
        if (plot_top){
            sec.axis_top <- ggplot2::dup_axis()
        } else {
            sec.axis_top <- ggplot2::waiver()
        }

        ggp <- ggp +
               ggplot2::scale_x_continuous(breaks=1:ncol(mtrx), labels=col_names, expand=c(0,0), sec.axis=sec.axis_top) +
               switch(col_names_orient,
                      'horizontal' = ggplot2::theme(),
                      'vertical'   = ggplot2::theme(axis.text.x.top=element_text(angle=-90, hjust=1, vjust=0.5),
                                                    axis.text.x.bottom=element_text(angle=-90, hjust=0, vjust=0.5)),
                      'slanted'    = ggplot2::theme(axis.text.x.top=element_text(angle=-45, hjust=1, vjust=0.5),
                                                    axis.text.x.bottom=element_text(angle=-45, hjust=0, vjust=1))
               )
    }
    else {
        ggp <- ggp +
               ggplot2::scale_x_continuous(expand=c(0,0)) +
               ggplot2::theme(axis.text.x=ggplot2::element_blank())
    }

    if (!is.null(row_names)) {

        if (plot_right){
            sec.axis_right <- ggplot2::dup_axis()
        } else {
            sec.axis_right <- ggplot2::waiver()
        }

      
        ggp <- ggp +
               ggplot2::scale_y_continuous(breaks=1:nrow(mtrx), labels=row_names, expand=c(0,0), sec.axis=sec.axis_right)
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
tgplot_add_axis_annotation <- function(heatmap, annotation, position='bottom', size=0.02)
{
    position = char.expand(position, c('top', 'bottom', 'left', 'right'))
    if (!is(annotation, 'gg')) {
        if (position %in% c('top', 'bottom')) {
            ggdata <- tibble::tibble(x=1:length(annotation), y=1)
        }
        else {
            ggdata <- tibble::tibble(x=1, y=1:length(annotation))
        }

        annotation <- ggplot2::ggplot(ggdata, aes(x=x, y=y)) +
                      ggplot2::geom_raster(fill=annotation) +
                      ggplot2::coord_cartesian(expand=FALSE) +
                      ggplot2::theme_void() +
                      ggplot2::theme(panel.border=element_rect(size=0.2, color='black', fill=NA))
    }

    if (position %in% c('top', 'bottom')) {
        heatmap <- cowplot::insert_xaxis_grob(heatmap, annotation, grid::unit(size, 'null'), position=position)
    }
    else {
        heatmap <- cowplot::insert_yaxis_grob(heatmap, annotation, grid::unit(size, 'null'), position=position)
    }

    return(heatmap)
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
