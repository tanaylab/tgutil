#' Set plot size for jupyter IRKernel
#'
#' @param width width of the plot
#' @param height height of the plot
#'
#' @examples
#'
#' # run inside a jupyter notebook
#' set_plot_size(10, 5)
#' plot(1:10)
#'
#' @export
set_plot_size <- function(width, height) {
    options(repr.plot.width = width, repr.plot.height = height)
}

#' Alias for set_plot_size
#'
#' @inheritParams set_plot_size
#'
#' @examples
#'
#' # run inside a jupyter notebook
#' sps(10, 5)
#' plot(1:10)
#'
#' @export
sps <- set_plot_size


#' Source a jupyter notebook
#'
#' @description convert a jupyter notebook to R using jupytext and source it. Jupytext must be installed and available in the path
#'
#' @param file path to the jupyter notebook
#' @param outdir directory to store the converted R file. By default the file would be created in the same directory as the jupyter notebook
#' @param jupytext_path path to jupytext executable
#' @param source_file if FALSE, and .R file is created but not sourced
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # notebook.R is created in the same directory as notebook.ipynb
#' source_jupyter("notebook.ipynb")
#'
#' # notebook.R is created in a temp directory
#' source_jupyter("notebook.ipynb", outdir = tempdir())
#' }
#'
#' @export
source_jupyter <- function(file, outdir = NULL, jupytext_path = "jupytext", source_file = TRUE) {
    file <- normalizePath(file)
    file_name <- basename(file)
    if (!is.null(outdir)) {
        # copy file to outdir
        file_out <- file.path(outdir, file_name)
        file.copy(file, file_out, overwrite = TRUE)
    } else {
        file_out <- file
    }


    cmd <- paste0("jupytext --to R ", file_out)
    system(cmd)

    r_file <- gsub(".ipynb$", ".R", file_out)

    if (!file.exists(r_file)) {
        stop("Could not find R file: ", r_file)
    }

    if (source_file) {
        source(r_file)
    }
}
