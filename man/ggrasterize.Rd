% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{ggrasterize}
\alias{ggrasterize}
\title{Rasterize ggplot panel area}
\usage{
ggrasterize(
  plot = ggplot2::last_plot(),
  filename = NULL,
  device = NULL,
  path = NULL,
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300,
  units = c("in", "cm", "mm"),
  limitsize = TRUE,
  ...
)
}
\arguments{
\item{plot}{Plot to save, defaults to last plot displayed.}

\item{filename}{File name to create on disk.}

\item{device}{Device to use. Can either be a device function
(e.g. \link{png}), or one of "eps", "ps", "tex" (pictex),
"pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only). If
\code{NULL} (default), the device is guessed based on the \code{filename} extension.}

\item{path}{Path of the directory to save plot to: \code{path} and \code{filename}
are combined to create the fully qualified file name. Defaults to the
working directory.}

\item{scale}{Multiplicative scaling factor.}

\item{width, height}{Plot size in units expressed by the \code{units} argument.
If not supplied, uses the size of the current graphics device.}

\item{dpi}{Plot resolution. Also accepts a string input: "retina" (320),
"print" (300), or "screen" (72). Applies only to raster output types.}

\item{units}{One of the following units in which the \code{width} and \code{height}
arguments are expressed: \code{"in"}, \code{"cm"}, \code{"mm"} or \code{"px"}.}

\item{limitsize}{When \code{TRUE} (the default), \code{ggsave()} will not
save images larger than 50x50 inches, to prevent the common error of
specifying dimensions in pixels.}

\item{...}{Other arguments passed on to the graphics device function,
as specified by \code{device}.}
}
\description{
Draws the panel area of a ggplot to a png file, and then re-draws it as raster to the current device. There is also an option to save the result to a file (if \code{filename} is not NULL).
}
\examples{
\dontrun{
library(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt)) +
    geom_point()
ggrasterize(p)

# save to a pdf file
ggrasterize(p, "myfile.pdf")
}

}
\seealso{
\code{\link[ggplot2]{ggsave}}
}
