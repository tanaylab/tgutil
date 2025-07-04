% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jupyter-utils.R, R/utils.R
\name{sps}
\alias{sps}
\title{Set plot size for jupyter IRKernel}
\usage{
sps(width, height)

sps(width, height)
}
\arguments{
\item{width}{width of the plot}

\item{height}{height of the plot}
}
\value{
previous width and height (invisibly)
}
\description{
Set plot size for jupyter IRKernel

Set plot size for jupyter IRKernel
}
\examples{

# run inside a jupyter notebook
sps(10, 5)
plot(1:10)

sps(10, 5)
plot(1:10)

# save previous size
prev <- sps(5, 5)
plot(1:10)
sps(prev[1], prev[2])

}
