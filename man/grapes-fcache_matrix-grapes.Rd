% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{\%fcache_matrix\%}
\alias{\%fcache_matrix\%}
\title{cache_matrix that force recreating the cached file}
\usage{
call \%fcache_matrix\% fname
}
\arguments{
\item{call}{function call}

\item{fname}{filename}
}
\description{
cache_matrix that force recreating the cached file
}
\examples{
temp_file <- tempfile(fileext = ".tsv")
calc_mtcars_cyl <- function(cyl) {
    message("calculating")
    as.matrix(mtcars[mtcars$cyl == cyl, ])
}

# Call "calc_mtcars_cyl" function
res1 <- calc_mtcars_cyl(6) \%cache_matrix\% temp_file

# Force re-calling the function
res2 <- calc_mtcars_cyl(6) \%fcache_matrix\% temp_file
}
