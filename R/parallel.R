########################################################################
#' Return the number of the system's physical CPU cores
#' @export
num_physical_cores <- function()
{
    proc <- readLines('/proc/cpuinfo')
    core_id <- paste0(grep('^physical id', proc, value=TRUE), ' | ',
                      grep('^core id', proc, value=TRUE))
    return(length(unique(core_id)))
}


########################################################################
#' Return the number of the system's logical CPU cores
#' @export
num_logical_cores <- function()
{
    proc <- readLines('/proc/cpuinfo')
    core_id <- grep('^processor', proc)
    return(length(core_id))
}


########################################################################
#' Generate RNG seeds for parallel runs
#' @export
gen_seeds <- function(n)
{
    # Our naive approach would break when the chance of a new random number being
    # unique is low
    stopifnot(n < (.Machine$integer.max / 10))

    seeds <- integer(0)
    while(length(seeds) < n) {
        missing <- n-length(seeds)
        new_seeds <- runif(missing*1.1, min=0, max=.Machine$integer.max)
        seeds <- unique(c(seeds, as.integer(new_seeds)))
    }

    return(seeds[1:n])
}


########################################################################
#' Similar to mclapply, however ensures a reproducible random-number
#' stream for each application of fun()
#' @export
run_wide <- function(x, fun, ..., mc_preschedule=TRUE, mc_cores=num_physical_cores(), .seeds=NULL)
{
    if (is.null(.seeds)) {
        .seeds = gen_seeds(length(x))
    }

    stopifnot(length(.seeds) == length(x))

    run_once <- function(i) {
        set.seed(.seeds[i])
        return(fun(x[i], ...))
    }

    idxs <- 1:length(x)
    names(idxs) <- names(x)

    return(parallel::mclapply(idxs, run_once, mc.preschedule=mc_preschedule, mc.cores=mc_cores))
}
