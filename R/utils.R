########################################################################
#' @export
annotate_members <- function(x, ...)
{
    groups <- list(...)
	group_names <- sapply(substitute(list(...))[-1], deparse)
	if (is.null(names(groups))) {
		names(groups) <- group_names
	} else {
        names(groups) <- ifelse((names(groups) != ''), names(groups), group_names)
    }
    
    annotations <- character(length(x))
    names(annotations) <- names(x)
    for (gname in names(groups)) {
        mask <- x %in% groups[[gname]]
        annotations[mask] <- paste0(annotations[mask], ',', gname)
    }
    return(substring(annotations, 2))
}


#' Install packages from the Tanay Group repository
#' 
#' @inheritDotParams install.packages
#' @export
tg_install <- function(...) install.packages(..., repos=c(getOption('repos'), 'https://tanaylab.bitbucket.io/repo'))

