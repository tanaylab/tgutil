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


########################################################################
#' Install packages from the Tanay Group repository
#' 
#' @inheritDotParams utils::install.packages
#' @export
tg_install <- function(...) install.packages(..., repos=c(getOption('repos'), 'https://tanaylab.bitbucket.io/repo'))

########################################################################
#' Cut a tree into groups of data in the order of the tree
#' 
#' Cut a tree (result from hclust) into groups of data. Groups are in the order of the tree leafs
#' 
#' @param tree an hclust object 
#' @param k the desired number of groups
#' @param h height where the tree is to be cut 
#' 
#' @section Notes: Taken from http://larmarange.github.io/JLutils/reference/cutree.order.html
#' 
#' @examples 
#' 
#' hc <- hclust(dist(USArrests))
#' memb <- cutree_order(hc, k = 10)
#' 
#' @export
cutree_order <- function(tree, k = k, h = h){
    coupe <- cutree(tree, k = k, h = h)
    coupe.or <- coupe[tree$order]
    coupe.out <- rep(NA, length(coupe))
    j <- 1
    k <- coupe.or[1]
    for (i in 1:length(coupe)) {
        if (coupe.or[i] == k) {
            next
        }
        else {
            coupe.out[which(coupe == k)] <- j
            j <- j + 1
            k <- coupe.or[i]
        }
    }
    coupe.out[is.na(coupe.out)] <- j
    names(coupe.out) <- names(coupe)
    coupe.out
}
