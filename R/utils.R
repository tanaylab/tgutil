########################################################################
#' @export
annotate_members <- function(x, ...) {
    groups <- list(...)
    group_names <- sapply(substitute(list(...))[-1], deparse)
    if (is.null(names(groups))) {
        names(groups) <- group_names
    } else {
        names(groups) <- ifelse((names(groups) != ""), names(groups), group_names)
    }

    annotations <- character(length(x))
    names(annotations) <- names(x)
    for (gname in names(groups)) {
        mask <- x %in% groups[[gname]]
        annotations[mask] <- paste0(annotations[mask], ",", gname)
    }
    return(substring(annotations, 2))
}


########################################################################
#' Install packages from the Tanay Group repository
#'
#' @export
tg_install <- function(...) {
    install.packages(..., repos = c(getOption("repos"), "https://tanaylab.bitbucket.io/repo"))
}

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
#' @export
cutree_order <- function(tree, k = k, h = h) {
    coupe <- cutree(tree, k = k, h = h)
    coupe.or <- coupe[tree$order]
    coupe.out <- rep(NA, length(coupe))
    j <- 1
    k <- coupe.or[1]
    for (i in 1:length(coupe)) {
        if (coupe.or[i] == k) {
            next
        } else {
            coupe.out[which(coupe == k)] <- j
            j <- j + 1
            k <- coupe.or[i]
        }
    }
    coupe.out[is.na(coupe.out)] <- j
    names(coupe.out) <- names(coupe)
    coupe.out
}


#######################################################################
#' Vectorized mean, similiar to \code{pmin} and \code{pmax}
#'
#' @param ... numeric vectors to average
#' @param na.rm a logical indicating whether missing values should be removed
#'
#' @return a vector with mean of \code{...} arguments
#'
#' @export
pmean <- function(..., na.rm = FALSE) {
    d <- do.call(cbind, list(...))
    res <- rowMeans(d, na.rm = na.rm)
    idx_na <- !rowMeans(!is.na(d))
    res[idx_na] <- NA
    return(res)
}


#######################################################################
#' Vectorized sum, similiar to \code{pmin} and \code{pmax}
#'
#' @param ... numeric vectors to sum
#' @param na.rm a logical indicating whether missing values should be removed
#'
#' @return a vector with sum of \code{...} arguments
#'
#' @export
psum <- function(..., na.rm = FALSE) {
    d <- do.call(cbind, list(...))
    res <- rowSums(d, na.rm = na.rm)
    idx_na <- !rowSums(!is.na(d))
    res[idx_na] <- NA
    return(res)
}



########################################################################
#' Equivalent to python's sys.exit()
#' Stop the script without saving R's status.
#'
#' @param rc Exist status. A NULL is equivalent to 0. Non integer values will be printed to stderr and exit status will be set to 1.
#'
#' @export
exit <- function(...) {
    args <- list(...)
    if (all(sapply(args, is.null))) {
        rc <- 0
    } else if ((length(args) > 1) || !is.numeric(args[[1]])) {
        args <- c(args, list("\n"))
        args$file <- stderr()
        args$sep <- ""
        do.call(cat, args)
        rc <- 1
    } else {
        rc <- as.integer(args[[1]])
    }

    q(save = "no", status = rc)
}


########################################################################
#' Call `main()`
#'
#' Call the function `main()` with the script name and the command line
#' argument. When the `main()` finishes, exit with the return code as
#' the program status.
#'
#' @examples
#' \dontrun{
#' if (sys.nframe() == 0) {
#'     call_main()
#' }
#' }
#'
#' @export
call_main <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    arg0 <- grep("^--file=", args, perl = TRUE, value = TRUE)
    if (length(arg0) > 0) {
        arg0 <- substring(arg0[1], 8)
    } else {
        arg0 <- NA
    }
    exit(main(c(arg0, commandArgs(trailingOnly = TRUE))))
}


#' Negation of the %in% operator
#'
#' @export
#' @noRd
`%!in%` <- Negate(`%in%`)
