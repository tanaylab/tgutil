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
#' @export
tg_install <- function(...) {
    install.packages(..., repos=c(getOption('repos'), 'https://tanaylab.bitbucket.io/repo'))
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


########################################################################
#' Equivalent to python's sys.exit()
#' Stop the script without saving R's status.
#'
#' @param rc Exist status. A NULL is equivalent to 0. Non integer values will be printed to stderr and exit status will be set to 1.
#'
#' @export
exit <- function(...)
{
    args <- list(...)
    if (all(sapply(args, is.null))) {
        rc <- 0
    }
    else if ((length(args) > 1) || !is.numeric(args[[1]])) {
        args = c(args, list('\n'))
        args$file = stderr()
        args$sep = ''
        do.call(cat, args)
        rc <- 1
    }
    else {
        rc <- as.integer(args[[1]])
    }

    q(save='no', status=rc)
}

########################################################################
#' Wrap a function that results a data frame to cache results in a csv file
#' 
#' @param func function to wrap
#' @param fn name of csv file
#' @param row.names Should row names be cached as well
#' 
#' @return a wrapped function
#' 
#' @examples
#' 
#'  temp_file <- tempfile(fileext = ".csv")
#' 
#'  calc_mtcars_cyl <- function(cyl) {
#'       message("calculating")
#'       mtcars[mtcars$cyl == cyl, ]
#'  }
#' 
#' get_mtcars_cyl <- get_csv(calc_mtcars_cyl, temp_file)
#'
#' # Call "calc_mtcars_cyl" function
#' get_mtcars_cyl(6)
#'
#' # Load cached file
#' get_mtcars_cyl(6)
#'
#' # Force re-calculating
#' get_mtcars_cyl(6, recalc=TRUE)
#' 
#' @export
get_csv <- function(func, fn, row.names = FALSE, ...) {
    func <- purrr::as_mapper(func)
    if (row.names){
        res_func <- function(..., recalc = FALSE) {
            if (!file.exists(fn) || recalc) {
                res <- func(...)        
                tgutil::fwrite(res, fn, row.names = TRUE)            
            } else {
                res <- tgutil::fread_rownames(fn)
                res <- tibble::column_to_rownames(res, "rowname")
            }
            return(res)
        }
    } else {
        res_func <- function(..., recalc = FALSE) {
            if (!file.exists(fn) || recalc) {
                res <- func(...)        
                tgutil::fwrite(res, fn, row.names = FALSE)            
            } else {
                res <- tgutil::fread(fn)
            }
            return(res)
        }        
    }
    return(res_func)
}


########################################################################
#' Wrap a function to cache results in an rds file
#' 
#' @param func function to wrap
#' @param fn name of rds file
#' 
#' @return a wrapped function
#' 
#' @examples
#' 
#'  temp_file <- tempfile(fileext = ".rds")
#' 
#'  calc_mtcars_cyl <- function(cyl) {#'       
#'       mtcars[mtcars$cyl == cyl, ]
#'  }
#' 
#' get_mtcars_cyl <- get_rds(calc_mtcars_cyl, temp_file)
#'
#' # Call "calc_mtcars_cyl" function
#' get_mtcars_cyl(6)
#'
#' # Load cached file
#' get_mtcars_cyl(6)
#'
#' # Force re-calculating
#' get_mtcars_cyl(6, recalc=TRUE)
#' 
#' @export
get_rds <- function(func, fn) {
    func <- purrr::as_mapper(func)
    res_func <- function(..., recalc = FALSE) {
            if (!file.exists(fn) || recalc) {
                res <- func(...)        
                readr::write_rds(res, fn)            
            } else {
                res <- readr::read_rds(fn)                
            }
            return(res)
    } 
    return(res_func)
}

