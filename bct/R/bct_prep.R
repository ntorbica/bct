#' @export
#'
#' @title Data preparation
#' @param data.in Input data specification of table to prepare. Can be a list containing
#'   peak data and information table (bct.infogen), or a character object containing the
#'   filename of the input data.
#' @description Prepares input data (specified in either list with table objects or
#'   filename) for passing to bct.format. Reads .csv files if input is a filename.
#' @return Returns a list object containing either two or one elements, depending on the
#'   input object length.

bct.prep <- function(data.in) {

    if (is.list(data.in)) {
        ld <- length(data.in)

        if (ld > 2) {
            warning("More than two list elements identified. The pipeline may fail if the first two elements are not the peaks and information table.\n")
            if (object.size(data.in[[1]]) > object.size(data.in[[2]])) {
                data.in <- list(P = bct.tabwrap(data.in[[1]], c("numeric", "data.frame")), I = bct.tabwrap(data.in[[2]], c("factor", "data.frame")))
            } else {
                data.in <- list(P = bct.tabwrap(data.in[[2]], c("numeric", "data.frame")), I = bct.tabwrap(data.in[[1]], c("factor", "data.frame")))
            }
        }

        if (ld == 2) {
            if (object.size(data.in[[1]]) > object.size(data.in[[2]])) {
                data.in <- list(P = bct.tabwrap(data.in[[1]], c("numeric", "data.frame")), I = bct.tabwrap(data.in[[2]], c("factor", "data.frame")))
            } else {
                data.in <- list(P = bct.tabwrap(data.in[[2]], c("numeric", "data.frame")), I = bct.tabwrap(data.in[[1]], c("factor", "data.frame")))
            }
        } else {
            data.in <- bct.tabwrap(list(ALL = data.in[[1]], c("factor", "data.frame")))
        }
    }
    if (is.matrix(data.in) | is.data.frame(data.in)) {
        data.in <- list(ALL = bct.tabwrap(data.in, c("factor", "data.frame")))
    }
    if (is.character(data.in)) {
        ld <- length(data.in)

        if (ld == 2) {
            data.load <- list()
            data.load[[1]] <- bct.tabwrap(data.in[[1]], c('factor', 'data.frame'))
            data.load[[2]] <- bct.tabwrap(data.in[[2]], c('factor', 'data.frame'))

            if (object.size(data.load[1]) > object.size(data.load[2])) {
                data.in <- list(P = bct.tabwrap(data.in[1], c("character", "data.frame")), I = bct.tabwrap(data.in[2], c("factor", "data.frame")))
            } else {
                data.in <- list(P = bct.tabwrap(data.in[2], c("character", "data.frame")), I = bct.tabwrap(data.in[1], c("factor", "data.frame")))
            }
            remove(data.load)

        } else {
            data.in <- list(ALL = bct.tabwrap(data.in, c("factor", "data.frame")))
        }
    }
    return(data.in)
}

