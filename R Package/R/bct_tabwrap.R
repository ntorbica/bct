#' @export
#'
#' @title Simplified class conversion funciton
#' @param tab.in Input table object to be converted.
#' @param type.out Output class of table object. Is a character vector containing
#'   the table element class as the first and and the table class as second element
#'   (i.e. c("character", "data.frame")). First element can be "numeric", "character",
#'   or "factor". Second element can be "matrix", "data.frame", or "data.table". Uses
#'   the data.table package.
#' @description Function mainly aiming at simple class conversion of table objects passed
#'   within the pipeline.
#' @return Returns the input table with specified classes.

bct.tabwrap <- function(tab.in, type.out) {

    if (class(tab.in) != "data.frame" & class(tab.in) != "character" & class(tab.in) != "matrix" & class(tab.in) != "data.table") {
        stop("bct.tabwrap: Input has to be a data.frame, matrix or filename")
    }

    if (is.character(tab.in)) {

    }

    L <- length(tab.in)
    l <- length(type.out)
    tab.out <- NULL

    if (l == 1L) {
        if (L == 1L) {
            tab.in <- fread(tab.in, stringsAsFactors = F, data.table = F)
        } else {

            if (type.out == "character") {
                tab.out <- apply(tab.in, 2, as.character)
            }
            if (type.out == "numeric") {
                tab.out <- apply(tab.in, 2, as.numeric)
            }
            if (type.out == "factor") {
                tab.out <- apply(tab.in, 2, as.factor)
            }
        }

    } else {

        if (L == 1L) {
            tab.in <- fread(tab.in, stringsAsFactors = F, data.table = F)
        }

        if (type.out[2] == "matrix") {
            if (type.out[1] == "character") {
                tab.out <- as.matrix(tab.in)
                tab.out <- apply(tab.in, 2, as.character)
            }
            if (type.out[1] == "numeric") {
                tab.out <- as.matrix(tab.in)
                tab.out <- apply(tab.in, 2, as.numeric)
            }
            if (type.out[1] == "factor") {
                tab.out <- as.matrix(tab.in)
                tab.in <- apply(tab.in, 2, as.factor)
            }
        }
        if (type.out[2] == "data.frame") {
            if (type.out[1] == "character") {
                tab.out <- data.frame(apply(tab.in, 2, as.character), stringsAsFactors = F)
            }
            if (type.out[1] == "numeric") {
                tab.out <- data.frame(apply(tab.in, 2, as.numeric), stringsAsFactors = F)
            }
            if (type.out[1] == "factor") {
                tab.out <- data.frame(apply(tab.in, 2, as.character), stringsAsFactors = T)
            }
        }
        if (type.out[2] == "data.table") {
            if (type.out[1] == "character") {
                tab.out <- data.table(apply(tab.in, 2, as.character))
            }
            if (type.out[1] == "numeric") {
                tab.out <- data.table(apply(tab.in, 2, as.numeric))
            }
            if (type.out[1] == "factor") {
                tab.out <- data.table(apply(tab.in, 2, as.factor))
            }
        }
    }

    return(tab.out)
}
