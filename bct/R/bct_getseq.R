#' @export
#'
#' @title Extract injection sequence from sample names
#' @param dat Sample name vector
#' @description Function that takes a vector containing sample names and extracts the
#'   injection sequence, returns a character vector.
#' @return Returns a numeric vector with the matched injection sequence.

bct.getseq <- function(dat) {

    id <- c(rep(0, length(dat)))
    for (i in 1:length(id)) {
        if (grepl("^[0-9]{8}_", dat[i]) == T) {
            id[i] <- as.numeric(gsub("_", "", regmatches(dat[i], regexpr("_[0-9]{3}", dat[i]))))
        }
    }

    return(id)
}
