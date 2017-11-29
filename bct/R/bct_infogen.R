#' @export
#'
#' @title Generate information table
#' @param data.in Peak data table within a list.
#' @param bn Character vector containing batch names as found within sample names.
#' @param gn Character vector containing group names as found within sample names
#'   (determined in BCT call).
#' @param p.id Column index of first sample data column (usually named 'Raw abundance').
#' @description Function to produce the required information table from original input
#'   table.
#' @return Returns a list containing the origianl input along with an information table
#'   element.

bct.infogen <- function(data.in, bn, gn, p.id) {

    if (is.list(data.in) & !is.data.frame(data.in)) {
        data.in <- data.in[[1]]
    }

    data.in <- bct.tabwrap(data.in, c("character", "data.frame"))
    gn <- as.character(gn)


    # get group name indices
    id.g <- data.frame(ncol = length(gn), nrow = 2)
    for (i in 1:length(gn)) {
        id.g[1, i] <- as.numeric(which(data.in[1, p.id:ncol(data.in)] == gn[i]))
        id.g[2, i] <- gn[i]
    }

    id.g <- id.g[, order(as.numeric(id.g[1, ]))]

    # create grouping vector according to id.g
    s.id <- as.vector(t(data.in[1, p.id:ncol(data.in)]))

    for (i in 1:length(gn)) {
        if (i < length(gn)) {
            s.id[as.numeric(id.g[1, i]):as.numeric(id.g[1, (i + 1)])] <- as.character(id.g[2, i])
        } else {
            s.id[as.numeric(id.g[1, i]):length(s.id)] <- as.character(id.g[2, i])
        }
    }

    s.n <- as.vector(t(data.in[2, p.id:ncol(data.in)]))
    b.id <- list()
    for (i in 1:length(bn)) {
        b.id[[i]] <- grep(bn[i], s.n)
    }

    B <- s.n
    for (i in 1:length(b.id)) {
        B[b.id[[i]]] <- bn[i]
    }

    info <- data.frame('Names' = s.n, 'SCode' = s.id, 'Batch' = B)
    info <- bct.tabwrap(info, c('factor', 'data.frame'))

    return(list(P = data.in, I = info))
}
