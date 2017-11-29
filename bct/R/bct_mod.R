#' @export
#'
#' @title Modification of peak data
#' @param data.in Peak data table formatted by bct.format.
#' @param mod Modification option. Modification means addition of one or multiple values. Can be "" for no modification, 1 for addition of limit of detection > 0, 2 for addition of the data mean, or 3 for the feature-wise addition of feature means.
#' @param log Logarithm transformation option. Can be "" for no log transformation, "e"
#'   for ln transformation, 2 for log2 transformation, or 10 for log10 transformation.
#' @param R Logical; should modifications be applied (R == F) or reverted?
#' @description Applies modification through addition of values as well as logarithm
#'   transformation of peak intensity data. To avoid negative infinity values (log of 0),
#'   the addition is always performed before log transformation. When reverting, the
#'   logarithm is reverted before subtracting the modification value.
#' @return Returns a numeric matrix with modified peak intensity values.

bct.mod <- function(data.in, mod = c('', 1, 2, 3), log = c("", "e", 2, 10), R = F) {

    out  <- NULL

    if (R) {
        if (!is.list(data.in)) {
            stop("Wrong input format for removal\n
           List with data.in and modification value required.\n")
        }

        rem <- data.in[[2]]
        data.in <- data.in[[1]]

        if (log == "") {cat("No log transform applied.\n")}

        if (log == "e") {data.in <- exp(data.in); cat("ln reversed.\n")}

        if (log == 2) {for (i in 1:length(data.in)) {data.in[i] <- 2^data.in[i]}; cat("log2 reversed.\n")}

        if (log == 10) {for (i in 1:length(data.in)) {data.in[i] <- 10^data.in[i]}; cat("log10 reversed.\n")}


        if (mod == "") {cat("no modification applied.\n")}

        if (mod == 1 | mod == 2) {for (i in 1:length(data.in)) {data.in[i] <- data.in[i] - rem}; cat("Subtracted Constant (",rem ,")\n", sep = '')}

        if (mod == 3) {
            for (i in 1:ncol(data.in)) {
                for (j in 1:nrow(data.in)) {
                  data.in[j, i] <- data.in[j, i] - rem[i]
                }
            }
            cat("Subtracted feature mean per feature.\n")
        }

        cat("\n")
        return(data.in)

    } else {

        if (mod == "") {cat("No modification applied.\n")}

        if (mod == 1) {

            data.in.min <- min(data.in[which(data.in != 0)])

            for (i in 1:length(data.in)) {
                data.in[i] <- data.in[i] + data.in.min
            }
            cat("Added limit of detection > 0 (", data.in.min, ").\n", sep = '')
            out <- list(Data = data.in, "Minimum > 0" = data.in.min)
        }



        if (mod == 2) {
            data.in.mean <- mean(data.in)

            for (i in 1:length(data.in)) {
                data.in[i] <- data.in[i] + data.in.mean
            }

            cat("Added data mean (", data.in.mean, ").\n", sep = '')
            out <- list(Data = data.in, Mean = data.in.mean)
        }

        if (mod == 3) {
            f.mean <- c()
            for (i in 1:ncol(data.in)) {
                f.mean[i] <- mean(data.in[, i])

                for (j in 1:nrow(data.in)) {
                  data.in[j, i] <- data.in[j, i] + f.mean[i]
                }
            }
            cat("Added feature mean per feature.\n", sep = '')
            out <- list(Data = data.in, `Feature Means` = f.mean)
        }


        if (log == "") {cat("no log transform applied.\n")}

        if (log == "e") {
            cat("ln applied.\n")
            if (length(which(data.in == 0) > 0)) {
                warning("Taking log of zero")
            }
            out[["Data"]] <- log(data.in)
        }

        if (log == 2) {
            cat("log2 applied.\n")
            if (length(which(data.in == 0) > 0)) {
                warning("Taking log of zero")
            }
            out[["Data"]] <- log2(data.in)
        }

        if (log == 10) {
          cat("log10 applied.\n")
            if (length(which(data.in == 0) > 0)) {
                warning("Taking log of zero")
            }
            out[["Data"]] <- log10(data.in)
        }


    }
    cat("\n")
    out[["Data"]] <- out[["Data"]][,order(colnames(out[["Data"]]))]
    return(out)
}
