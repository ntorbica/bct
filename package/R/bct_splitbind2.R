#' @export
#'
#' @title Split or Combine LC-MS Datasets
#' @param data.in Input data to split or bind. Has to be a list containing the peak table
#'   to split or the peak tables to bind.
#' @param info Information table to the peak data intended to split or bind.
#' @param p.i Index or name of peak table within input list
#' @param bn Batch naming as found in information table
#' @param bind Logical; should the data be split (bind = F) or bound together (bind = T)
#' @param form Logical; has the data been formatted by bct.format (form = T) before
#'   inputting to bct.splitbind2?
#' @param p.id Required if form = F; column index of first data column (usually named
#'   'Raw abundance').
#' @param gn Required if form = F; sample groups as found in information table, used
#'   to correctly label the samples by grouping.
#' @description Function in order to prepare peak data tables for intra-batch
#'   normalization, as well as returning them to their original composition.
#' @return Produces a list containing data table elements split by batch labels if
#'   bind = F. Produces a table object with combined tables if bind = T.

bct.splitbind2 <- function(data.in, info, p.i, bn, start.ord, bind, form, p.id = NULL, gn = NULL){

  if(form){
    if(is.list(data.in) & !is.data.frame(data.in)){
      if(!bind){

        # create splitting objects
        b.id <- list()

        data.split <- list()
        length(data.split) <- length(bn)

        for(i in 1:length(bn)){

          b.id[[i]] <- which(info[["I"]][["Batch"]] == bn[i])

          data.split[[i]][[p.i]] <- bct.tabwrap(data.in[[p.i]][b.id[[i]], ], c("numeric", "data.frame"))
          b.ord <- data.split[[i]][2, p.id:ncol(data.in[[p.i]])]

          data.split[[i]][["I"]] <- bct.tabwrap(data.in[["I"]][b.id[[i]], ], c("factor", "data.frame"))
          rownames(data.split[[i]][[p.i]]) <- rownames(data.in[[p.i]])[b.id[[i]]]
          rownames(data.split[[i]][["I"]]) <- rownames(data.in[["I"]])[b.id[[i]]]
        }

        names(data.split) <- bn

        return(data.split)

      } else {

        # Bind split data
        if(is.list(data.in) & !is.data.frame(data.in)){

            data.bound <- NULL

            for(i in 1:length(data.in[[p.i]])){
              data.bound <- rbind(data.bound, data.in[[p.i]][[i]])
            }

            pn <- rownames(data.bound)
            data.bound <- bct.tabwrap(data.bound, c("numeric", "matrix"))
            rownames(data.bound) <- pn

            return(data.bound)

        } else {
          stop("Wrong input object; need list with lists containing the split data.")
        }
      }
    }


  } else {

    # Split LaunchAnalyzer format
    if(is.list(data.in) & !is.data.frame(data.in)){
      if(!bind){

        # create splitting objects
        b.id <- list()
        data.split <- list()
        length(data.split) <- length(bn)

        for(i in 1:length(bn)){
          b.id[[i]] <- grep(bn[[i]], data.in[[p.i]][2,])

          tmp <- data.in[[p.i]][, b.id[[i]]]

          data.split[[i]][[p.i]] <- bct.tabwrap(tmp, c("character", "matrix"))


          for(j in 1:ncol(data.split[[i]][[p.i]])){
            nm <- data.split[[i]][[p.i]][2, j]
            g <- as.character(info[nm, "SCode"])

            if(length(g) > 0){
              if(g == 'ref'){
                g <- 'QC'
                data.split[[i]][[p.i]][1, j] <- g
              } else {
                data.split[[i]][[p.i]][1, j] <- g
              }
            }
          }

          nms <- data.split[[i]][[p.i]][1:2, ]
          for(k in gn){
            id <- which(nms[1, ] == k)
            if(length(id) > 1){
              nms[1, id[2:length(id)]] <- ''
            }
          }

          data.split[[i]][[p.i]][1:2, ] <- nms



          data.split[[i]][[p.i]] <- cbind(data.in[[p.i]][, 1:p.id - 1], data.split[[i]][[p.i]])
          colnames(data.split[[i]][[p.i]])[p.id] <- "Raw.abundance"
        }

        names(data.split) <- bn

        return(data.split)

      } else {

        ## This part is rather obsolete. There is no function except for BCT_bridge that produces
        ## the required format. Never say never, though.

        # Bind LaunchAnalyzer format
        tmp <- data.in[[p.i]][[bn[1]]][[p.i]][, 1:p.id]

        data.bound <- NULL

        for(i in 1:length(data.in[[p.i]])){
          data.bound <- cbind(data.bound, data.in[[p.i]][[i]][[p.i]][, -(1:p.id)])
        }

        data.bound <- cbind(tmp, data.bound)

        pn <- gsub('V[0-9]+', '', colnames(data.bound))
        data.bound <- bct.tabwrap(data.bound, c("character", "matrix"))
        colnames(data.bound) <- pn

        # remove 'Raw.abundance' from column names except first
        if(length(which(colnames(data.bound) == 'Raw.abundance')) > 1){
          remn <- which(colnames(data.bound) == 'Raw.abundance')
          colnames(data.bound)[remn[which(remn > p.id + 1)]] <- ''
        }

        return(data.bound)
      }
    }
  }

}
