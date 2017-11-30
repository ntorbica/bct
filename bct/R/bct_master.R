#' @export
#'
#' @title Master function of the batch correction tool
#' @description Main function wrapper containing the intended workflow of the batch
#'   effect correction algorithm. Calling BCT will initiate the input interface
#'   requiring user interaction. The chosen inputs are then passed within the pipeline
#'   to compute corrections as well as perform the required modifications and formatting.
#'   The final results are then consistently outputted to separate directories created
#'   during the process.
#' @return The output object is a list containing three lists: DATA, containing all of
#'   the data passed within the pipeline; DATA.FORMATTED, containing the tables that
#'   are saved as .csv files; and ARGS, containing the input arguments as well as
#'   arguments added during the execution.

BCT <- function() {

    tkmessageBox(message = "Welcome to the Batch Correction Tool!\nYou will now be prompted for the required inputs.", icon = 'info')

    start.dir <- getwd()


# User Input --------------------------------------------------------------
    bct.msg('Taking Input...')
    arguments <- bct.in.main()

    if(preset == '1'){
      arg.frame <- data.frame(matrix(nrow = length(arguments)))
      rownames(arg.frame) <- names(arguments)
      colnames(arg.frame) <- 'Value'

      for(i in 1:length(arguments)){
        arg.frame[i, ] <- deparse(arguments[[i]])
      }
      write.csv2(arg.frame, 'BCT_preset.csv')
    }

    remove(preset, envir = .GlobalEnv)

# Prepare the data --------------------------------------------------------

    bct.msg('Preparing Data...')
    setwd(arguments$path)

    assign("fname", basename(arguments$data.in), envir = .GlobalEnv)
    data.out <- bct.prep(arguments$data.in)

    # Generate information table if none given

    arguments$p.id <- which(colnames(data.out$ALL) == 'Raw.abundance')
    arguments$gn <- levels(factor(as.matrix(data.out$ALL[1,])))
    arguments$gn <- arguments$gn[-which(arguments$gn == "")]
    start.ord <- as.character(unlist(c(data.out$ALL[2, ])))

    data.out <- bct.infogen(data.out, arguments$bn, arguments$gn, arguments$p.id)
    arguments$select <- c('SCode', 'Batch')


# Format for correction ---------------------------------------------------

    bct.msg('Formatting...')
    data.out <- bct.format(data.in = data.out, long = arguments$long, bn = arguments$bn,
                            QC.n = arguments$qcn, backup.obj = T, p.id = arguments$p.id, seqm = T)
    backup <- data.out[[2]]
    data.out <- data.out[[1]]


# Modification of data ----------------------------------------------------

    # Addition of constant modification
    data.pm <- bct.mod(data.out$P, mod = arguments$mod, log = '', R = F)
    data.out$P.m <- data.pm$Data
    arguments$mod.val <- P.mod <- data.pm[[2]]
    remove(data.pm)


# Correction and Normalization --------------------------------------------
    bct.msg('Applying Corrections...', side = 'top')


    # Correction
    cat('Batch Effect Correction...\n')
    if(arguments$log != ''){
      data.out$P.ml <- bct.mod(data.out$P.m, mod = '', log = arguments$log, R = F)$Data
    }
    data.out$P.c <- switch(arguments$correction,
                           none = 'No Correction',
                           ANCOVA = bct.ancova(peaks = data.out$P.ml,
                                                info = data.out$I,
                                                minQC = arguments$minQC,
                                                condition = arguments$condition,
                                                method = arguments$method,
                                                QC = arguments$QC))

    rownames(data.out$P.c) <- rownames(data.out$P)


    if(arguments$normalization == 'LOESS'){
      # Backtransform data for LOESS
      data.out$P.c <- bct.mod(data.in = list(data.out$P.c, P.mod), mod = arguments$mod, log = arguments$log, R = T)
      rownames(data.out$P.c) <- rownames(data.out$P)

      # Reorder to group-order from initial data.
      tmp <- NULL
      for(i in arguments$p.id:length(start.ord)){
        tmp <- rbind(tmp, data.out$P[which(rownames(data.out$P) == start.ord[i]), ])
      }

      rownames(tmp) <- start.ord[arguments$p.id:length(start.ord)]
      tmp <- tmp[, order(colnames(tmp))]
      data.out$L.in <- bct.bridge(peaks = tmp, info = data.out$I, start.ord, gn = arguments$gn,
                                  backup = backup, csv = F, p.id = arguments$p.id)

      tmp <- NULL
      for(i in 1:length(start.ord)){
        tmp <- rbind(tmp, data.out$P.c[which(rownames(data.out$P.c) == start.ord[i]), ])
      }
      data.out$L.c <- bct.bridge(peaks = tmp, info = data.out$I, start.ord, gn = arguments$gn,
                                 backup = backup, csv = F, p.id = arguments$p.id)
    }

    # Split the data
    if(arguments$split){
      if(arguments$normalization == 'LOESS'){

        data.out$I.s <- list()
        for(i in 1:length(arguments$bn)){
          data.out$I.s[[arguments$bn[i]]] <- data.out$I[which(data.out$I$Batch == arguments$bn[i]), ]
        }

        data.out$L.in.s <- bct.splitbind2(data.in = data.out, info = data.out$info, p.i = "L.in", bn = arguments$bn, start.ord = start.ord,
                                          gn = arguments$gn, bind = F, form = F, p.id = arguments$p.id)

        names(data.out$L.in.s) <- arguments$bn


        # Group naming in first row of SIMCA format for split files
        # Hotfix, because it does not output correctly within splitbind
        tmp <- data.out$L.in.s

        for(i in 1:length(arguments$bn)){
          for(j in arguments$p.id:ncol(tmp[[arguments$bn[i]]][["L.in"]])){
            nm <- tmp[[arguments$bn[i]]][["L.in"]][2, j]
            g <- as.character(data.out$I$SCode[which(rownames(data.out$I) == nm)])
            if(length(g) == 0){
              next()
            }

            if(g == 'ref'){
              g <- 'QC'
              tmp[[arguments$bn[i]]][["L.in"]][1, j] <- g
            } else {
              tmp[[arguments$bn[i]]][["L.in"]][1, j] <- g
            }
          }


          nms <- tmp[[arguments$bn[i]]][["L.in"]][1:2, arguments$p.id:ncol(tmp[[arguments$bn[i]]][["L.in"]])]
          for(k in arguments$gn){
            id <- which(nms[1, ] == k)
            if(length(id) > 1){
              nms[1, id[2:length(id)]] <- ''
            }
          }
          tmp[[arguments$bn[i]]][["L.in"]][1:2, arguments$p.id:ncol(tmp[[arguments$bn[i]]][["L.in"]])] <- nms
          if(NA %in% tmp[[arguments$bn[i]]][["L.in"]][1, ]){
            na.id <- which(is.na(tmp[[arguments$bn[i]]][["L.in"]][1, ]))
            tmp[[arguments$bn[i]]][["L.in"]][1, na.id] <- ''
          }
        }

        data.out$L.in.s <- tmp

        backup.s <- bct.splitbind2(data.in = list(backup), info = data.out$info, p.i = 1, bn = arguments$bn, start.ord = start.ord,
                                  gn = arguments$gn, bind = F, form = F, p.id = arguments$p.id)
        names(backup.s) <- arguments$bn

      } else {

        data.out$P.m.s <- bct.splitbind2(data.out, p.i = "P.m", bn = arguments$bn, start.ord = start.ord, bind = F, form = T, p.id = arguments$p.id)
        backup.s <- bct.splitbind2(list(backup), p.i = 1, bn = arguments$bn, start.ord = start.ord, bind = F, form = F, p.id = arguments$p.id)
        names(backup.s) <- arguments$bn
      }
    }


    # Normalization of input data
    if(!arguments$split){
      cat("Normalization of Complete Data...\n")
      data.out$P.n <- switch(arguments$normalization,
                             none = 'No normalizaiton',
                             LOESS = bct.loess(data.in = data.out$L.in,
                                               info = data.out$I,
                                               bn = arguments$bn,
                                               LOESS_SPAN = arguments$LOESS_SPAN,
                                               LOESS_WARNING = arguments$LOESS_WARNING,
                                               normplot = arguments$plot.n,
                                               pn = 'FULL')$Data)

      negval.n <- which(data.out$P.nc < 0)

      if(!is.null(length(negval.n))){
        f.mod.n <- min(data.out$P.n) - P.mod
        data.out$P.n <- data.out$P.n + abs(f.mod.nc)
      }

    } else {
      cat("Intra-Batch Normalization...\n")
      data.out$P.n.s <- switch(arguments$normalization,
                               none = cat('No normalization'),
                               LOESS = base::lapply(1:length(arguments$bn),
                                                    function(x) {cat("Batch", arguments$bn[x]);
                                                      bct.loess(data.in = data.out[["L.in.s"]][[arguments$bn[x]]][["L.in"]],
                                                      info = data.out$I.s[[arguments$bn[x]]],
                                                      bn = arguments$bn[x],
                                                      LOESS_SPAN = arguments$LOESS_SPAN,
                                                      LOESS_WARNING = arguments$LOESS_WARNING,
                                                      normplot = arguments$plot.n,
                                                      pn = arguments$bn[x])$Data}))

      names(data.out$P.n.s) <- arguments$bn


      # Compensate negative values with one constant
      cat("Compensating Negative Values in Normalized Data...\n\n")

      negval.n <- list()
      for(i in 1:length(data.out$P.n.s)){
        negval.n[[i]] <- which(data.out$P.n.s[[i]] < 0)
      }

      f.mod.n <- list()
      length(f.mod.n) <- length(data.out$P.n.s)

      for(i in 1:length(data.out$P.n.s)){
        if(!is.null(length(negval.n[[i]]))){
        f.mod.n[[i]] <- min(data.out$P.n.s[[i]]) - P.mod
        data.out$P.n.s[[i]] <- data.out$P.n.s[[i]] + abs(f.mod.n[[i]])
        }
      }

      data.out$P.n <- bct.splitbind2(data.out, p.i = "P.n.s", bn = arguments$bn, bind = T, form = T, p.id = arguments$p.id)
    }

    # The LaunchAnalyzer LOESS normalization also performs formatting, producing the necessity
    # of reformatting to apply correction of normalized peaks in the lext step
    if(arguments$normalization == 'LOESS'){
      if(!arguments$split2){

        cat("Normalization of Corrected Data...\n")
        data.out$L.n <- bct.bridge(peaks = data.out$P.n, info = data.out$I, start.ord,
                                   gn = arguments$gn, backup = backup, p.id = arguments$p.id)

        # Normalization of corrected data
        data.out$P.cn <- switch(arguments$normalization,
                                none = cat('No normalization'),
                                LOESS = bct.loess(data.in = data.out$L.c,
                                                  info = data.out$I,
                                                  bn = arguments$bn,
                                                  LOESS_SPAN = arguments$LOESS_SPAN,
                                                  LOESS_WARNING = arguments$LOESS_WARNING,
                                                  normplot = arguments$plot.n,
                                                  pn = 'CorrNorm_Full')$Data)

      } else {

        cat("Intra-Batch Normalization of Corrected Data...\n")
        data.out$L.n.s <- base::lapply(1:length(arguments$bn),
                                       function(x) bct.bridge(peaks = data.out[["P.n.s"]][[arguments$bn[x]]], start.ord,
                                       info = data.out[["I.s"]][[arguments$bn[x]]],
                                       backup = backup.s[[arguments$bn[x]]][[1]], p.id = arguments$p.id))

        data.out$L.n <- bct.bridge(peaks = data.out$P.n, info = data.out$I, start.ord,
                                   gn = arguments$gn, backup = backup, p.id = arguments$p.id)

        data.out$P.cn.s <- switch(arguments$normalization,
                                 none = cat('No normalization'),


                                 LOESS = base::lapply(1:length(arguments$bn),
                                                      function(x) bct.loess(data.in = data.out[["L.in.s"]][[arguments$bn[x]]][["L.in"]],
                                                                            info = data.out$I.s[[arguments$bn[x]]],
                                                                            bn = arguments$bn[x],
                                                                            LOESS_SPAN = arguments$LOESS_SPAN,
                                                                            LOESS_WARNING = arguments$LOESS_WARNING,
                                                                            normplot = arguments$plot.n,
                                                                            pn = arguments$bn[x])$Data))

        names(data.out$P.cn.s) <- arguments$bn
        # The binding of split data does not require conditioning on LOESS normalization

        # Compensate negative values with one constant
        cat("Compensating Negative Values in Normalized Data...\n\n")
        negval.cn <- list()
        for(i in 1:length(data.out$P.cn.s)){
          negval.cn[[i]] <- which(data.out$P.cn.s[[i]] < 0)
        }

        f.mod.cn <- list()
        length(f.mod.cn) <- length(data.out$P.cn.s)

        for(i in 1:length(data.out$P.cn.s)){
          if(!is.null(length(negval.cn[[i]]))){
            f.mod.cn[[i]] <- min(data.out$P.cn.s[[i]]) - P.mod
            data.out$P.cn.s[[i]] <- data.out$P.cn.s[[i]] + abs(f.mod.cn[[i]])
          }
        }

        data.out$P.cn <- bct.splitbind2(data.out, p.i = "P.cn.s", bn = arguments$bn,
                                        bind = T, form = T, p.id = arguments$p.id)
      }
    }



    # Correction of normalized data
    cat("Batch-Effect Correction of Normalized Data...\n")
    data.out$P.nm <- bct.mod(data.in = data.out$P.n, mod = '', log = arguments$log, R = F)[[1]]
    data.out$P.nc <- switch(arguments$correction,
                            none = cat('No correction'),
                            ANCOVA = bct.ancova(peaks = data.out$P.nm,
                                                info = data.out$I,
                                                minQC = arguments$minQC,
                                                condition = arguments$condition,
                                                method = arguments$method,
                                                QC = arguments$QC,
                                                PCA = arguments$PCA,
                                                Duplo = arguments$Duplo))

    rownames(data.out$P.nc) <- rownames(data.out$P)


    # Compensate negative values with one constant
    negval.nc <- which(data.out$P.nc < 0)

    if(!is.null(length(negval.nc))){
      f.mod.nc <- min(data.out$P.nc) - P.mod
      data.out$P.nc <- data.out$P.nc + abs(f.mod.nc)
    }
    bct.msg('Corrections & Normalizations Complete!', side = 'bottom')



# Output ------------------------------------------------------------------

    bct.msg('Creating Output...', side = 'top')

    # Compensate negative values with one constant
    negval.c <- which(data.out$P.c < 0)

    if(!is.null(length(negval.c))){
      f.mod.c <- min(data.out$P.c) - P.mod
      data.out$P.c <- data.out$P.c + abs(f.mod.c)
    }

    negval.cn <- which(data.out$P.cn < 0)

    if(!is.null(length(negval.cn))){
      f.mod.cn <- min(data.out$P.cn) - P.mod
      data.out$P.cn <- data.out$P.cn + abs(f.mod.cn)
    }


    if(arguments$revert){
      cat("Reverting Modifications...\n\n")

      data.out$P.c <- bct.mod(data.in = list(data.out$P.c, P.mod), mod = arguments$mod, log = '', R = T)

      data.out$P.cn <- bct.mod(data.in = list(data.out$P.cn, P.mod), mod = arguments$mod, log = '', R = T)

      data.out$P.n <- bct.mod(data.in = list(data.out$P.n, P.mod), mod = arguments$mod, log = '', R = T)

      data.out$P.nc <- bct.mod(data.in = list(data.out$P.nc, P.mod), mod = arguments$mod, log = '', R = T)

    }

    cat("Reformatting to Input Format...\n\n")
    out <- list()
    if(arguments$split){
      if(arguments$normalization != 'LOESS'){

        for(i in 1:length(arguments$bn)){
          out[[paste("Modified Data", arguments$bn[[i]], sep = ' ')]] <- bct.bridge(peaks = data.out[["P.m.s"]][[arguments$bn[[i]]]][["P.m"]],
                                                                                    info = data.out$I,
                                                                                    start.ord, gn = arguments$gn,
                                                                                    backup = backup.s[[arguments$bn[[i]]]][[1]],
                                                                                    csv = arguments$out1,
                                                                                    n = paste('1_F_B', arguments$bn[[i]], sep = '_'),
                                                                                    p.id = arguments$p.id)
        }
      } else {
        for(i in 1:length(arguments$bn)){
          out[[paste("Modified Data", arguments$bn[[i]], sep = ' ')]] <- bct.bridge(peaks = data.out[["L.in.s"]][[arguments$bn[[i]]]][["L.in"]],
                                                                                    info = data.out$I,
                                                                                    start.ord, gn = arguments$gn,
                                                                                    backup = backup.s[[arguments$bn[[i]]]][[1]],
                                                                                    csv = arguments$out1,
                                                                                    n = paste('1_F_B', arguments$bn[[i]], sep = '_'),
                                                                                    wrap = F, p.id = arguments$p.id)
        }
      }
    } else {
      out[["Modified Data"]] <- bct.bridge(peaks = data.out$P.m,
                                           info = data.out$I,
                                           start.ord, gn = arguments$gn, backup = backup, csv = arguments$out1, n = '1_F', p.id = arguments$p.id)
    }

    out[["Corrected Data"]] <- bct.bridge(data.out$P.c,
                                          data.out$I,
                                          start.ord, gn = arguments$gn, backup = backup, csv = arguments$out2, n = '2_Corrected', p.id = arguments$p.id)

    if(arguments$split){
      if(arguments$normalization != 'LOESS'){
        for(i in 1:length(arguments$bn)){
          out[[paste("Normalized Data", arguments$bn[[i]], sep = ' ')]] <- bct.bridge(peaks = data.out[["P.n.s"]][[arguments$bn[[i]]]],
                                                                                      info = data.out[["I.s"]][[arguments$bn[[i]]]],
                                                                                      start.ord, gn = arguments$gn,
                                                                                      backup = backup.s[[arguments$bn[[i]]]][[1]],
                                                                                      csv = arguments$out1,
                                                                                      n = paste('3_BNorm', arguments$bn[[i]], sep = '_'),
                                                                                      p.id = arguments$p.id)
        }
        out[["Normalized Data"]] <- bct.bridge(peaks = data.out$P.n,
                                               info = data.out$I,
                                               start.ord, gn = arguments$gn, backup = backup, csv = arguments$out1, n = '3_Normalized', p.id = arguments$p.id)
      } else {
        for(i in 1:length(arguments$bn)){
          out[[paste("Normalized Data", arguments$bn[[i]], sep = ' ')]] <- bct.bridge(peaks = data.out[["L.in.s"]][[arguments$bn[[i]]]][["L.in"]],
                                                                                      info = data.out[["I.s"]][[arguments$bn[[i]]]],
                                                                                      start.ord, gn = arguments$gn,
                                                                                      backup = backup.s[[arguments$bn[[i]]]][[1]],
                                                                                      csv = arguments$out1,
                                                                                      n = paste('3_BNorm', arguments$bn[[i]], sep = '_'),
                                                                                      wrap = F, p.id = arguments$p.id)
        }

        out[["Normalized Data"]] <- bct.bridge(peaks = data.out$P.n, info = data.out$I, start.ord, gn = arguments$gn,
                                               backup = backup, csv = arguments$out1, n = '3_Normalized', p.id = arguments$p.id)
      }
    } else {
      out[["Normalized Data"]] <- bct.bridge(peaks = data.out$P.n,  info = data.out$I, start.ord, gn = arguments$gn,
                                             backup = backup, csv = arguments$out1, n = '3_Normalized', p.id = arguments$p.id)
    }
    out[["CorrNorm Data"]] <- bct.bridge(data.out$P.cn, info = data.out$I, start.ord, gn = arguments$gn,
                                         backup = backup, csv = arguments$out4, n = '4_CorrNorm', p.id = arguments$p.id)
    out[["NormCorr Data"]] <- bct.bridge(data.out$P.nc, info = data.out$I, start.ord, gn = arguments$gn,
                                         backup = backup, csv = arguments$out5, n = '5_NormCorr', p.id = arguments$p.id)

    if(start.dir != arguments$path){
      setwd(start.dir)
      bct.msg(paste("Pipeline Complete! Results saved to:\n",
                    arguments$path), side = 'bottom')
    } else {
      bct.msg(paste("Pipeline Complete! Results saved to:\n",
                    arguments$path), side = 'bottom')
    }


    return(list(DATA = data.out, DATA.FORMATTED = out, ARGS = arguments))
}
