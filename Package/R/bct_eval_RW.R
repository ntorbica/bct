#' @export
#'
#' @title Basic evaluation of batch effect correction
#' @param peaks Peak data to evaluate. Samples have to be in rows (as produced
#'   by outputted by the BCT call)
#' @param info Information table on peaks, containing information on sample grouping,
#'   injection sequence and batch labels
#' @param arguments Input arguments from BCT produced object.
#' @param plot.pca Logical; should a PCA score plot be produced?
#' @param plot.rep Logical; should a repeatability histogram be produced?
#' @param plot.pdist Logical; should a p-value histogram be produced?
#' @param n Plot title
#' @description Rudimentary evaluation function computing Bhattacharyya distance and
#'   plotting PCA the according score plots, computing repeatability between batches
#'   and plotting histograms, as well as featurewise p-values for sample groups,
#'   injection sequence and batch labels.
#' @return Returns a list object with the values specified in the arguments object.

bct.eval_RW <- function(peaks, info, arguments, plot.pca = F, plot.rep = F, plot.pdist = F,
                        n = '', adj = F, dens = F){

  out <- list()

  if(arguments$PCA == arguments$Duplo){
    layout(matrix(c(1,1,3,1,1,4,2,2,5), 3, 3, byrow = T))
    par(mar = c(4,5,6.5,4))
    pca <- evaluatePCA(peaks, info, plot = plot.pca, perBatch = F)
    out[["Bhattacharyya Distance"]] <- pca
    title(paste("Bhattacharyya Distance: ", round(pca, 3), sep = ''))
    par(mar = c(6,6,5,6))
    dup <- evaluateDuplos(peaks, info, plot = plot.rep, breaks = 20, xlim = c(0,1))
    out[["Repeatabilities"]] <- dup
    title('Feature Repeatability')
    legend(x = 'topright', legend = paste('Mean Rep: ', round(mean(dup, na.rm = T), 3), sep = ''))

  } else {
    layout(matrix(c(1,1,2,1,1,3,1,1,4), 3, 3, byrow = T))
    if(arguments$PCA == '1'){
      par(mar = c(4,5,6.5,4))
      pca <- evaluatePCA(peaks, info, plot = plot.pca, perBatch = F)
      title(paste("Bhattacharyya Distance: ", round(pca, 3), sep = ''))
      out[["Bhattacharyya Distance"]] <- pca
    }
    if(arguments$Duplo == '1'){
      par(mar = c(6,6,5,6))
      dup <- evaluateDuplos(peaks, info, plot = plot.rep, breaks = 20, xlim = c(0,1))
      title('Feature Repeatability')
      legend(x = 'topright', legend = paste('Mean Rep: ', round(mean(dup, na.rm = T), 3), sep = ''))
      out[["Repeatabilities"]] <- dup
    }
  }


  if(arguments$pdist == '1'){
    par(mar = c(6,4,6,4))
    pval.b <- bct.pval(peaks, info, PLOT = plot.pdist, t = '',
                       plotn = 'Batch', adj = adj, dens = dens)
    pval.g <- bct.pval(peaks, info, PLOT = plot.pdist, t = '',
                       plotn = 'Group', adj = adj, dens = dens)
    pval.s <- bct.pval(peaks, info, PLOT = plot.pdist, t = '',
                       plotn = 'SeqNr', adj = adj, dens = dens)
    out[["P-value distributions"]] <- list("Batch" = pval.b,
                                           "Group" = pval.g,
                                           "Order" = pval.s)
  } else {
    pval <- NULL
  }
  par(oma = c(0, 1.5, 0, 0))
  mtext(n, side = 3, outer = T, line = -2.5, adj = c(0, -0.5), cex = 1.2)

  return(out)

}
