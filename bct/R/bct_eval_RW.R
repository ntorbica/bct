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

bct.eval_RW <- function(peaks, info, arguments, plot.pca = F, plot.rep = F, plot.pdist = F, n = ''){

  if(arguments$PCA == '1'){
    pca <- evaluatePCA(peaks, info, plot = plot.pca, main = n, perBatch = F)
  } else {
    pca <- NULL
  }

  if(arguments$Duplo == '1'){
    dup <- evaluateDuplos(peaks, info, plot = plot.rep)
  } else {
    dup <- NULL
  }

  if(arguments$pdist == '1'){
    pval <- bct.pval(peaks, info, PLOT = plot.pdist, t = '', plotn = 'Batch')
  } else {
    pval <- NULL
  }

  return(list('Bhatt. Dist PC1 vs PC2' = pca, "Repeatabilities" = dup, "P-value distributions" = pval))

}
