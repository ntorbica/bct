#' @export
#'
#' @title Master graphical user interface input function
#' @description Calls graphical user interface functions to ease interaction with
#'   the pipeline. First calls bct.in.methods, registering the correction and
#'   normalization methods as well as the choice of regression samples (QCs or
#'   study samples). Second calls bct.in.args to take all of the required inputs
#'   for correct pipeline execution.
#' @return Returns a list object containing the user choices, intended for passing
#'   arguments within the pipeline.

bct.in.main <- function(gui){

  AGMT <- list()
# Input methods -----------------------------------------------------------
##############
  if(gui){
    bct.in.methods()

    if(cancelled){
      if(cancelled){
        sq <- function(){
          opt <- options(show.error.messages=FALSE)
          on.exit(options(opt))
          stop("Pipeline stopped.")
        }
        sq()
      }
    }

    if(QC == 1){
      QC <- T
    } else {
      QC <- F
    }

    AGMT$QC <- QC

  # Input Args --------------------------------------------------------------
  ##############
    bct.in.args()

    if(cancelled){
      sq <- function(){
        opt <- options(show.error.messages=FALSE)
        on.exit(options(opt))
        stop("Pipeline stopped.")
      }
      sq()
    }

    AGMT$data.in <- data.in
    AGMT$path <- fpath
    AGMT$correction <- correction
    AGMT$condition <- condition
    AGMT$normalization <- normalization

    if(mod == 'none'){AGMT$mod <- ''}
    if(mod == 'add limit of detection > 0'){AGMT$mod <- 1}
    if(mod == 'add global data mean'){AGMT$mod <- 2}
    if(mod == 'add feature means'){AGMT$mod <- 3}

    if(logt == 'none'){AGMT$log <- ''}
    if(logt == 'e'){AGMT$log <- 'e'}
    if(logt == 2){AGMT$log <- 2}
    if(logt == 10){AGMT$log <- 10}
    AGMT$revert <- revert
    AGMT$split <- split.b
    AGMT$split2 <- split.bc
    AGMT$plot.n <- plot.n

    if(correction == "ANCOVA"){

      if(condition == 'none'){AGMT$condition <- ''}
      if(condition == 'impute 0'){AGMT$condition <- 0}
      if(condition == 'impute half of limit of detection'){AGMT$condition <- 1}
      if(condition == 'impute limit of detection'){AGMT$condition <- 2}
      if(condition == 'censoring threshold'){
        AGMT$condition <- 'c'
        AGMT$method <- 'tobit'
        tkmessageBox(message = 'CAUTION: method will be set to censored regression when censoring threshold (lod) is chosen.',
                     icon = 'warning')
      }

      AGMT$minQC <- as.integer(minQC)


      if(method == 'linear model'){AGMT$method <- 'lm'}
      if(method == 'robust linear model'){AGMT$method <- 'rlm'}
      if(method == 'censored regression (tobit)'){
        AGMT$method <- 'tobit'
        AGMT$condition <- 'c'

        tkmessageBox(message = 'CAUTION: imputation will be set to censoring threshold (lod) when censored regression is chosen.',
                     icon = 'warning')
      }

      AGMT$PCA <- PCA
      AGMT$Duplo <- Duplo
      AGMT$pdist <- pdist

    }

    if(normalization == "LOESS"){
      AGMT$LOESS_SPAN <- as.numeric(LOESS_SPAN)
      AGMT$LOESS_WARNING <- as.numeric(LOESS_WARNING)

      remove(LOESS_SPAN, LOESS_WARNING, envir = .GlobalEnv)
    }
    # if(normalization == "RUVs"){AGMT$k <- as.numeric(k)}


    AGMT$out1 <- as.numeric(o1)
    AGMT$out2 <- as.numeric(o2)
    AGMT$out3 <- as.numeric(o3)
    AGMT$out4 <- as.numeric(o4)
    AGMT$out5 <- as.numeric(o5)


    AGMT$long <- as.numeric(long)


    AGMT$bn <- bn
    AGMT$qcn <- qcn


    remove(data.in, correction, method, condition, fpath,
           QC, qcn, normalization, long, revert, pdist,
           o1, o2, o3, o4, o5, PCA, Duplo, minQC, logt, mod,
           path.lab, data.lab, cancelled, split.b, bn, plot.n,
           envir = .GlobalEnv)

    return(AGMT)

    ## Alternative terminal input
  } else {

    start.dir <- getwd()

    cat("Press enter to proceed.\n\n")
    ent <- readline()
    remove(ent)

    bct.msg("Please select a .csv file of the required format to apply correction on:")
    AGMT$data.in <- file.choose()


    # Methods
    bct.msg('Please select the batch effect correction method to use: \n\n1 - analysis of covariance (ANCOVA)')
    correction <- as.numeric(readline())


    if(correction == 1){
      AGMT$correction <- 'ANCOVA'
    }

    bct.msg('Please select the intra-batch normalization method to use: \n\n1 - locally weighted scatterplot smoothing (LOESS)')
    correction <- as.numeric(readline())
    if(correction == 1){
      AGMT$normalization <- 'LOESS'
    }

    bct.msg('Should QCs be used for correction; if not, study samples will be used):\n\n0 - No\n1 - Yes')
    AGMT$QC <- as.numeric(readline())



    # Correction
    cat("\nNext, you will be asked for the batch effect correction options\n\n")

    if(AGMT$correction == 'ANCOVA'){
      bct.msg("ANCOVA has been selected as correction method\n\nWhich model should be applied for batch effect correction:\n\n1 - linear model\n2 - robust linear model\n3 - censored regression by Tobit")
      method <- as.numeric(readline())

      if(method == 1){
        AGMT$method <- 'lm'
      }
      if(method == 2){
        AGMT$method <- 'rlm'
      }
      if(method == 3){
        AGMT$method <- 't'
        AGMT$condition <- 'c'
        bct.msg("You chose censored regression for correction, which requires a censoring threshold.\nThis value will be determined during correction and no imputation will be applied.")
      }

      if(AGMT$method != 't'){
       bct.msg('What value should be used to impute missing data (optional):\n\n0 - none\n1 - zero\n2 - half of limit of detection\n3 - limit of detection')
        condition <- as.numeric(readline())
        if(condition == 0){AGMT$condition <- ''}
        if(condition == 1){AGMT$condition <- 0}
        if(condition == 2){AGMT$condition <- 1}
        if(condition == 3){AGMT$condition <- 2}
      }


      bct.msg('What is the minimum number of quality control samples for the sample batches?\nBatches with less QCs will be omitted; defaults to 4.')
      AGMT$minQC <- as.numeric(readline())

      cat("\nANCOVA input complete. Proceeding with normalization inputs.\n\n\n")
    }

    # Normalizaion
    if(AGMT$normalization == 'LOESS'){

      bct.msg("LOESS has been selected\n\nChoose a span parameter (best between 0 and 1):")
      AGMT$LOESS_SPAN <- as.numeric(readline())
      AGMT$LOESS_WARNING <- T

      bct.msg("Should normalization plots be saved as .png files?\n\n0 - No\n1 - Yes")
      AGMT$plot.n <- as.numeric(readline())

      bct.msg("Should normalization be applied batchwise (split data)?\nCAUTION: applying normalization on the complete data is not considered intra-batch normalization. Use at own risk.\n\n0 - No\n1 - Yes")
      AGMT$split <- as.numeric(readline())

      bct.msg("Should data corrected for batch effects be normalized batchwise?\n\n0 - No\n1 - Yes")
      AGMT$split2 <- as.numeric(readline())

      cat("\nLOESS input complete. Proceeding to formatting options.\n\n\n")
    }


    # Formatting

    bct.msg("Please enter the batch naming as found in the sample names in vector format (i.e. c('Batch1', 'Batch2', ...):")
    AGMT$bn <- eval(parse(text = readline()))
    bct.msg("", side = 'bottom')

    bct.msg("Please enter the label of the quality control samples:")
    AGMT$qcn <- readline()

    bct.msg("In the data to correct, where are the features located?:\n\n0 - Columns\n1 - Rows")
    AGMT$long <- as.numeric(readline())

    bct.msg("How should the data be modified for correction (the data must not contain 0 measurements, as it obstructs the correction)?:\n\n0 - no modification\n1 - add limit of detection > 0 to all values\n2 - add data mean to all values")
    AGMT$mod <- as.numeric(readline())
    if(AGMT$mod == 0){AGMT$mod <- ''}

    bct.msg("Should the data be logarithmically transformed (when using linear modelling approaches, it may prove useful to log transform)?:\n\n0 - no log transform\n1 - ln\n2 - log2\n3 - log10")
    logt <- as.numeric(readline())
    if(logt == 0){AGMT$log <- ''}
    if(logt == 1){AGMT$log <- 'e'}
    if(logt == 2){AGMT$log <- 2}
    if(logt == 3){AGMT$log <- 10}

    bct.msg("Should the previously defined modifications and/or log transformations be reverted before saving output files?\n\n0 - No\n1 - Yes")
    AGMT$revert <- as.numeric(readline())


    cat("\nFormatting and modifications set. Proceeding to output options\n\n\n")


    bct.msg("Flag Bhattacharyya distance (has to be computed after pipeline completes correction)\n\n0 - No\n1 - Yes")
    AGMT$PCA <- as.numeric(readline())

    bct.msg("Flag Repeatability (requires duplicate measurements; has to be computed after pipeline completes correction)\n\n0 - No\n1 - Yes")
    AGMT$Duplo <- as.numeric(readline())

    bct.msg("Flag p-value distributions (has to be computed after pipeline completes correction)\n\n0 - No\n1 - Yes")
    AGMT$pdist <- as.numeric(readline())

    bct.msg("Save files of modified and split data?\n\n0 - No\n1 - Yes")
    AGMT$out1 <- as.numeric(readline())

    bct.msg("Save file of corrected data?\n\n0 - No\n1 - Yes")
    AGMT$out2 <- as.numeric(readline())

    bct.msg("Save files of normalized (and split) data?\n\n0 - No\n1 - Yes")
    AGMT$out3 <- as.numeric(readline())

    bct.msg("Save file of data that has been normalized after correction?\n\n0 - No\n1 - Yes")
    AGMT$out4 <- as.numeric(readline())

    bct.msg("Save file of data that has been corrected after normalization?\n\n0 - No\n1 - Yes")
    AGMT$out5 <- as.numeric(readline())

    bct.msg("Where should the output files be saved to?")
    AGMT$path <- choose.dir(default = start.dir)

    bct.msg("Input complete! Proceeding with correction.")

    return(AGMT)
  }
}
