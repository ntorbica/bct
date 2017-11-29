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

bct.in.main <- function(){

  AGMT <- list()
# Input methods -----------------------------------------------------------
##############
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
  bct.in.args(QC)

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
}
