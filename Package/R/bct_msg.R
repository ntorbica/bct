#' @export
#'
#' @title Console message printing
#' @param msg Message text
#' @param side Broken line position. Can be "top" to print a line above the text, "bottom"
#'   to print a line below the text, "both" to print lines above and below the text, or
#'   "none" to not print lines.
#' @description Printing function for prettier messages. Not very fancy.
#' @return Prints a message to the R console.

bct.msg <- function(msg, side = c('both', 'top', 'bottom', 'none')){

  if(missing(side)){
    side <- 'both'
  }
  if(side == 'both'){
    cat('-----------------------------------------------------------------------\n')
    cat(msg, '\n')
    cat('-----------------------------------------------------------------------\n\n')
  }

  if(side == 'top'){
    cat('-----------------------------------------------------------------------\n')
    cat(msg, '\n\n')
  }

  if(side == 'bottom'){
    cat(msg, '\n')
    cat('-----------------------------------------------------------------------\n\n')
  }

  if(side == 'none'){
    cat('\n\n')
    cat(msg, '\n\n')
  }
}
