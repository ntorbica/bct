#' @export
#'
#' @title Graphical user interface funciton to input method selection
#' @description Initial graphical user interface to choose the batch effect correction
#'   and normalization methods, and the choice to perform batch correction at hand of
#'   quality control samples or study samples.
#' @return Assigns global variables used in bct.in.main.

bct.in.methods <- function(){

  tclRequire("BWidget")
  tt <- tktoplevel()
  tktitle(tt) <- "Intialization"

  # main frame
  tt$env$main <- tk2frame(tt, borderwidth = 5, relief = "groove",
                          padding = 10)
  tkpack(tt$env$main, expand = FALSE, fill = 'x', side = 'top')

  # Top frame for inputs
  tt$env$main$top <- tk2frame(tt$env$main, borderwidth = 5, relief = "groove",
                              padding = 10)
  tkpack(tt$env$main$top, expand = FALSE, fill = 'x', side = 'top')

  tkgrid(tk2label(tt$env$main$top, text = "Choose correction, normalization and use of QCs:"), sticky = 'w')
  tkgrid(tk2label(tt$env$main$top, text = ""))


  # Correction Method
  corr <- c("ANCOVA")
  comboBox.c <- tkwidget(tt$env$main$top, "ComboBox", editable = F, values = corr)
  tkgrid(tk2label(tt$env$main$top, text = "Correction Method"), comboBox.c, padx = 5, sticky = 'w')

  # Normalization Method
  nlz <- c("LOESS")
  comboBox.n <- tkwidget(tt$env$main$top, "ComboBox", editable = F, values = nlz)
  tkgrid(tk2label(tt$env$main$top, text = "Normalization Method"), comboBox.n, padx = 5, sticky = 'w')


  # QC Checkbox
  QCb <- tk2checkbutton(tt$env$main$top, text = 'Use QC-samples for correction:')
  QCbVal <- tclVar("0")
  tkconfigure(QCb, variable = QCbVal)

  tkgrid(QCb, columnspan = 1, sticky = 'w', padx = 5)


  # Buttons
  OnOK <- function() {
    assign("correction", corr[as.numeric(tclvalue(tcl(comboBox.c, "getvalue"))) + 1], envir = .GlobalEnv)
    assign("normalization", nlz[as.numeric(tclvalue(tcl(comboBox.n, "getvalue"))) + 1], envir = .GlobalEnv)
    assign("QC", tclvalue(QCbVal), envir = .GlobalEnv)

    assign("cancelled", FALSE, envir = .GlobalEnv)
    tkdestroy(tt)
  }

  OnCancel <- function() {
    assign("cancelled", TRUE, envir = .GlobalEnv)
    tkdestroy(tt)
    tkmessageBox(message = "Pipeline stopped.")
  }

  tt$env$but <- tk2frame(tt, borderwidth = 5, relief = "flat",
                         padding = 10)
  tkpack(tt$env$but, expand = FALSE, fill = 'y', side = 'bottom')

  OK.but <- tk2button(tt$env$but, text = " Submit ", command = OnOK)
  Cancel.but <- tk2button(tt$env$but, text = " Cancel ", command = OnCancel)

  tkpack(tt$env$main, expand = TRUE, fill = "both", fill = "x")
  tkgrid(OK.but, Cancel.but)

  tkwait.window(tt)
}
