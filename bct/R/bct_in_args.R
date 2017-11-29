#' @export
#'
#' @title Graphical user interface function to input function arguments
#' @description Builds a graphical user interface depending on the choices taken in
#'   bct.in.methods. Takes all of the required function arguments used during pipeline
#'   execution.
#' @return Assigns global variables used in bct.in.main.

bct.in.args <- function(){
  ## Argument input interface

    tt <- tktoplevel()
    tktitle(tt) <- "Function Arguments"

    # Create main frame
    tt$env$main <- tk2frame(tt, borderwidth = 5, relief = "groove",
                            padding = 10)
    tkpack(tt$env$main, expand = FALSE, fill = 'x', side = 'top')

    # Top frame for fnc arguments
    tt$env$main$top <- tk2frame(tt$env$main, borderwidth = 5, relief = "groove",
                            padding = 10)
    tkpack(tt$env$main$top, expand = FALSE, fill = 'x', side = 'top')

    # Bottom frame for data, dir and buttons
    tt$env$main$bot <- tk2frame(tt$env$main, borderwidth = 5, relief = "groove",
                                padding = 10)
    tkpack(tt$env$main$bot, expand = FALSE, fill = 'x', side = 'bottom')


# Dynamic top frames -------------------------------------------------

    # Make frames for each set of arguments

#############################
    if(correction == 'ANCOVA'){

      tt$env$main$top$anc <- tk2frame(tt$env$main$top, borderwidth = 5, relief = "groove",
                             padding = 10, height = 100, width = 50)
      tkpack(tk2label(tt$env$main$top$anc,
                      text = 'ANCOVA arguments:'),
             side = "top", expand = FALSE, fill = "x", ipady = 5, ipadx = 5)
      tkpack(tk2label(tt$env$main$top$anc, text = ''))

      tkpack(tt$env$main$top$anc, expand = TRUE, fill = 'y', side = 'left')

      condition <- c('none', 'impute 0', 'impute half of limit of detection', 'impute limit of detection', 'censoring threshold')
      method <- c('linear model', 'robust linear model', 'censored regression (tobit)')


      # Method Choice
      tt$env$main$top$anc$met <- tk2frame(tt$env$main$top$anc, borderwidth = 1, relief = "flat",
                                 padding = 1)
      tkpack(tt$env$main$top$anc$met, side = 'top')

      comboBox.m <- tkwidget(tt$env$main$top$anc$met, "ComboBox", editable = F, values = method)

      tkgrid(tk2label(tt$env$main$top$anc$met, text = 'Method (linear model):'), columnspan = 2)
      tkgrid(comboBox.m, columnspan = 2)
      tkgrid(tk2label(tt$env$main$top$anc$met, text = ''))


      # Imputation Choice
      tt$env$main$top$anc$imp <- tk2frame(tt$env$main$top$anc, borderwidth = 1, relief = "flat", padding = 1)
      tkpack(tt$env$main$top$anc$imp, side = 'top')
      comboBox.cond <- tkwidget(tt$env$main$top$anc$imp, "ComboBox", editable = F, values = condition)

      tkgrid(tk2label(tt$env$main$top$anc$imp, text = 'NA imputation:'), columnspan = 2)
      tkgrid(comboBox.cond, columnspan = 2)
      tkgrid(tk2label(tt$env$main$top$anc$imp, text = ''))


      # T/F Buttons
      tt$env$main$top$anc$R <- tk2frame(tt$env$main$top$anc, borderwidth = 1, relief = "flat")
      PCAb <- tk2checkbutton(tt$env$main$top$anc$R, text = 'Compute Bhattacharyya dist.:')
      REPb <- tk2checkbutton(tt$env$main$top$anc$R, text = 'Compute repeatabilities:')
      Pdb <- tk2checkbutton(tt$env$main$top$anc$R, text = 'Compute p-value distribution:')

      PCAbVal <- tclVar("0")
      REPbVal <- tclVar("0")
      PdbVal <- tclVar("0")

      tkconfigure(PCAb, variable = PCAbVal)
      tkconfigure(REPb, variable = REPbVal)
      tkconfigure(Pdb, variable = PdbVal)

      tkpack(tt$env$main$top$anc$R, side = 'top')

      tkgrid(PCAb, columnspan = 1, sticky = 'w')
      tkgrid(REPb, columnspan = 1, sticky = 'w')
      tkgrid(Pdb, columnspan = 1, sticky = 'w')

      tkgrid(tk2label(tt$env$main$top$anc$R, text = ''))


      # minQC slider
      tt$env$main$top$anc$B <- tk2frame(tt$env$main$top$anc, borderwidth = 1, relief = 'flat')
      tkpack(tt$env$main$top$anc$B, side = 'bottom')

      sV.a <- tclVar(4)

      tt$env$main$top$anc$B$slab <- tk2label(tt$env$main$top$anc$B, text = 'number of QCs per batch: 4')
      tkgrid(tt$env$main$top$anc$B$slab)

      slide <- function(...){
        val <- as.integer(tclvalue(sV.a))
        lab <- sprintf("number of QCs per batch: %d", val)
        tkconfigure(tt$env$main$top$anc$B$slab, text = lab)
      }

      tt$env$main$top$anc$B$slider <- tk2scale(tt$env$main$top$anc$B, from = 0, to = 50,
                         variable = sV.a, orient = 'h', command = slide, length = 200)

      tkgrid(tt$env$main$top$anc$B$slider)
    }

##########################
    if(normalization == 'LOESS'){

      tt$env$main$top$lss <- tk2frame(tt$env$main$top, borderwidth = 5, relief = "groove",
                                  padding = 10, height = 100, width = 50)

      tkpack(tk2label(tt$env$main$top$lss,
                      text = 'LOESS arguments:'),
             side = "top", expand = FALSE, fill = "x", ipady = 5, ipadx = 5)
      tkpack(tk2label(tt$env$main$top$lss, text = ''))

      tkpack(tt$env$main$top$lss, expand = TRUE, fill = 'y', side = 'left')

      tt$env$main$top$lss$t <- tk2frame(tt$env$main$top$lss, borderwidth = 1, relief = 'flat')
      tkpack(tt$env$main$top$lss$t, side = 'top')

      # Span slider

      #
      # sV.l <- tclVar(0.75)
      #
      # tt$env$main$top$lss$t$slab <- tk2label(tt$env$main$top$lss$t, text = 'LOESS span: 0.75')
      # tkgrid(tt$env$main$top$lss$t$slab)
      #
      # # slide <- function(...){
      # #   val <- round(as.numeric(tclvalue(sV.l)), digits = 2)
      # #   lab <- sprintf("LOESS span: %g", round(val, digits = 2))
      # #   tkconfigure(tt$env$main$top$lss$t$slab, text = lab)
      # # }
      #
      # tt$env$main$top$lss$t$slider <- tk2scale(tt$env$main$top$lss$t, from = 0, to = 1,
      #                                      variable = sV.l, orient = 'h', command = slide, length = 200)
      #
      # tkgrid(tt$env$main$top$lss$t$slider, sticky = 'n')
      # tkgrid(tk2label(tt$env$main$top$lss$t, text = ''))

      # Span entry
      sp <- tclVar('0.75')

      enter.sp <- tk2entry(tt$env$main$top$lss$t, textvariable = sp, width = 25)
      tkgrid(tk2label(tt$env$main$top$lss$t, text = 'LOESS Span:'))
      tkgrid(enter.sp)
      tkgrid(tk2label(tt$env$main$top$lss$t, text = ''))

      # Warning button
      Wb <- tk2checkbutton(tt$env$main$top$lss$t, text = 'Show warnings (if produced):')

      WbVal <- tclVar("0")

      tkconfigure(Wb, variable = WbVal)

      tkgrid(Wb, columnspan = 1, sticky = 'w')
      tkgrid(tk2label(tt$env$main$top$lss$t, text = ''))

    }

########################


# Mandatory formatting input ----------------------------------------------

    tt$env$main$top$fmt <- tk2frame(tt$env$main$top, borderwidth = 5, relief = "groove",
                                padding = 10, height = 100, width = 50)

    tkpack(tk2label(tt$env$main$top$fmt,
                    text = 'Formatting options:'),
           side = "top", expand = FALSE, fill = "x", ipady = 5, ipadx = 5)
    tkpack(tk2label(tt$env$main$top$fmt, text = ''))

    tkpack(tt$env$main$top$fmt, expand = TRUE, fill = 'y', side = 'left')

    tt$env$main$top$fmt$t <- tk2frame(tt$env$main$top$fmt, borderwidth = 1, relief = 'flat')
    tkpack(tt$env$main$top$fmt$t, side = 'top')

    Sb <- tk2checkbutton(tt$env$main$top$fmt$t, text = 'Normalize batches individually (split data):')
    SbVal <- tclVar("0")
    tkconfigure(Sb, variable = SbVal)

    tkgrid(Sb, columnspan = 1, sticky = 'w')
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))

    Scb <- tk2checkbutton(tt$env$main$top$fmt$t, text = 'Normalize corrected batches individually (split data):')
    ScbVal <- tclVar("0")
    tkconfigure(Scb, variable = ScbVal)

    tkgrid(Scb, columnspan = 1, sticky = 'w')
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))

    Pb <- tk2checkbutton(tt$env$main$top$fmt$t, text = 'Save feature normalization plots:')
    PbVal <- tclVar("0")
    tkconfigure(Pb, variable = PbVal)

    tkgrid(Pb, columnspan = 1, sticky = 'w')
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))


#####
    bn <- tclVar("c('Batch1', 'Batch2')")

    enter.bn <- tk2entry(tt$env$main$top$fmt$t, textvariable = bn, width = 25)
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = 'Batch names:'))
    tkgrid(enter.bn)
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))

    qcn <- tclVar("'QC'")

    enter.qcn <- tk2entry(tt$env$main$top$fmt$t, textvariable = qcn, width = 25)
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = 'Quality control names:'))
    tkgrid(enter.qcn)
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))


    Lb <- tk2checkbutton(tt$env$main$top$fmt$t, text = 'Features are in rows')
    LbVal <- tclVar("0")
    tkconfigure(Lb, variable = LbVal)

    tkgrid(Lb, columnspan = 1, sticky = 'w')
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))

    tkconfigure(PCAb, variable = PCAbVal)

    mod <- c('none', 'add limit of detection > 0', 'add global data mean', 'add feature means')
    logmod <- c('none', 'e', 2, 10)
    comboBox.mod <- tkwidget(tt$env$main$top$fmt$t, "ComboBox", editable = F, values = mod)

    tkgrid(tk2label(tt$env$main$top$fmt$t, text = 'Data modification:'), columnspan = 2)
    tkgrid(comboBox.mod, columnspan = 2)
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))


    comboBox.log <- tkwidget(tt$env$main$top$fmt$t, "ComboBox", editable = F, values = logmod)

    tkgrid(tk2label(tt$env$main$top$fmt$t, text = 'Log transformation:'), columnspan = 2)
    tkgrid(comboBox.log, columnspan = 2)
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))

    Rb <- tk2checkbutton(tt$env$main$top$fmt$t, text = 'Revert modification before output:')
    RbVal <- tclVar("0")
    tkconfigure(Rb, variable = RbVal)
    tkgrid(Rb, columnspan = 1, sticky = 'w')
    tkgrid(tk2label(tt$env$main$top$fmt$t, text = ''))


# Mandatory output frame --------------------------------------------------

    tt$env$main$top$opt <- tk2frame(tt$env$main$top, borderwidth = 5, relief = "groove",
                                padding = 10, height = 100, width = 50)
    tkpack(tk2label(tt$env$main$top$opt,
                    text = 'Output .csv file options:'),
           side = "top", expand = FALSE, fill = "x", ipady = 5, ipadx = 5)
    tkpack(tk2label(tt$env$main$top$opt, text = ''))

    tkpack(tt$env$main$top$opt, expand = TRUE, fill = 'y', side = 'left')

    tt$env$main$top$opt$t <- tk2frame(tt$env$main$top$opt, borderwidth = 1, relief = 'flat')
    tkpack(tt$env$main$top$opt$t, side = 'top')

    # Output buttons
    O1 <- tk2checkbutton(tt$env$main$top$opt$t, text = 'Modified')
    O2 <- tk2checkbutton(tt$env$main$top$opt$t, text = 'Corrected')
    O3 <- tk2checkbutton(tt$env$main$top$opt$t, text = 'Normalized')
    O4 <- tk2checkbutton(tt$env$main$top$opt$t, text = 'Corrected -> Normalized')
    O5 <- tk2checkbutton(tt$env$main$top$opt$t, text = 'Normalized -> Corrected')

    preset <- tk2checkbutton(tt$env$main$top$opt$t, text = 'Save BCT options to file')


    O1Val <- tclVar("0")
    O2Val <- tclVar("0")
    O3Val <- tclVar("0")
    O4Val <- tclVar("0")
    O5Val <- tclVar("0")

    presetVal <- tclVar("0")

    tkconfigure(O1, variable = O1Val)
    tkconfigure(O2, variable = O2Val)
    tkconfigure(O3, variable = O3Val)
    tkconfigure(O4, variable = O4Val)
    tkconfigure(O5, variable = O5Val)

    tkconfigure(preset, variable = presetVal)

    tkgrid(O1, columnspan = 1, sticky = 'w')
    tkgrid(O2, columnspan = 1, sticky = 'w')
    tkgrid(O3, columnspan = 1, sticky = 'w')
    tkgrid(O4, columnspan = 1, sticky = 'w')
    tkgrid(O5, columnspan = 1, sticky = 'w')
    tkgrid(tk2label(tt$env$main$top$opt$t, text = ''), sticky = 'n')

    tkgrid(preset, columnspan = 1, sticky = 'w')
    tkgrid(tk2label(tt$env$main$top$opt$t, text = ''), sticky = 'n')




# Bottom frame ------------------------------------------------------------

    s.dir <- function(){
      dir.sel <- tk_choose.dir()
      assign("fpath", dir.sel, envir = .GlobalEnv)
      assign("path.lab", tclVar(paste("      Output directory:   ", fpath)), envir = .GlobalEnv)
      tclvalue(p.lab) <- paste("      Output directory:   ", fpath)
      tkconfigure(path.lab, text = tclvalue(p.lab))
    }

    s.dat <- function(){
      dat.sel <- tk_choose.files()
      assign("data.in", dat.sel, envir = .GlobalEnv)
      assign("data.lab", tclVar(paste("      Peak file:                  ", data.in)), envir = .GlobalEnv)
      tclvalue(d.lab) <- paste("      Peak file:                  ", data.in)
      tkconfigure(data.lab, text = tclvalue(d.lab))
    }


    assign("fpath", "Directory path", envir = .GlobalEnv)
    assign("data.in", "Input Data (.csv)", envir = .GlobalEnv)

    tt$env$main$bot$sel <- tk2frame(tt$env$main$bot, borderwidth = 5, relief = "groove",
                                    padding = 10, height = 100, width = 50)
    tkpack(tt$env$main$bot$sel, side = 'top', expand = TRUE, fill = 'y', side = 'left')

    tkpack(tk2label(tt$env$main$bot$sel,
                    text = 'Data and Directory selection:'),
           side = "top", expand = FALSE, fill = "x", ipady = 5, ipadx = 5)
    tkpack(tk2label(tt$env$main$bot$sel, text = ''))
    tkpack(tt$env$main$bot$sel, expand = TRUE, fill = 'y', side = 'left')

    tt$env$main$bot$sel$dat <- tk2frame(tt$env$main$bot$sel, borderwidth = 2, relief = "flat")
    tkpack(tt$env$main$bot$sel$dat, expand = TRUE, fill = 'y', side = 'top')
    dat.but <- tk2button(tt$env$main$bot$sel$dat, text = "...", command = s.dat)
    d.lab <- tclVar(paste("      Peak file:                  ", data.in))
    data.lab <- tk2label(tt$env$main$bot$sel$dat, text = tclvalue(d.lab), width = 138, background = "#ffffff")
    tkgrid(dat.but, data.lab, sticky = 'w')

    # tkgrid(tk2label(tt$env$main$top$sel$dat, text = ''), sticky = 'n')

    tt$env$main$bot$sel$dir <- tk2frame(tt$env$main$bot$sel, borderwidth = 2, relief = "flat")
    tkpack(tt$env$main$bot$sel$dir, expand = TRUE, fill = 'y', side = 'top')
    dir.but <- tk2button(tt$env$main$bot$sel$dir, text = "...", command = s.dir)
    p.lab <- tclVar(paste("      Output directory:   ", fpath))
    path.lab <- tk2label(tt$env$main$bot$sel$dir, text = tclvalue(p.lab), width = 138, background = "#ffffff")
    tkgrid(dir.but, path.lab, sticky = 'w')

    # tkgrid(tk2label(tt$env$main$top$sel$dir, text = ''), sticky = 'n')

# Assignment of variables -------------------------------------------------

    OK <- function(){
      if(correction == 'ANCOVA'){
        assign("condition", condition[as.numeric(tclvalue(tcl(comboBox.cond, "getvalue"))) + 1], envir = .GlobalEnv)
        assign("method", method[as.numeric(tclvalue(tcl(comboBox.m, "getvalue"))) + 1], envir = .GlobalEnv)
        assign("PCA", tclvalue(PCAbVal), envir = .GlobalEnv)
        assign("Duplo", tclvalue(REPbVal), envir = .GlobalEnv)
        assign("pdist", tclvalue(PdbVal), envir = .GlobalEnv)
        assign("minQC", tclvalue(sV.a), envir = .GlobalEnv)
        assign("qcn", tclvalue(qcn), envir = .GlobalEnv)
      }

      if(normalization == 'LOESS'){
        assign("LOESS_SPAN", tclvalue(sp), envir = .GlobalEnv)
        assign("LOESS_WARNING", tclvalue(WbVal), envir = .GlobalEnv)
      }
      # if(normalization == 'RUVs'){
      #   assign("k", tclvalue(sV.c), envir = .GlobalEnv)
      # }

      assign("bn", eval(parse(text = tclvalue(bn))), envir = .GlobalEnv)


      assign("long", tclvalue(LbVal), envir = .GlobalEnv)

      assign("qcn", eval(parse(text = tclvalue(qcn))), envir = .GlobalEnv)

      assign("mod", mod[as.numeric(tclvalue(tcl(comboBox.mod, "getvalue"))) + 1], envir = .GlobalEnv)

      assign("logt", logmod[as.numeric(tclvalue(tcl(comboBox.log, "getvalue"))) + 1], envir = .GlobalEnv)

      assign("revert", as.numeric(tclvalue(RbVal)) , envir = .GlobalEnv)

      assign("split.b", as.numeric(tclvalue(SbVal)) , envir = .GlobalEnv)

      assign("split.bc", as.numeric(tclvalue(ScbVal)) , envir = .GlobalEnv)

      assign("plot.n", as.numeric(tclvalue(PbVal)), envir = .GlobalEnv)

      assign("o1", tclvalue(O1Val), envir = .GlobalEnv)
      assign("o2", tclvalue(O2Val), envir = .GlobalEnv)
      assign("o3", tclvalue(O3Val), envir = .GlobalEnv)
      assign("o4", tclvalue(O4Val), envir = .GlobalEnv)
      assign("o5", tclvalue(O5Val), envir = .GlobalEnv)

      assign("preset", tclvalue(presetVal), envir = .GlobalEnv)

      assign("cancelled", FALSE, envir = .GlobalEnv)

      tkdestroy(tt)
      tk_messageBox(message = "Inputs saved.\n\nProceeding with correction.")
    }

    CNC <- function(){
      assign("cancelled", TRUE, envir = .GlobalEnv)
      tkdestroy(tt)
      tk_messageBox(message = "Pipeline stopped.")
    }

# Confirmation ------------------------------------------------------------

    tt$env$but <- tk2frame(tt, borderwidth = 5, relief = "flat",
                            padding = 10)
    tkpack(tt$env$but, expand = FALSE, fill = 'y', side = 'bottom')

    OK.b <- tk2button(tt$env$but, text = 'Submit', command = OK)
    CNC.b <- tk2button(tt$env$but, text = 'Cancel', command = CNC)
    tkgrid(OK.b, CNC.b, stick = 'n', columnspan = 2)

    tkfocus(tt)
    tkwait.window(tt)
}
