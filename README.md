# bct
Batch correction tool for removal of batch effects correction for untargeted LC-MS data


To install this package in R, type the following in a R console or RStudio:

install.packages("devtools") <----- Only required if the devtools package has not been previously installed

library(devtools)

install_github("ntorbica/bct/Package")


The pipeline is launched as follows:

BCT(gui = T)

This command calls the pipeline with a tcl/tk guided user interface (gui). If the pipeline fails to execute corrrectly, it is recommended to launch the pipeline in its default mode with the gui disabled (requires terminal inputs):

BCT(gui = F)

It is also advised to save the pipeline output to a variable:

bct.results <- BCT()

This produces three large list elements in the result variable, with one containing the data how it is passed during pipeline execution (DATA), one containing the formatted data that is outputted at the end of the pipeline (DATA.FORMATTED), and one containing the inputs given at the beginning of the pipeline (ARGS).



A user manual is in production. Supplementary material, such as example data or descriptions, are found in the 'Data' directory.
