# bct
Batch correction tool for removal of batch effects correction for untargeted LC-MS data


To install this package in R, type the following in a R console or RStudio:

install.packages("devtools") <----- Only required if the devtools package has not been previously installed

library(devtools)

install_github("ntorbica/bct/Package")


The pipeline is launched as follows:

BCT(gui = T)
This command calls the pipeline with a tcl/tk guided user interface (gui). If the pipeline fails, it is recommended to launch the pipeline with terminal inputting:

BCT(gui = F)
It is also advised to save the pipeline output to a variable:

bct.results <- BCT()



A user manual is in production. Supplementary material, such as example data or descriptions, are found in the 'Data' directory.
