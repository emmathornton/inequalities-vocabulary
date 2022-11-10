# inequalities-vocabulary

This repository contains all R code used for the paper **'Tracking the relation between different dimensions of socio-economic circumstance and vocabulary across developmental and historical time'**

A note on the Datasets, which contains details on which datasets need to be downloaded from the UK Data Service for these analyses, is also included. 

The repository is organised into folders based on each analysis run - for example, analyses for RQ 1- 3 can be found in the folder "MCS Only". Within the MCS only folder, the file 'MCS ses compile code.R' is outdated and not used in current analyses. Similarly, in the Cross cohort comparison folder, the scripts 'bcs_compile_cross_cohort.R' and 'mcs_compile_cross_cohort.R' are outdated and not used in current analyses. In sensitivity analyses -> age 14 SES sensitivity, 'age 14 SES variables.R' and 'age 14 sensitivity ANALYSES.R' are outdated and not used in these analyses. 

NOTE: In order for the Rmd files to run, a custom package 'imputools' is needed. To load this package: 

install.packages("devtools")
library(devtools)
install_github("cbannard/imputools")


One of the imputools package dependencies is the Dummies package, which is not available for the latest version of R. To get around this: 

- download the most uptodate version from https://cran.r-project.org/src/contrib/Archive/dummies/ 
- decompress the zip file 
Open the terminal window and type: 
- cd ~/Downloads
- R CMD INSTALL dummies_1.5.6
- open R studio and call the package library(dummies)
- library(imputools)
