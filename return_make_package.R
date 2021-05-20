# return_make_package.R
# Create a package from return_calculations.R

# --> Set Working Directory !!
install.packages("devtools")

rm(list=ls())
source("return_calculations.R")
package.skeleton("retcalc")

# fill in all the files

# --> Set Working Directory to location of DESCRIPTION file 
devtools::check()
devtools::build()

library(rretcalc)
