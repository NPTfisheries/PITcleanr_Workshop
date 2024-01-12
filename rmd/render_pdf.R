# Author: Kevin See
# Purpose: turn html presentation into PDF
# Created: 110/2024
# Last Modified: 1/10/2024
# Notes:

#-----------------------------------------------------------------
# load needed libraries
library(renderthis)

# render presentation into PDF
to_pdf(from = "rmd/PITcleanr_Presentation.html",
       to = "rmd/PITcleanr_Presentation.pdf",
       complex_slides = T,
       partial_slides = T)


library(pagedown)

chrome_print(input = "rmd/PITcleanr_Presentation.html",
             format = "pdf")
