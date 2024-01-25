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

# render presentation into powerpoint
to_pptx(from = "rmd/PITcleanr_Presentation.html",
        to = "rmd/PITcleanr_Presentation.pptx",
        density = 300,
        complex_slides = T,
        partial_slides = T,
        delay = F,
        ratio = "16:9")

#-----------------------------------------------------------------
# try a different package
library(pagedown)

chrome_print(input = "rmd/PITcleanr_Presentation.html",
             format = "pdf")
