# Author: Saeesh Mangwani
# Date: 2020-03-17
# Description: A short script for exporting the resume to html and then pdf 

# ==== Loading libraries ====
library(rmarkdown)
library(pagedown)

rmarkdown::render("saeesh_resume.Rmd")
pagedown::chrome_print('saeesh_resume.html')

