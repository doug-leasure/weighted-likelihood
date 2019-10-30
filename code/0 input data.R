# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

# working directory
setwd('C:/RESEARCH/git/wpgp/weighted-likelihood')

# source directory
if(.Platform$OS.type=="unix"){
  srcdir <- '/Volumes/worldpop/Projects/WP517763_GRID3/Working/weighted-likelihood/in/'
} else {
  srcdir <- '//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_GRID3/Working/weighted-likelihood/in'
}
# copy source
file.copy(from=srcdir, to=getwd(), overwrite=T, recursive=T)


