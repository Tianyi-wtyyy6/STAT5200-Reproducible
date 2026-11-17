options(repos=c(CRAN='https://cloud.r-project.org'))
dir.create('.Rlib',showWarnings=FALSE)
.libPaths(c(normalizePath('.Rlib'),.libPaths()))
pkgs<-c('Synth','osqp','Matrix','ggplot2','dplyr','tidyr','readr','yaml','jsonlite','digest','renv','knitr','patchwork')
need<-pkgs[!vapply(pkgs,requireNamespace,TRUE,quietly=TRUE)]
if(length(need))install.packages(need,lib='.Rlib')
if(as.character(packageVersion('Synth'))!='1.1.10')install.packages('Synth',lib='.Rlib')
cat('Use renv::restore() to reproduce exact recorded versions; this script is an initial bootstrap.\n')
