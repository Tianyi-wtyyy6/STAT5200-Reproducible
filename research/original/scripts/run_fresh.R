#!/usr/bin/env Rscript
# Cold execution in a new directory; never copy models, predictions, cases, or results.
args<-commandArgs(trailingOnly=TRUE)
stage<-if('--stage' %in% args)args[match('--stage',args)+1L]else'all'
if(!stage %in% c('all','main'))stop('--stage must be all or main')
base<-normalizePath(getwd())
if(!file.exists(file.path(base,'run_all.R')))stop('Run from the extracted project root')
if(dir.exists('.Rlib')) .libPaths(c(normalizePath('.Rlib'),.libPaths()))
if(requireNamespace('renv',quietly=TRUE)) {
  lib<-renv::paths$library(project=base)
  if(dir.exists(lib)) .libPaths(c(lib,.libPaths()))
}
fresh<-file.path(base,'fresh_runs',paste0(format(Sys.time(),'%Y%m%dT%H%M%S',tz='UTC'),'_',Sys.getpid()))
dir.create(fresh,recursive=TRUE)
for(d in c('R','config','data','scripts'))stopifnot(file.copy(file.path(base,d),fresh,recursive=TRUE))
for(f in c('run_all.R','renv.lock'))stopifnot(file.copy(file.path(base,f),file.path(fresh,f)))
dir.create(file.path(fresh,'metadata'))
stopifnot(file.copy(file.path(base,'metadata','data_provenance.json'),file.path(fresh,'metadata','data_provenance.json')))
Sys.setenv(R_LIBS=paste(.libPaths(),collapse=.Platform$path.sep))
cat('Cold workspace:',fresh,'\nNo model or result caches were copied.\n')
dir.create(file.path(base,'metadata'),showWarnings=FALSE)
writeLines(fresh,file.path(base,'metadata','fresh_last_workspace.txt'))
setwd(fresh)
status<-system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','run_all.R','--config','config/project.yml','--stage',stage))
setwd(base)
if(status!=0)quit(status=status)
candidate<-file.path(fresh,'results',readLines(file.path(fresh,'metadata','current_run.txt')))
reference<-file.path(base,'results',readLines(file.path(base,'provenance','reference_run_id.txt')))
cmd<-c('--vanilla','scripts/verify_reproduction.R','--candidate',shQuote(candidate),'--reference',shQuote(reference),
       '--output',shQuote(file.path(fresh,'metadata','reproduction_comparison.csv')))
if(stage=='main')cmd<-c(cmd,'--main-only')
status<-system2(file.path(R.home('bin'),'Rscript'),cmd)
cat('Cold workspace retained at',fresh,'\n')
quit(status=status)
