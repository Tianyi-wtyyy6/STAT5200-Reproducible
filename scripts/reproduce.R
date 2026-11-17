#!/usr/bin/env Rscript
# Orchestration only: never edits the archived scientific implementation.
options(warn=1)
args <- commandArgs(trailingOnly=TRUE)
usage <- function() cat('Usage: Rscript --vanilla scripts/reproduce.R {smoke|main|full|figures} [--output NEW_DIRECTORY]\nAll output directories must be new. full/main start without any fit/result caches.\nfigures redraws the supplied reference results without fitting.\n')
if (!length(args) || args[1] %in% c('-h','--help')) { usage(); quit(status=0) }
mode <- args[1]
if (!mode %in% c('smoke','main','full','figures')) stop('Unknown mode; use --help')
if (!(length(args)==1L || (length(args)==3L && args[2]=='--output' && nzchar(args[3])))) stop('Invalid arguments; use --help')
script <- sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE)[1])
root <- normalizePath(file.path(dirname(script),'..'))
original <- file.path(root,'research','original')
output <- if(length(args)==3L) args[3] else file.path(root,'results','runs',paste0(mode,'_',format(Sys.time(),'%Y%m%dT%H%M%S',tz='UTC'),'_',Sys.getpid()))
if(file.exists(output) || dir.exists(output)) stop('Output already exists; choose a new directory to preserve earlier runs')
required <- c('Synth','osqp','Matrix','optimx','kernlab','rgenoud','ggplot2','dplyr','tidyr','readr','yaml','jsonlite','digest','renv','knitr','patchwork')
missing <- required[!vapply(required,requireNamespace,TRUE,quietly=TRUE)]
if(length(missing)) stop('Missing packages: ',paste(missing,collapse=', '),'; see README.md')
manifest <- readLines(file.path(original,'SHA256SUMS'))
for(line in manifest) {
  hash <- substr(line,1,64); path <- substring(line,67)
  actual <- digest::digest(file=file.path(original,path),algo='sha256',serialize=FALSE)
  if(!identical(hash,actual)) stop('Archived input modified: ',path)
}
dir.create(output,recursive=TRUE); output <- normalizePath(output)
Sys.setenv(OPENBLAS_NUM_THREADS='1',OMP_NUM_THREADS='1',MKL_NUM_THREADS='1',
           XDG_CACHE_HOME=file.path(output,'cache'),MPLCONFIGDIR=file.path(output,'cache','matplotlib'))
start <- Sys.time()
sha <- tryCatch(system2('git',c('-C',shQuote(root),'rev-parse','HEAD'),stdout=TRUE,stderr=FALSE),error=function(e)'unavailable')
versions <- setNames(vapply(required,function(p)as.character(packageVersion(p)),''),required)
record <- list(mode=mode,commit=sha,R=R.version.string,platform=R.version$platform,packages=as.list(versions),
               started_utc=format(start,tz='UTC'),seed=5200,initial_fit_caches=if(mode=='figures')'reference artifacts only'else'none',status='running')
write_record <- function() jsonlite::write_json(record,file.path(output,'validation.json'),pretty=TRUE,auto_unbox=TRUE)
write_record()
status <- tryCatch({
  setwd(output)
  if(mode=='smoke') {
    source(file.path(root,'tests','smoke.R'))
    run_smoke(original,output)
  } else {
    for(d in c('R','config','scripts')) stopifnot(file.copy(file.path(original,d),output,recursive=TRUE))
    for(f in c('run_all.R','renv.lock')) stopifnot(file.copy(file.path(original,f),file.path(output,f)))
    dir.create('data/raw',recursive=TRUE); dir.create('data/processed',recursive=TRUE); dir.create('metadata')
    stopifnot(file.copy(file.path(original,'data/raw/smoking.rda'),'data/raw/smoking.rda'))
    stopifnot(file.copy(file.path(original,'metadata/data_provenance.json'),'metadata/data_provenance.json'))
    reference_id <- readLines(file.path(original,'provenance/reference_run_id.txt'))
    if(mode=='figures') {
      dir.create('results'); stopifnot(file.copy(file.path(original,'results',reference_id),'results',recursive=TRUE))
      stopifnot(file.copy(file.path(original,'models'),output,recursive=TRUE))
      for(f in c('common','models','figures','tables')) source(paste0('R/',f,'.R'))
      ctx <- list(cfg=yaml::read_yaml('config/project.yml'),run_id=reference_id,
                  result_dir=file.path('results',reference_id),figure_dir=file.path('figures',reference_id),
                  log_dir=file.path('logs',reference_id),versions=versions)
      dir.create(ctx$log_dir,recursive=TRUE)
      make_figures(ctx); export_display_tables(ctx)
      writeLines(reference_id,'metadata/current_run.txt')
    } else {
      rc <- system2(file.path(R.home('bin'),'Rscript'),c('--vanilla','run_all.R','--config','config/project.yml','--stage',if(mode=='full')'all'else'main'))
      if(rc!=0L) stop('Computational driver failed, status ',rc)
    }
    candidate <- file.path(output,'results',readLines('metadata/current_run.txt'))
    compare <- c('--vanilla',shQuote(file.path(original,'scripts/verify_reproduction.R')),
                 '--candidate',shQuote(candidate),'--reference',shQuote(file.path(original,'results',reference_id)),
                 '--output',shQuote(file.path(output,'reproduction_comparison.csv')))
    if(mode=='main') compare <- c(compare,'--main-only')
    rc <- system2(file.path(R.home('bin'),'Rscript'),compare)
    if(rc!=0L) stop('Reference comparison failed; see reproduction_comparison.csv (tolerance unchanged)')
    if(mode %in% c('full','figures')) {
      idx <- read.csv(file.path(candidate,'figure_table_index.csv'))
      stopifnot(sum(idx$type=='figure')==59L,sum(idx$type=='table')==28L)
      for(col in c('pdf_path','png_path','csv_path')) {
        files <- idx[[col]][!is.na(idx[[col]]) & nzchar(idx[[col]])]
        stopifnot(all(file.exists(files)),all(file.info(files)$size>0))
      }
    }
  }
  0L
},error=function(e) { message(conditionMessage(e));record$error <<- conditionMessage(e);1L })
record$status <- if(status==0L)'pass'else'fail'
record$elapsed_seconds <- as.numeric(difftime(Sys.time(),start,units='secs'))
write_record()
cat('Validation:',record$status,'; output:',output,'\n')
quit(status=status)
