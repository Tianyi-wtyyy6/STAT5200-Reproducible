#!/usr/bin/env Rscript
options(stringsAsFactors=FALSE,warn=1)
Sys.setenv(OPENBLAS_NUM_THREADS='1',OMP_NUM_THREADS='1',MKL_NUM_THREADS='1')
args<-commandArgs(trailingOnly=TRUE)
getarg<-function(key,default) {i<-match(key,args);if(is.na(i))default else args[i+1L]}
config<-getarg('--config','config/project.yml');stage<-getarg('--stage','all')
allowed<-c('all','data','main','checks','experiments','summarize','figures','tables')
if(!stage %in% allowed)stop('Stage must be one of: ',paste(allowed,collapse=', '))
if(dir.exists('.Rlib')) .libPaths(c(normalizePath('.Rlib'),.libPaths()))
if(file.exists('renv.lock') && requireNamespace('renv',quietly=TRUE)) {
  restored_library<-renv::paths$library(project=getwd())
  if(dir.exists(restored_library)) .libPaths(c(restored_library,.libPaths()))
}
required<-c('Synth','osqp','Matrix','ggplot2','dplyr','tidyr','readr','yaml','jsonlite','digest','renv','knitr','patchwork')
missing<-required[!vapply(required,requireNamespace,TRUE,quietly=TRUE)]
if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '),'. See README and scripts/install_dependencies.R')
for(f in c('common','data','models','experiments','checks','summaries','figures','tables'))source(paste0('R/',f,'.R'))
if(!file.exists('data/raw/smoking.rda'))source('scripts/acquire_data.R')
ctx<-init_context(config)
logfile<-file.path(ctx$log_dir,paste0('run_',format(Sys.time(),'%Y%m%dT%H%M%S',tz='UTC'),'_',stage,'.log'))
con<-file(logfile,'wt');sink(con,split=TRUE);sink(con,type='message')
cat('Run:',ctx$run_id,' Stage:',stage,' UTC:',utc_now(),'\n')
record_manifest(ctx,stage,'running')
tryCatch({
  if(stage %in% c('all','data') || !file.exists('data/processed/panel.rds') || !file.exists(file.path(ctx$result_dir,'data_audit.csv')))d<-audit_data(ctx)else d<-load_project_data()
  reg<-build_registry(ctx,d)
  if(stage %in% c('all','main'))run_cases(ctx,d,reg,'main')
  if(stage %in% c('all','checks'))run_acceptance_checks(ctx,d,reg)
  if(stage %in% c('all','experiments'))run_cases(ctx,d,reg,setdiff(unique(reg$analysis_type),'main'))
  if(stage=='main')collect_cases(ctx,reg)
  if(stage %in% c('all','summarize','experiments')) {
    obj<-collect_cases(ctx,reg);summarize_results(ctx,obj)
  }
  if(stage %in% c('all','figures'))make_figures(ctx)
  if(stage %in% c('all','tables'))export_display_tables(ctx)
  if(stage=='all') {
    checks<-c('acceptance_checks','batch_acceptance_checks')
    for(name in checks)assert(all(read_csv(file.path(ctx$result_dir,paste0(name,'.csv')))$status=='pass'),paste(name,'failed'))
    file.copy(file.path(ctx$meta_dir,'sessionInfo.txt'),'metadata/sessionInfo.txt',overwrite=TRUE)
  }
  stat<-if(file.exists(file.path(ctx$result_dir,'run_status.csv')))read_csv(file.path(ctx$result_dir,'run_status.csv'))else NULL
  completed<-!is.null(stat)&&all(stat$status %in% c('success','skipped'))
  record_manifest(ctx,stage,if(completed)'experiments_complete'else'partial_stages')
  cat('Stage completed at',utc_now(),'\n')
},error=function(e){record_manifest(ctx,stage,'partial');cat('ERROR:',conditionMessage(e),'\n');sink(type='message');sink();close(con);quit(status=1)})
sink(type='message');sink();close(con)
