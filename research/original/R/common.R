`%||%` <- function(x,y) if (is.null(x) || length(x)==0L) y else x
utc_now <- function() format(Sys.time(), '%Y-%m-%dT%H:%M:%OS3Z', tz='UTC')
yrs <- function(x) seq.int(as.integer(x[1]), as.integer(x[2]))
slug <- function(x) gsub('[^A-Za-z0-9]+','_',x)
hash_obj <- function(x) digest::digest(x, algo='sha256')
hash_file <- function(x) digest::digest(file=x,algo='sha256',serialize=FALSE)
write_csv <- function(x,path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  temporary<-tempfile(pattern='.csv_',tmpdir=dirname(path))
  readr::write_csv(as.data.frame(x),temporary,na='NA')
  assert(file.rename(temporary,path),paste('Atomic CSV replacement failed:',path))
}
read_csv <- function(path) as.data.frame(readr::read_csv(path,show_col_types=FALSE,progress=FALSE))
out_csv <- function(ctx,x,name) write_csv(x,file.path(ctx$result_dir,paste0(name,'.csv')))
bind_rows <- function(...) dplyr::bind_rows(...)
assert <- function(ok,msg) if (!isTRUE(ok)) stop(msg,call.=FALSE)
load_project_data <- function() readRDS('data/processed/panel.rds')
init_context <- function(config_path) {
  cfg <- yaml::read_yaml(config_path)
  assert(cfg$first_treated_year==1989L,'First treated year must be 1989')
  set.seed(cfg$seed)
  statistical_files <- c('run_all.R','R/common.R','R/data.R','R/models.R','R/experiments.R','R/checks.R','R/summaries.R')
  code <- setNames(vapply(statistical_files,hash_file,''),statistical_files)
  data_hash <- hash_file('data/raw/smoking.rda')
  packages <- c('Synth','osqp','Matrix','optimx','kernlab','rgenoud','ggplot2','dplyr','tidyr','readr','yaml','jsonlite','digest','renv','knitr','patchwork')
  versions <- setNames(vapply(packages,function(p) as.character(utils::packageVersion(p)),''),packages)
  signature <- hash_obj(list(config=hash_file(config_path),data=data_hash,code=code,versions=versions[c('Synth','osqp','Matrix','optimx','kernlab','rgenoud')]))
  ctx <- list(cfg=cfg,run_id=paste0('run_',substr(signature,1,16)),data_hash=data_hash,config_hash=hash_file(config_path),code_hash=hash_obj(code),code_files=code,versions=versions)
  ctx$result_dir <- file.path('results',ctx$run_id)
  ctx$figure_dir <- file.path('figures',ctx$run_id)
  ctx$log_dir <- file.path('logs',ctx$run_id)
  ctx$meta_dir <- file.path('metadata',ctx$run_id)
  for(p in c(ctx$result_dir,ctx$figure_dir,ctx$log_dir,ctx$meta_dir,'models')) dir.create(p,recursive=TRUE,showWarnings=FALSE)
  snapshot <- file.path(ctx$meta_dir,'project.yml')
  if(!file.exists(snapshot)) file.copy(config_path,snapshot)
  for(p in statistical_files) {
    dest<-file.path(ctx$meta_dir,'code_snapshot',p)
    dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
    if(!file.exists(dest))file.copy(p,dest)
  }
  writeLines(ctx$run_id,'metadata/current_run.txt')
  ctx
}
record_manifest <- function(ctx,stage,status) {
  p <- 'metadata/run_manifest.json'
  old <- if(file.exists(file.path(ctx$meta_dir,'run_manifest.json'))) jsonlite::read_json(file.path(ctx$meta_dir,'run_manifest.json'),simplifyVector=FALSE) else NULL
  files <- sort(list.files('R',pattern='\\.R$',full.names=TRUE))
  files <- c(files,'run_all.R','config/project.yml','renv.lock',list.files('scripts',pattern='\\.(R|py|sh)$',full.names=TRUE),list.files('report',pattern='\\.(tex|bib)$',full.names=TRUE))
  manifest <- list(run_id=ctx$run_id,created_utc=old$created_utc %||% utc_now(),updated_utc=utc_now(),status=status,last_stage=stage,
    data_hash=ctx$data_hash,config_hash=ctx$config_hash,statistical_code_hash=ctx$code_hash,
    code_sha256=as.list(setNames(vapply(files,hash_file,''),files)),packages=as.list(ctx$versions),R=R.version.string,
    platform=R.version$platform,seed=ctx$cfg$seed,command=paste(commandArgs(),collapse=' '),
    canonical_command='Rscript --vanilla run_all.R --config config/project.yml',
    data_provenance=jsonlite::read_json('metadata/data_provenance.json'),
    information_boundary='fit receives training rows only; predict receives donor outcomes only; score is the only step receiving held-out target labels',
    artifacts=list(results=ctx$result_dir,figures=ctx$figure_dir,logs=ctx$log_dir),
    design_status=ctx$cfg$design_status)
  jsonlite::write_json(manifest,p,pretty=TRUE,auto_unbox=TRUE,na='null')
  jsonlite::write_json(manifest,file.path(ctx$meta_dir,'run_manifest.json'),pretty=TRUE,auto_unbox=TRUE,na='null')
  capture.output(sessionInfo(),file=file.path(ctx$meta_dir,'sessionInfo.txt'))
}
ratio_stat <- function(pre,post,eps) {
  if(!is.finite(pre)||!is.finite(post)) return(list(ratio=NA_real_,status='invalid_mspe'))
  if(pre>eps) return(list(ratio=post/pre,status='defined'))
  if(post>eps) return(list(ratio=Inf,status='near_zero_pre_nonzero_post'))
  list(ratio=NA_real_,status='undefined_both_near_zero')
}
error_metrics <- function(error) {
  assert(length(error)>0 && all(is.finite(error)),'Cannot score missing/nonfinite errors')
  list(n_eval=length(error),mspe=mean(error^2),rmse=sqrt(mean(error^2)),mae=mean(abs(error)),mean_error=mean(error))
}
