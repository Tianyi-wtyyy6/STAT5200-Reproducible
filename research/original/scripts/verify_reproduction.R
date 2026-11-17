#!/usr/bin/env Rscript
# Compare scientific values by stable case keys, not by run IDs or execution timestamps.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k,d){i<-match(k,args);if(is.na(i))d else args[i+1L]}
candidate<-arg('--candidate',file.path('results',readLines('metadata/current_run.txt')))
reference<-arg('--reference',file.path('results',readLines('provenance/reference_run_id.txt')))
output<-arg('--output','metadata/reproduction_comparison.csv')
main_only<-'--main-only' %in% args
tol<-1e-6
read<-function(dir,name)read.csv(file.path(dir,paste0(name,'.csv')),check.names=FALSE,stringsAsFactors=FALSE,na.strings='NA')
checks<-list()
add<-function(table,field,ok,n=NA_integer_,difference=NA_real_,detail='') {
  checks[[length(checks)+1L]]<<-data.frame(table=table,field=field,n=n,max_absolute_difference=difference,
    tolerance=tol,status=if(ok)'pass'else'fail',detail=detail)
}
definitions<-list(
  annual_estimates=c('spec_id','case_id','method','comparison_set_id','year'),
  effect_summary=c('spec_id','case_id','method','comparison_set_id'),
  donor_weights=c('spec_id','case_id','method','comparison_set_id','donor'),
  did_offsets=c('spec_id','case_id','method','comparison_set_id'),
  predictor_balance=c('spec_id','case_id','method','comparison_set_id','predictor'),
  predictor_weights=c('spec_id','case_id','method','comparison_set_id','predictor'),
  validation_predictions=c('spec_id','case_id','method','comparison_set_id','year'),
  validation_case_metrics=c('spec_id','case_id','method','comparison_set_id'),
  paired_method_comparisons=c('analysis_type','case_id'),
  placebo_summary=c('comparison_set_id','method','target','filter'),
  robustness_summary=c('spec_id','case_id','method','comparison_set_id'),
  validation_aggregate=c('analysis_type','method'),
  published_vs_replicated=c('item'),
  placebo_ranks=c('method','filter'),
  placebo_common_eligible=c('method','filter'))
if(main_only)definitions<-definitions[seq_len(6)]
for(name in names(definitions)) {
  a<-read(reference,name);b<-read(candidate,name)
  if(main_only){a<-a[a$analysis_type=='main',];b<-b[b$analysis_type=='main',]}
  key<-definitions[[name]]
  ka<-do.call(paste,c(a[key],sep='\r'));kb<-do.call(paste,c(b[key],sep='\r'))
  valid<-!anyDuplicated(ka)&&!anyDuplicated(kb)&&setequal(ka,kb)
  add(name,'keys',valid,length(ka),detail='Same registered cases; unique stable keys')
  if(!valid)next
  b<-b[match(ka,kb),,drop=FALSE]
  numeric<-intersect(names(a)[vapply(a,is.numeric,TRUE)],names(b)[vapply(b,is.numeric,TRUE)])
  for(f in numeric) {
    x<-a[[f]];y<-b[[f]]
    same_na<-identical(is.na(x),is.na(y))
    same_inf<-identical(is.infinite(x),is.infinite(y))&&all(x[is.infinite(x)]==y[is.infinite(y)])
    ix<-is.finite(x)&is.finite(y);err<-if(any(ix))max(abs(x[ix]-y[ix]))else 0
    add(name,f,same_na&&same_inf&&err<=tol,sum(ix),err)
  }
}
st<-read(candidate,'run_status')
if(!main_only){add('run_status','case_counts',nrow(st)==375&&sum(st$status=='success')==373&&sum(st$status=='skipped')==2&&sum(st$status=='failed')==0,nrow(st))} else {add('run_status','main_success',sum(st$status=='success' & st$analysis_type=='main')==3,3)}
for(name in c('data_audit',if(!main_only)c('acceptance_checks','batch_acceptance_checks'))) {
  x<-read(candidate,name);add(name,'all_checks',all(x$status=='pass'),nrow(x))
}
if(!main_only) {
  index<-read(candidate,'figure_table_index');index<-index[index$type=='figure',]
  project<-normalizePath(file.path(candidate,'..','..'))
  p<-c(index$pdf_path,index$png_path);add('figures','all_59_pdf_png_pairs',nrow(index)==59&&all(file.exists(file.path(project,p))),length(p))
  add('scope','no_document_output',!dir.exists(file.path(project,'report')),detail='Numerical/figure execution needs no LaTeX')
}
out<-do.call(rbind,checks);dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
write.csv(out,output,row.names=FALSE,na='NA')
cat('Compared',nrow(out),'scientific value/structure checks; failures:',sum(out$status!='pass'),'\n')
if(any(out$status!='pass'))print(out[out$status!='pass',])
quit(status=if(all(out$status=='pass'))0 else 1)
