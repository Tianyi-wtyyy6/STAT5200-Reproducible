build_registry <- function(ctx,d) {
  cfg<-ctx$cfg; states<-sort(unique(d$state),method='radix'); ca<-cfg$treated_state; donors<-setdiff(states,ca)
  rows<-list(); pools<-list()
  add<-function(case_id,method,analysis_type,target=ca,controls=donors,train=yrs(cfg$train_years),eval=yrs(cfg$post_years),
                comparison='CA_main_38',visible_end=max(eval),drop=NULL,perturbation='',skip='') {
    controls<-sort(controls,method='radix'); pool_id<-paste0('pool_',substr(hash_obj(list(target=target,controls=controls)),1,16))
    pools[[pool_id]]<<-data.frame(donor_pool_id=pool_id,state=states,included=states %in% controls,
      exclusion_reason=ifelse(states %in% controls,'',ifelse(states==target,'target',ifelse(states=='California','California excluded by clean design','prespecified donor removal'))))
    spec_id<-paste0(method,if(!is.null(drop))paste0('_drop_',drop)else'')
    rows[[length(rows)+1L]]<<-data.frame(run_id=ctx$run_id,spec_id=spec_id,case_id=case_id,method=method,analysis_type=analysis_type,
      comparison_set_id=comparison,donor_pool_id=pool_id,data_hash=ctx$data_hash,target=target,
      train_start=min(train),train_end=max(train),n_train=length(train),eval_start=min(eval),eval_end=max(eval),n_eval=length(eval),visible_end=visible_end,
      n_donors=length(controls),donors=paste(controls,collapse='|'),drop_predictor=drop %||% '',perturbation=perturbation,
      predictor_rule=if(method=='SCM_ADH')paste0('Original seven windows; omitted=',drop %||% 'none')else'Yearly training cigsale levels only',
      optimization=if(method=='SCM_ADH')as.character(jsonlite::toJSON(cfg$adh,auto_unbox=TRUE))else if(method=='SCM_OUTCOME')as.character(jsonlite::toJSON(cfg$qp,auto_unbox=TRUE))else'equal donor mean + training mean offset',
      seed=cfg$seed,planned_status=if(nzchar(skip))'skipped'else'planned',skip_reason=skip,stringsAsFactors=FALSE)
  }
  for(m in cfg$methods)add('CA_main',m,'main')
  for(f in cfg$validation$ca_folds) for(m in cfg$methods) add(f$id,m,'ca_validation',train=yrs(f$train),eval=yrs(f$eval),visible_end=f$visible_end,
    comparison='CA_pre_holdout',skip=if(m=='SCM_ADH')'not_applicable: original 1988 lag and covariate windows unavailable in this training period'else'')
  for(q in donors)for(m in cfg$methods)add(paste0('donor_',slug(q)),m,'donor_validation',target=q,controls=setdiff(donors,q),comparison=cfg$placebo$clean_id)
  for(q in states)for(m in cfg$methods)add(paste0('classic_',slug(q)),m,'classic_placebo',target=q,controls=setdiff(states,q),comparison=cfg$placebo$classic_id)
  for(q in donors)for(m in cfg$methods)add(paste0('LOO_',slug(q)),m,'loo',controls=setdiff(donors,q),comparison='CA_LOO_37',perturbation=q)
  for(win in cfg$robustness$train_windows)for(m in c('SCM_OUTCOME','DID'))add(paste0('window_',win[1]),m,'training_window',train=yrs(win),comparison='CA_window_38',perturbation=paste(win,collapse='-'))
  for(v in cfg$robustness$drop_predictors)add(paste0('drop_',v),'SCM_ADH','predictor_deletion',drop=v,comparison='CA_predictor_38',perturbation=v)
  for(f in cfg$robustness$time_placebos)for(m in c('SCM_OUTCOME','DID'))add(f$id,m,'time_placebo',train=yrs(f$train),eval=yrs(f$eval),visible_end=f$visible_end,comparison='CA_time_pre_only',perturbation=as.character(f$pseudo_year))
  for(m in c('SCM_OUTCOME','DID')) {
    add('near_policy_post',m,'near_policy',train=yrs(cfg$robustness$near_policy$train),comparison='CA_near_policy_38',perturbation='Exclude 1987-1988 from fit')
    add('near_policy_diagnostic',m,'near_policy_diagnostic',train=yrs(cfg$robustness$near_policy$train),eval=yrs(cfg$robustness$near_policy$diagnostic),visible_end=1988,comparison='CA_near_policy_38',perturbation='1987-1988 diagnostic')
  }
  reg<-bind_rows(rows); assert(!anyDuplicated(reg[c('case_id','spec_id','comparison_set_id')]),'Duplicate registry key')
  out_csv(ctx,reg,'spec_registry'); out_csv(ctx,bind_rows(pools),'donor_pool_members')
  reg
}
case_identifiers <- function(r,fit_id=NA_character_) {
  x<-r[c('run_id','spec_id','case_id','method','analysis_type','comparison_set_id','donor_pool_id','data_hash','target','train_start','train_end','n_train','eval_start','eval_end','n_eval','n_donors','perturbation')]
  x$fit_id<-fit_id; x
}
add_ids <- function(ids,x) cbind(ids[rep(1,nrow(x)),,drop=FALSE],x,row.names=NULL)
case_metrics <- function(scored,fit,r,ctx) {
  pre<-scored$year %in% seq.int(r$train_start,r$train_end); ev<-scored$year %in% seq.int(r$eval_start,r$eval_end)
  pm<-error_metrics(scored$gap[pre]); em<-error_metrics(scored$gap[ev])
  rr<-ratio_stat(pm$mspe,em$mspe,ctx$cfg$tolerances$near_zero_mspe)
  den<-sum(scored$y0_hat[ev]); relative<-if(den>0)-100*sum(scored$gap[ev])/den else NA_real_
  endpoint<-if(2000 %in% scored$year[ev])scored$gap[scored$year==2000] else NA_real_
  phases<-vapply(ctx$cfg$robustness$dynamic_periods,function(a)if(all(yrs(a) %in% scored$year[ev]))mean(scored$gap[scored$year %in% yrs(a)])else NA_real_,0)
  common<-1980:1988
  cg<-if(all(common %in% scored$year) && r$eval_start>=1989)scored$gap[match(common,scored$year)]else rep(NA_real_,9)
  role<-if(r$target=='California' && r$eval_start>=1989)'real_CA_policy_gap_not_observed_Y0_prediction_error'
    else if(r$analysis_type=='classic_placebo')'classic_pseudo_gap_deviation_statistic'
    else if(r$analysis_type=='donor_validation')'clean_heldout_observed_outcome_prediction'
    else'pre_policy_heldout_observed_outcome_prediction'
  data.frame(pre_mspe=pm$mspe,pre_rmspe=pm$rmse,pre_mae=pm$mae,pre_mean_gap=pm$mean_error,
    pre_gap_slope=unname(coef(lm(scored$gap[pre]~scored$year[pre]))[2]),eval_mspe=em$mspe,eval_rmse=em$rmse,eval_mae=em$mae,
    eval_gap_rms=em$rmse,eval_mean_abs_gap=em$mae,evaluation_role=role,
    post_mspe=if(r$eval_start>=1989)em$mspe else NA_real_,mspe_ratio=rr$ratio,ratio_status=rr$status,
    mean_gap=em$mean_error,gap_2000=endpoint,gap_2000_status=if(is.na(endpoint))'not_applicable'else'applicable',
    relative_reduction_pct=relative,relative_status=if(den>0)'counterfactual_sum_denominator'else'nonpositive_denominator_not_interpretable',
    phase_1989_1992=phases[1],phase_1993_1996=phases[2],phase_1997_2000=phases[3],
    common_1980_1988_rmspe=if(all(is.finite(cg)))sqrt(mean(cg^2))else NA_real_,
    common_1980_1988_mae=if(all(is.finite(cg)))mean(abs(cg))else NA_real_,
    max_weight=max(fit$w),H=sum(fit$w^2),effective_donors=1/sum(fit$w^2),offset=fit$offset,
    objective=fit$objective,raw_equal_mean_mspe=fit$raw_equal_mean_mspe,constraint_sum=fit$constraint_sum,constraint_nonneg=fit$constraint_nonneg,
    solver_status=fit$solver_status,status='success')
}
run_one_case <- function(ctx,d,r) {
  controls<-strsplit(r$donors,'|',fixed=TRUE)[[1]]
  inp<-prepare_training(d,r$target,controls,seq.int(r$train_start,r$train_end),r$method,if(nzchar(r$drop_predictor))r$drop_predictor else NULL)
  fit<-fit_model(inp,ctx,use_cache=TRUE)
  years<-seq.int(r$train_start,r$visible_end)
  donor_data<-d[d$state %in% controls & d$year %in% years,c('state','year','cigsale')]
  pred<-predict_model(fit,donor_data,years)
  pred_id<-paste0('pred_',substr(hash_obj(list(fit$fit_id,donor_data,years)),1,24))
  # Persist/freeze predictions before the scoring interface receives target truth.
  pred_path<-file.path('models',paste0(pred_id,'.rds'))
  if(!file.exists(pred_path))saveRDS(list(prediction_id=pred_id,fit_id=fit$fit_id,predictions=pred,donor_input_hash=hash_obj(donor_data),frozen_utc=utc_now()),pred_path)
  truth<-d[d$state==r$target & d$year %in% years,c('year','cigsale')]
  scored<-score_predictions(pred,truth)
  scored$period_role<-ifelse(scored$year %in% seq.int(r$train_start,r$train_end),'training',ifelse(scored$year %in% seq.int(r$eval_start,r$eval_end),'evaluation','diagnostic_or_unscored'))
  scored$prediction_id<-pred_id
  ids<-case_identifiers(r,fit$fit_id)
  list(annual=add_ids(ids,scored),metrics=add_ids(ids,case_metrics(scored,fit,r,ctx)),fit_id=fit$fit_id,
    cache_reused=fit$cache_reused,warnings=paste(fit$warnings,collapse=' | '),retries=length(fit$attempts)-1L,
    fit_computed_utc=fit$computed_utc)
}
run_cases <- function(ctx,d,reg,types=NULL) {
  case_dir<-file.path(ctx$result_dir,'cases'); dir.create(case_dir,showWarnings=FALSE)
  status_path<-file.path(ctx$result_dir,'run_status.csv')
  status<-if(file.exists(status_path))read_csv(status_path)else cbind(reg[c('run_id','spec_id','case_id','method','analysis_type','comparison_set_id')],
    data.frame(status=reg$planned_status,start_utc=NA_character_,end_utc=NA_character_,error=reg$skip_reason,warnings='',retries=NA_integer_,fit_id=NA_character_,cache_reused=NA,elapsed_sec=NA_real_))
  selection<-if(is.null(types))seq_len(nrow(reg))else which(reg$analysis_type %in% types)
  for(i in selection) {
    r<-reg[i,,drop=FALSE]; file<-file.path(case_dir,paste0(r$case_id,'__',r$spec_id,'.rds'))
    if(r$planned_status=='skipped') {status$status[i]<-'skipped';status$error[i]<-r$skip_reason;status$start_utc[i]<-status$end_utc[i]<-utc_now();next}
    if(file.exists(file) && status$status[i]=='success')next
    status$status[i]<-'running'; status$start_utc[i]<-utc_now(); write_csv(status,status_path)
    cat(sprintf('[%d/%d] %s %s (%s)\n',i,nrow(reg),r$case_id,r$method,r$analysis_type)); flush.console()
    t0<-proc.time()[3]
    ans<-tryCatch(run_one_case(ctx,d,r),error=function(e)e)
    status$end_utc[i]<-utc_now(); status$elapsed_sec[i]<-unname(proc.time()[3]-t0)
    if(inherits(ans,'error')) {
      status$status[i]<-'failed';status$error[i]<-conditionMessage(ans);cat('FAILED:',conditionMessage(ans),'\n')
    } else {
      saveRDS(ans,file);status$status[i]<-'success'; status$error[i]<-'';status$warnings[i]<-ans$warnings;status$retries[i]<-ans$retries;status$fit_id[i]<-ans$fit_id;status$cache_reused[i]<-ans$cache_reused
      cat(sprintf('  pre RMSPE %.4f; evaluation mean gap %.4f; cached=%s\n',ans$metrics$pre_rmspe,ans$metrics$mean_gap,ans$cache_reused));flush.console()
    }
    write_csv(status,status_path)
  }
  write_csv(status,status_path)
  invisible(status)
}
collect_cases <- function(ctx,reg) {
  status<-read_csv(file.path(ctx$result_dir,'run_status.csv')); success<-which(status$status=='success')
  cases<-lapply(success,function(i)readRDS(file.path(ctx$result_dir,'cases',paste0(status$case_id[i],'__',status$spec_id[i],'.rds'))))
  annual<-bind_rows(lapply(cases,`[[`,'annual')); metrics<-bind_rows(lapply(cases,`[[`,'metrics'))
  key<-c('run_id','spec_id','case_id','method','comparison_set_id','year')
  assert(!anyDuplicated(annual[key]),'Duplicate annual estimate keys')
  out_csv(ctx,annual,'annual_estimates');out_csv(ctx,metrics,'effect_summary')
  weights<-list(); offsets<-list(); bal<-list(); vs<-list();diagnostics<-list()
  for(i in success) {
    r<-reg[i,,drop=FALSE];fit<-readRDS(file.path('models',paste0(status$fit_id[i],'.rds')));ids<-case_identifiers(r,fit$fit_id)
    weights[[length(weights)+1L]]<-add_ids(ids,data.frame(donor=names(fit$w),weight=as.numeric(fit$w),raw_weight=as.numeric(fit$raw_w)))
    offsets[[length(offsets)+1L]]<-add_ids(ids,data.frame(offset=fit$offset,interpretation=if(r$method=='DID')'training mean target-minus-equal-donor difference'else'no intercept'))
    if(r$method=='SCM_ADH') {
      bal[[length(bal)+1L]]<-add_ids(ids,data.frame(predictor=rownames(fit$X1),target_value=as.numeric(fit$X1),synthetic_value=as.numeric(fit$X0%*%fit$w),
        equal_weight_donor_mean=rowMeans(fit$X0),gap=as.numeric(fit$X1-fit$X0%*%fit$w),scale_sd=as.numeric(fit$scale_sd),donor_mean_definition='unweighted; not published population-weighted Table 1 column'))
      vs[[length(vs)+1L]]<-add_ids(ids,data.frame(predictor=names(fit$V),V=as.numeric(fit$V),scale_sd=as.numeric(fit$scale_sd),original_scale_coefficient=as.numeric(fit$V/fit$scale_sd^2),scale_note='X divided by sample SD across target and donors; V normalized to sum 1; predictive, not causal importance'))
    }
    diagnostics[[length(diagnostics)+1L]]<-add_ids(ids,data.frame(objective=fit$objective,loss_w=fit$loss_w %||% NA_real_,solver_status=fit$solver_status,
      optimizer_summary=if(!is.null(fit$optimizer))as.character(jsonlite::toJSON(fit$optimizer,auto_unbox=TRUE,na='null'))else'',
      solver_info=if(!is.null(fit$solver_info))as.character(jsonlite::toJSON(fit$solver_info,auto_unbox=TRUE,na='null'))else'',
      hessian_rank=fit$hessian_rank %||% NA_integer_,hessian_dimension=fit$hessian_dimension %||% NA_integer_,
      constraint_sum=fit$constraint_sum,constraint_nonneg=fit$constraint_nonneg,retries=status$retries[i],warnings=status$warnings[i],cache_reused=status$cache_reused[i],computed_utc=fit$computed_utc))
  }
  out_csv(ctx,bind_rows(weights),'donor_weights');out_csv(ctx,bind_rows(offsets),'did_offsets');out_csv(ctx,bind_rows(bal),'predictor_balance');out_csv(ctx,bind_rows(vs),'predictor_weights');out_csv(ctx,bind_rows(diagnostics),'optimization_diagnostics')
  list(annual=annual,metrics=metrics,status=status,registry=reg,weights=bind_rows(weights),balance=bind_rows(bal))
}
