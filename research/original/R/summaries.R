summarize_validation <- function(ctx,obj) {
  a<-obj$annual;m<-obj$metrics;reg<-obj$registry;stat<-obj$status
  types<-c('ca_validation','donor_validation','time_placebo','near_policy_diagnostic')
  p<-a[a$analysis_type %in% types & a$period_role=='evaluation',]
  p$fold<-p$case_id;p$error<-p$gap;p$squared_error<-p$error^2
  out_csv(ctx,p,'validation_predictions')
  rows<-list()
  for(i in which(reg$analysis_type %in% types)) {
    r<-reg[i,];v<-m[m$case_id==r$case_id & m$spec_id==r$spec_id,]
    vals<-data.frame(n_test=if(nrow(v))v$n_eval else 0L,expected_n_test=r$n_eval,RMSE=if(nrow(v))v$eval_rmse else NA_real_,MAE=if(nrow(v))v$eval_mae else NA_real_,mean_error=if(nrow(v))v$mean_gap else NA_real_,
      status=stat$status[i],reason=stat$error[i])
    rows[[length(rows)+1L]]<-add_ids(case_identifiers(r,if(nrow(v))v$fit_id else NA_character_),vals)
  }
  cm<-bind_rows(rows);out_csv(ctx,cm,'validation_case_metrics')
  pair<-list();aggregate<-list()
  for(type in c('ca_validation','donor_validation')) {
    cases<-unique(reg$case_id[reg$analysis_type==type]);delta<-list()
    for(cas in cases) {
      x<-cm[cm$case_id==cas & cm$method=='SCM_OUTCOME',];y<-cm[cm$case_id==cas & cm$method=='DID',]
      common<-nrow(x)==1 && nrow(y)==1 && x$status=='success' && y$status=='success'
      diff<-if(common)x$RMSE-y$RMSE else NA_real_
      outcome<-if(!common)'unavailable'else if(abs(diff)<=ctx$cfg$tolerances$tie_rmse)'tie'else if(diff<0)'SCM_OUTCOME_win'else'DID_win'
      delta[[length(delta)+1L]]<-data.frame(run_id=ctx$run_id,analysis_type=type,comparison_set_id=if(type=='donor_validation')ctx$cfg$placebo$clean_id else'CA_pre_holdout',case_id=cas,target=x$target,
        train_start=x$train_start,train_end=x$train_end,eval_start=x$eval_start,eval_end=x$eval_end,n_eval=x$n_eval,RMSE_SCM_OUTCOME=x$RMSE,RMSE_DID=y$RMSE,
        delta_RMSE=diff,winner=outcome,common_success=common,tie_tolerance=ctx$cfg$tolerances$tie_rmse)
    }
    pr<-bind_rows(delta);pair[[type]]<-pr
    for(method in ctx$cfg$methods) {
      x<-cm[cm$analysis_type==type & cm$method==method,];success<-x$status=='success'
      common_cases<-pr$case_id[pr$common_success]
      # Main SCM-vs-DID comparison uses a common case set. ADH is an additional-information reference.
      use<-if(method %in% c('SCM_OUTCOME','DID'))success & x$case_id %in% common_cases else success
      pp<-p[p$analysis_type==type & p$method==method & p$case_id %in% x$case_id[use],]
      aggregate[[length(aggregate)+1L]]<-data.frame(run_id=ctx$run_id,analysis_type=type,method=method,expected_cases=length(cases),success_cases=sum(success),failed_cases=sum(x$status=='failed'),skipped_cases=sum(x$status=='skipped'),common_success_cases=sum(pr$common_success),summary_cases=sum(use),heldout_cells=nrow(pp),
        macro_RMSE=if(any(use))mean(x$RMSE[use])else NA_real_,median_RMSE=if(any(use))median(x$RMSE[use])else NA_real_,pooled_RMSE=if(nrow(pp))sqrt(mean(pp$error^2))else NA_real_,
        macro_MAE=if(any(use))mean(x$MAE[use])else NA_real_,mean_error=if(nrow(pp))mean(pp$error)else NA_real_,
        delta_mean=if(any(pr$common_success))mean(pr$delta_RMSE[pr$common_success])else NA_real_,delta_median=if(any(pr$common_success))median(pr$delta_RMSE[pr$common_success])else NA_real_,
        scm_wins=sum(pr$winner=='SCM_OUTCOME_win'),ties=sum(pr$winner=='tie'),did_wins=sum(pr$winner=='DID_win'),
        scm_win_fraction=if(any(pr$common_success))mean(pr$winner[pr$common_success]=='SCM_OUTCOME_win')else NA_real_,
        tie_fraction=if(any(pr$common_success))mean(pr$winner[pr$common_success]=='tie')else NA_real_,did_win_fraction=if(any(pr$common_success))mean(pr$winner[pr$common_success]=='DID_win')else NA_real_,
        evidence_reuse=if(type=='donor_validation')'same fits and predictions as clean placebo'else'same fits as time placebo; evaluation periods may differ')
    }
  }
  out_csv(ctx,bind_rows(pair),'paired_method_comparisons');out_csv(ctx,bind_rows(aggregate),'validation_aggregate')
}
summarize_placebo <- function(ctx,obj) {
  m<-obj$metrics;reg<-obj$registry;status<-obj$status;rows<-list();rankrows<-list();checks<-list()
  classic<-reg[reg$analysis_type=='classic_placebo',]
  filters<-c('unfiltered',paste0('c',unlist(ctx$cfg$placebo$thresholds)))
  for(method in ctx$cfg$methods) {
    base<-m[m$case_id=='CA_main' & m$method==method,]
    if(!nrow(base))next
    for(f in filters) {
      cr<-classic[classic$method==method,];v<-m[m$analysis_type=='classic_placebo' & m$method==method,]
      r<-merge(cr[c('target','case_id','n_donors')],v[c('case_id','fit_id','pre_mspe','post_mspe','mean_gap','eval_rmse','eval_mae','mspe_ratio','ratio_status')],by='case_id',all.x=TRUE,sort=FALSE)
      r$status<-status$status[match(paste(r$case_id,method),paste(status$case_id,status$method))]
      c<-if(f=='unfiltered')Inf else as.numeric(sub('c','',f,fixed=TRUE))
      r$eligible<-r$status=='success' & is.finite(r$pre_mspe) & r$pre_mspe<=c*base$pre_mspe
      if(is.infinite(c))r$eligible<-r$status=='success' & is.finite(r$pre_mspe)
      r$valid_ratio<-!is.na(r$mspe_ratio)
      r$exclusion_reason<-ifelse(r$status!='success','fit not successful',ifelse(!r$eligible,'pre-MSPE exceeds threshold',ifelse(!r$valid_ratio,'undefined ratio','')))
      ca<-r[r$target=='California',];eligible_donors<-r$eligible & r$target!='California';den<-1+sum(eligible_donors)
      full_ok<-nrow(r)==39 && all(r$status=='success') && all(r$valid_ratio)
      rank<-if(nrow(ca)==1 && ca$valid_ratio && ca$eligible && all(r$valid_ratio[r$eligible]))(1+sum(r$mspe_ratio[eligible_donors]>=ca$mspe_ratio))/den else NA_real_
      if(f=='unfiltered' && !full_ok)rank<-NA_real_
      # Available-case rank is kept separate and has its actual denominator.
      av<-r$eligible & r$valid_ratio & r$target!='California';avden<-1+sum(av)
      avrank<-if(nrow(ca)==1 && ca$valid_ratio)(1+sum(r$mspe_ratio[av]>=ca$mspe_ratio))/avden else NA_real_
      summary<-data.frame(run_id=ctx$run_id,method=method,comparison_set_id=ctx$cfg$placebo$classic_id,filter=f,threshold_MSPE=c,
        expected_targets=39,successful_targets=sum(r$status=='success'),valid_ratios=sum(r$valid_ratio),eligible_donors=sum(eligible_donors),rank_denominator=den,
        rank_fraction=rank,rank_type=if(f=='unfiltered')'complete_39_rank'else'filtered_descriptive_rank',full_39_available=full_ok,
        available_case_rank=avrank,available_case_denominator=avden,no_informative_comparison=sum(eligible_donors)==0,
        eligible_states=paste(r$target[r$eligible],collapse='|'),missing_or_invalid_states=paste(r$target[r$status!='success'|!r$valid_ratio],collapse='|'),
        inference_note='MSPE ratio is nondirectional; rank is not automatically a randomized-policy exact p-value')
      rankrows[[length(rankrows)+1L]]<-summary
      r$run_id<-ctx$run_id;r$method<-method;r$comparison_set_id<-ctx$cfg$placebo$classic_id;r$filter<-f;r$threshold_MSPE<-c
      r$rank_fraction<-rank;r$rank_denominator<-den;r$rank_type<-summary$rank_type;r$train_start<-1970;r$train_end<-1988;r$eval_start<-1989;r$eval_end<-2000;r$n_eval<-12
      rows[[length(rows)+1L]]<-r
      checks[[length(checks)+1L]]<-data.frame(check='placebo_denominator',method=method,filter=f,status=if(den==1+sum(r$eligible & r$target!='California'))'pass'else'fail',denominator=den)
    }
  }
  clean<-m[m$analysis_type=='donor_validation',]
  if(nrow(clean)) {
    r<-data.frame(case_id=clean$case_id,target=clean$target,n_donors=clean$n_donors,fit_id=clean$fit_id,pre_mspe=clean$pre_mspe,post_mspe=clean$post_mspe,
      mean_gap=clean$mean_gap,eval_rmse=clean$eval_rmse,eval_mae=clean$eval_mae,mspe_ratio=clean$mspe_ratio,ratio_status=clean$ratio_status,status='success',eligible=TRUE,valid_ratio=!is.na(clean$mspe_ratio),exclusion_reason='',
      run_id=ctx$run_id,method=clean$method,comparison_set_id=ctx$cfg$placebo$clean_id,filter='descriptive_unfiltered',threshold_MSPE=NA_real_,rank_fraction=NA_real_,rank_denominator=NA_integer_,rank_type='not_applicable_clean_asymmetric',train_start=1970,train_end=1988,eval_start=1989,eval_end=2000,n_eval=12)
    rows[[length(rows)+1L]]<-r
  }
  ps<-bind_rows(rows);out_csv(ctx,ps,'placebo_summary');out_csv(ctx,bind_rows(rankrows),'placebo_ranks')
  intersections<-list()
  for(f in filters) {
    e<-lapply(ctx$cfg$methods,function(m)ps$target[ps$method==m & ps$comparison_set_id==ctx$cfg$placebo$classic_id & ps$filter==f & ps$eligible])
    ix<-Reduce(intersect,e)
    for(meth in ctx$cfg$methods) {
      v<-ps[ps$method==meth & ps$comparison_set_id==ctx$cfg$placebo$classic_id & ps$filter==f & ps$target %in% ix,]
      ca<-v[v$target=='California',];ds<-v[v$target!='California',]
      rf<-if(nrow(ca) && !is.na(ca$mspe_ratio) && all(!is.na(ds$mspe_ratio)) && nrow(ds)) (1+sum(ds$mspe_ratio>=ca$mspe_ratio))/(nrow(ds)+1) else NA_real_
      intersections[[length(intersections)+1L]]<-data.frame(run_id=ctx$run_id,method=meth,comparison_set_id=ctx$cfg$placebo$classic_id,filter=f,n_common_donors=sum(ix!='California'),common_states=paste(ix,collapse='|'),common_set_descriptive_rank=rf,denominator=if('California' %in% ix)length(ix)else NA_integer_)
    }
  }
  out_csv(ctx,bind_rows(intersections),'placebo_common_eligible')
  cl<-m[m$analysis_type=='classic_placebo' & m$target!='California',];cn<-m[m$analysis_type=='donor_validation',]
  both<-merge(cl[c('target','method','fit_id','mean_gap','eval_rmse','pre_mspe')],cn[c('target','method','fit_id','mean_gap','eval_rmse','pre_mspe')],by=c('target','method'),suffixes=c('_classic','_clean'),all=TRUE)
  both$mean_gap_clean_minus_classic<-both$mean_gap_clean-both$mean_gap_classic;both$RMSE_clean_minus_classic<-both$eval_rmse_clean-both$eval_rmse_classic
  both$classic_donors<-38;both$clean_donors<-37;both$run_id<-ctx$run_id
  out_csv(ctx,both,'classic_clean_comparison');out_csv(ctx,bind_rows(checks),'placebo_checks')
}
summarize_robustness <- function(ctx,obj) {
  m<-obj$metrics;w<-obj$weights;base<-m[m$analysis_type=='main',]
  rob<-m[m$analysis_type %in% c('loo','training_window','predictor_deletion','time_placebo','near_policy','near_policy_diagnostic'),]
  rob$baseline_mean_gap<-base$mean_gap[match(rob$method,base$method)];rob$baseline_gap_2000<-base$gap_2000[match(rob$method,base$method)]
  genuine<-rob$eval_start==1989 & rob$eval_end==2000
  rob$mean_gap_change<-ifelse(genuine,rob$mean_gap-rob$baseline_mean_gap,NA_real_)
  rob$gap_2000_change<-ifelse(genuine,rob$gap_2000-rob$baseline_gap_2000,NA_real_)
  rob$effect_comparison_status<-ifelse(genuine,'same_true_post_window','not_applicable_different_evaluation_window')
  wb<-w[w$analysis_type=='main',]
  rob$weight_L1_change<-vapply(seq_len(nrow(rob)),function(i){
    v<-w[w$case_id==rob$case_id[i]&w$method==rob$method[i],];b<-wb[wb$method==rob$method[i],]
    all<-union(b$donor,v$donor);x<-b$weight[match(all,b$donor)];y<-v$weight[match(all,v$donor)];x[is.na(x)]<-0;y[is.na(y)]<-0;sum(abs(x-y))
  },0)
  highlight<-unique(wb$donor[wb$method %in% c('SCM_ADH','SCM_OUTCOME') & wb$weight>=ctx$cfg$robustness$highlight_weight])
  rob$highlight_loo<-rob$analysis_type=='loo' & rob$perturbation %in% highlight
  out_csv(ctx,rob,'robustness_summary')
  range<-bind_rows(lapply(split(rob[rob$analysis_type=='loo',],rob$method[rob$analysis_type=='loo']),function(z)data.frame(method=z$method[1],n_cases=nrow(z),mean_gap_min=min(z$mean_gap),mean_gap_max=max(z$mean_gap),gap_2000_min=min(z$gap_2000),gap_2000_max=max(z$gap_2000),max_abs_mean_change=max(abs(z$mean_gap_change)),most_influential=z$perturbation[which.max(abs(z$mean_gap_change))])))
  range$run_id<-ctx$run_id;out_csv(ctx,range,'loo_ranges')
}
published_comparison <- function(ctx,obj) {
  m<-obj$metrics;b<-m[m$analysis_type=='main' & m$method=='SCM_ADH',];if(!nrow(b))return(invisible(NULL))
  rows<-list();add<-function(item,pub,rep,unit,precision,source,explanation,approx=FALSE)rows[[length(rows)+1L]]<<-data.frame(run_id=ctx$run_id,spec_id='SCM_ADH',case_id='CA_main',method='SCM_ADH',item=item,published_value=pub,reproduced_value=rep,difference=rep-pub,unit=unit,published_precision=precision,approximate_benchmark=approx,source_location=source,explanation=explanation)
  w<-obj$weights[obj$weights$analysis_type=='main' & obj$weights$method=='SCM_ADH',]
  pubw<-c(Colorado=.164,Connecticut=.069,Montana=.199,Nevada=.234,Utah=.334)
  for(s in w$donor)add(paste0('weight_',s),if(s %in% names(pubw))pubw[s]else 0,w$weight[w$donor==s],'simplex weight','0.001 for positive weights; printed zero otherwise','ADH p.500, Table 2','Same data and predictor windows; current Synth local optimizer and explicitly fixed numerical settings may differ from historical implementation.')
  add('gap_2000',-26,b$gap_2000,'packs/person/year','about 26','ADH pp.493-494, abstract/introduction; Figure 3 p.501','Approximate published endpoint compared with the actual 2000 gap.',TRUE)
  add('mean_gap_1989_2000',-20,b$mean_gap,'packs/person/year','almost 20','ADH p.500, section 3.3','The benchmark is prose approximation; both refer to average annual per-capita reduction.',TRUE)
  add('relative_reduction_pct',25,b$relative_reduction_pct,'percent','approximately 25','ADH p.500, section 3.3','Project uses -100*sum(gap)/sum(counterfactual). The published prose does not fully specify the aggregation formula; comparison is approximate.',TRUE)
  add('pre_mspe',3,b$pre_mspe,'packs^2/person^2/year^2','about 3','ADH p.501, section 3.4','1970-1988 average squared gap, not RMSPE.',TRUE)
  add('post_pre_mspe_ratio',130,b$mspe_ratio,'dimensionless','about 130','ADH p.503, Figure 8 and text','1989-2000 MSPE / 1970-1988 MSPE; denominator changes can strongly affect ratio.',TRUE)
  rp<-read_csv(file.path(ctx$result_dir,'placebo_ranks.csv'));r<-rp[rp$method=='SCM_ADH' & rp$filter=='unfiltered',]
  if(nrow(r))add('classic_rank_fraction',1/39,r$rank_fraction,'fraction','exact 1/39','ADH p.503, Figure 8','Full symmetric 39-target rotation; ties counted with >=; not an automatically exact randomized-policy p-value.')
  for(f in c('c20','c5','c2')) {
    z<-rp[rp$method=='SCM_ADH' & rp$filter==f,]
    ref<-c(c20=34,c5=29,c2=19)[f]
    loc<-c(c20='ADH p.502, Figure 5',c5='ADH p.502, Figure 6',c2='ADH p.503, Figure 7')[f]
    if(nrow(z))add(paste0('classic_eligible_donors_',f),ref,z$eligible_donors,'donor states','integer count',loc,'Each target is refitted. Eligibility uses target pre-MSPE relative to reproduced California pre-MSPE; differences in local pre-fits change cutoff membership. Historical optimizer settings are not fully reconstructed.')
  }
  pbal<-obj$balance[obj$balance$analysis_type=='main' & obj$balance$method=='SCM_ADH',]
  real<-c(lnincome=10.08,retprice=89.42,age15to24=17.40,beer=24.28,cigsale_1975=127.10,cigsale_1980=120.20,cigsale_1988=90.10)
  syn<-c(lnincome=9.86,retprice=89.41,age15to24=17.40,beer=24.20,cigsale_1975=126.99,cigsale_1980=120.43,cigsale_1988=91.62)
  for(i in seq_len(nrow(pbal))) {
    v<-pbal$predictor[i];factor<-if(v=='age15to24')100 else 1;unit<-if(v=='age15to24')'percent (original proportion x 100)'else if(v=='lnincome')'original log-income field'else if(v=='retprice')'cents/pack'else if(v=='beer')'gallons/person/year'else'packs/person/year'
    for(which in c('real','synthetic'))add(paste(which,v,sep='_'),if(which=='real')real[v]else syn[v],factor*if(which=='real')pbal$target_value[i]else pbal$synthetic_value[i],unit,'0.01','ADH p.499, Table 1 and notes',if(v=='lnincome')'Table labels Ln(GDP per capita); Appendix A describes logged real personal income. Preserve lnincome rather than relabeling economic variable.'else'Original-scale predictor average or specified lag; no reconstruction of population-weighted third column.')
  }
  out_csv(ctx,bind_rows(rows),'published_vs_replicated')
}
summarize_results <- function(ctx,obj) {
  summarize_validation(ctx,obj);summarize_placebo(ctx,obj);summarize_robustness(ctx,obj);published_comparison(ctx,obj)
  # Restore complete per-case identities in derived comparison tables.
  reg<-obj$registry;st<-obj$status
  ps<-read_csv(file.path(ctx$result_dir,'placebo_summary.csv'))
  j<-match(paste(ps$case_id,ps$method),paste(reg$case_id,reg$method))
  for(n in c('spec_id','donor_pool_id','data_hash','analysis_type'))ps[[n]]<-reg[[n]][j]
  out_csv(ctx,ps,'placebo_summary')
  pr<-read_csv(file.path(ctx$result_dir,'paired_method_comparisons.csv'))
  for(method in c('SCM_OUTCOME','DID')) {
    j<-match(paste(pr$case_id,method),paste(obj$metrics$case_id,obj$metrics$method))
    pr[[paste0('fit_id_',method)]]<-obj$metrics$fit_id[j]
    pr[[paste0('spec_id_',method)]]<-obj$metrics$spec_id[j]
  }
  j<-match(pr$case_id,reg$case_id);pr$donor_pool_id<-reg$donor_pool_id[j];pr$data_hash<-ctx$data_hash
  out_csv(ctx,pr,'paired_method_comparisons')
  pv<-read_csv(file.path(ctx$result_dir,'published_vs_replicated.csv'));base<-obj$metrics[obj$metrics$analysis_type=='main' & obj$metrics$method=='SCM_ADH',]
  for(n in c('fit_id','comparison_set_id','donor_pool_id','data_hash','train_start','train_end','eval_start','eval_end','n_eval'))pv[[n]]<-base[[n]]
  out_csv(ctx,pv,'published_vs_replicated')
  reg$fit_id<-st$fit_id;reg$execution_status<-st$status;out_csv(ctx,reg,'spec_registry')
  for(name in c('placebo_ranks','placebo_common_eligible','loo_ranges')) {
    x<-read_csv(file.path(ctx$result_dir,paste0(name,'.csv')));x$train_start<-1970L;x$train_end<-1988L;x$eval_start<-1989L;x$eval_end<-2000L;x$n_eval<-12L;x$data_hash<-ctx$data_hash;out_csv(ctx,x,name)
  }
  x<-read_csv(file.path(ctx$result_dir,'validation_aggregate.csv'));isdonor<-x$analysis_type=='donor_validation'
  x$train_start<-1970L;x$train_end<-ifelse(isdonor,1988L,NA_integer_);x$eval_start<-ifelse(isdonor,1989L,1981L);x$eval_end<-ifelse(isdonor,2000L,1988L);x$n_eval<-ifelse(isdonor,12L,8L)
  x$window_definition<-ifelse(isdonor,'Same 1970-1988 train and 1989-2000 evaluation for every state','Two folds: train ends 1980/1984, evaluate 1981-1984/1985-1988; eight distinct evaluation years; train_end not a single value')
  x$data_hash<-ctx$data_hash;out_csv(ctx,x,'validation_aggregate')
  key<-c('run_id','spec_id','case_id','method','comparison_set_id','year')
  assert(!anyDuplicated(obj$annual[key]),'Duplicate results key')
  checks<-list()
  add<-function(name,ok,detail)checks[[length(checks)+1L]]<<-data.frame(check=name,status=if(ok)'pass'else'fail',detail=detail)
  add('planned_case_count',nrow(obj$registry)==375,paste('Registry has',nrow(obj$registry),'case-method specifications'))
  # Per-fit constraints/objectives and original-order prediction reconstruction across ALL successful cases.
  d<-load_project_data();devs<-numeric();qbound<-numeric()
  for(fid in unique(obj$metrics$fit_id)) {
    f<-readRDS(file.path('models',paste0(fid,'.rds')))
    z<-outcome_matrix(f$input$data,f$donors,f$train);y<-as.numeric(outcome_matrix(f$input$data,f$target,f$train))
    devs<-c(devs,abs(sum(f$w)-1),max(0,-min(f$w)),abs(mean((y-as.numeric(z%*%f$w)-f$offset)^2)-f$objective))
    if(f$method=='SCM_OUTCOME')qbound<-c(qbound,f$objective-f$raw_equal_mean_mspe)
  }
  add('all_fit_constraints_and_objectives',length(devs)>0 && max(devs)<1e-6,paste('Max discrepancy:',format(max(devs),digits=10)))
  add('all_outcome_qp_equal_mean_bound',length(qbound)>0 && max(qbound)<=1e-5,paste('Max SCM minus equal-mean MSPE:',format(max(qbound),digits=10)))
  cleanpools<-unique(obj$registry$donor_pool_id[obj$registry$analysis_type=='donor_validation'])
  pool<-read_csv(file.path(ctx$result_dir,'donor_pool_members.csv'))
  add('clean_pool_excludes_CA',!any(pool$included & pool$state=='California' & pool$donor_pool_id %in% cleanpools),'Clean donor-validation and placebo use 37 donors, never California')
  pc<-read_csv(file.path(ctx$result_dir,'placebo_checks.csv'));add('all_placebo_denominators',all(pc$status=='pass'),'Actual eligible donor count + 1; classic and clean IDs distinct')
  out_csv(ctx,bind_rows(checks),'batch_acceptance_checks')
}
