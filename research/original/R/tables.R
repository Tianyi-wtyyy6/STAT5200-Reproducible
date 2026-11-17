# CSV-only display tables; estimation code is unchanged.
fmt <- function(x,d=2) ifelse(is.na(x),'NA',ifelse(is.infinite(x),'Inf',formatC(x,format='f',digits=d)))
export_display_tables <- function(ctx) {
  rd<-ctx$result_dir
  get<-function(name)read_csv(file.path(rd,paste0(name,'.csv')))
  table_index<-list()
  table_tex<-function(df,id,caption='',...,source='') {
    out_csv(ctx,df,id)
    table_index[[length(table_index)+1L]]<<-data.frame(run_id=ctx$run_id,item_id=id,type='table',
      pdf_path=NA_character_,png_path=NA_character_,csv_path=file.path(ctx$result_dir,paste0(id,'.csv')),
      input_tables=source,filter_conditions='',comparison_set_id='',scope=if(grepl('^T[1-4]',id))'main'else'supplement',
      purpose=caption)
  }
  m<-get('effect_summary');main<-m[m$analysis_type=='main',];reg<-get('spec_registry');st<-get('run_status');w<-get('donor_weights');bal<-get('predictor_balance');V<-get('predictor_weights')
  va<-get('validation_aggregate');vc<-get('validation_case_metrics');pair<-get('paired_method_comparisons');ranks<-get('placebo_ranks');rob<-get('robustness_summary');pub<-get('published_vs_replicated');ps<-get('placebo_summary')
  diag<-get('optimization_diagnostics');cc<-get('classic_clean_comparison');lr<-get('loo_ranges');tw<-get('twfe_equivalence');audit<-get('data_audit');ds<-get('descriptive_statistics')
  a<-main[main$method=='SCM_ADH',];o<-main[main$method=='SCM_OUTCOME',];di<-main[main$method=='DID',]
  assert(nrow(a)==1&&nrow(o)==1&&nrow(di)==1,'Report requires all main fits')
  donor<-va[va$analysis_type=='donor_validation',];outd<-donor[donor$method=='SCM_OUTCOME',];didd<-donor[donor$method=='DID',]
  t1<-ds[ds$sample_definition=='all / all_years',]
  t1<-t1[match(c('cigsale','lnincome','retprice','age15to24','beer'),t1$variable),]
  table_tex(data.frame(Variable=t1$variable,Observed=t1$n,Missing=t1$n_missing,Mean=fmt(t1$mean),SD=fmt(t1$sd),Min=fmt(t1$min),Max=fmt(t1$max)),
    'T1_data','Data audit and full-panel observed-cell descriptive statistics',note='39 states x 31 years (1970-2000), 1209 unique state-years. Age is a proportion; cigsale is packs/person/year; price cents/pack; beer gallons/person/year; income is the original logged field. No rows are deleted. CA and donor, pre and post summaries appear in descriptive_statistics.csv.',source='descriptive_statistics.csv;data_audit.csv')
  items<-c(paste0('weight_',c('Colorado','Connecticut','Montana','Nevada','Utah')),'pre_mspe','mean_gap_1989_2000','gap_2000','relative_reduction_pct','post_pre_mspe_ratio','classic_rank_fraction')
  x<-pub[match(items,pub$item),];labels<-c('Colorado weight','Connecticut weight','Montana weight','Nevada weight','Utah weight','Pre-MSPE','Mean gap, 1989-2000','Gap, 2000','Relative reduction (%)','Post/pre MSPE ratio','Classic rank fraction')
  table_tex(data.frame(Quantity=labels,Published=fmt(x$published_value,3),Reproduced=fmt(x$reproduced_value,3),Difference=fmt(x$difference,3)),
    'T2_replication','Published benchmarks and reproduced SCM_ADH estimates',align='lrrr',note='Weights: ADH p.500, Table 2 (printed to 0.001); endpoint: pp.493-494, about 26 packs; mean and percent: p.500, almost 20 packs and approximately 25%; pre-MSPE: p.501, about 3; ratio and rank: p.503, Figure 8. Approximate prose benchmarks are not exact test targets. Full units, precision, Table 1 balance, and explanations are in published_vs_replicated.csv.',source='published_vs_replicated.csv')
  table_tex(data.frame(Method=main$method,`Pre RMSPE`=fmt(main$pre_rmspe),`Mean gap`=fmt(main$mean_gap),`2000 gap`=fmt(main$gap_2000),`Reduction %`=fmt(main$relative_reduction_pct),H=fmt(main$H,3),`1/H`=fmt(main$effective_donors),Offset=fmt(main$offset),check.names=FALSE),
    'T3_methods','Main California fit, effects, and weight concentration',size='footnotesize',note='Train 1970-1988; evaluate 1989-2000; 38 donors. Gaps and RMSPE are packs/person/year. Reduction is minus summed gaps divided by summed counterfactual sales. DID fixes equal weights and adds the displayed training offset; SCM has no intercept. Training error does not measure held-out error.',source='effect_summary.csv')
  t4<-va
  table_tex(data.frame(Sample=ifelse(t4$analysis_type=='ca_validation','CA blocks','38 donors'),Method=t4$method,`OK/planned`=paste0(t4$success_cases,'/',t4$expected_cases),`Fail/skip`=paste0(t4$failed_cases,'/',t4$skipped_cases),Macro=fmt(t4$macro_RMSE),Median=fmt(t4$median_RMSE),Pooled=fmt(t4$pooled_RMSE),check.names=FALSE),
    'T4_validation','Held-out validation: success counts and RMSE summaries',size='footnotesize',note=paste0('RMSE units: packs/person/year. CA blocks contain eight held-out years; ADH is not applicable. Donor main comparison uses ',outd$common_success_cases,' common successes. SCM_OUTCOME versus DID wins/ties/losses: ',outd$scm_wins,'/',outd$ties,'/',outd$did_wins,'. Macro is mean case RMSE; pooled weights held-out cells equally. No cases are removed for poor performance.'),source='validation_aggregate.csv')
  unique_diag<-diag[!duplicated(diag$fit_id),];adh_diag<-unique_diag[unique_diag$method=='SCM_ADH',]
  limit<-grepl('convcode [^0]',adh_diag$solver_status);retries<-sum(unique_diag$retries)
  # Numeric diagnostics do not choose a different fitted solution.
  qp_rows<-lapply(unique_diag$fit_id[unique_diag$method=='SCM_OUTCOME'],function(fid){f<-readRDS(file.path('models',paste0(fid,'.rds')));z<-outcome_matrix(f$input$data,f$donors,f$train);active<-f$w>1e-7
    data.frame(fit_id=fid,target=f$target,n_train=length(f$train),n_donors=length(f$w),hessian_rank=f$hessian_rank,hessian_dimension=f$hessian_dimension,active_weights=sum(active),active_augmented_rank=as.integer(Matrix::rankMatrix(rbind(z[,active,drop=FALSE],1))),
      raw_min_weight=min(f$raw_w),raw_sum_residual=abs(sum(f$raw_w)-1),objective=f$objective,raw_objective=f$raw_objective,cleanup_objective_change=f$objective-f$raw_objective,status=f$solver_status)})
  qp<-bind_rows(qp_rows);out_csv(ctx,qp,'qp_numeric_stability')
  dr<-rob[rob$analysis_type=='predictor_deletion',]
  # Supplementary tables retain every prespecified case, not selected best cases.
  table_tex(audit[c('check','actual','status')],'S_data_audit','Actual data-contract checks',TRUE,align='p{3.1cm}p{10.2cm}l',size='scriptsize',source='data_audit.csv')
  ac<-get('acceptance_checks');table_tex(ac[c('check','method','status','cache_disabled_recomputed')],'S_acceptance','Implementation and actual no-cache refit checks',TRUE,align='p{6cm}lcc',size='scriptsize',source='acceptance_checks.csv')
  table_tex(get('batch_acceptance_checks')[c('check','status','detail')],'S_batch_checks','Batch checks across all successful fits',TRUE,align='p{5.1cm}lp{7cm}',size='scriptsize',source='batch_acceptance_checks.csv')
  table_tex(data.frame(Package=names(ctx$versions),Version=unlist(ctx$versions)),'S_software',paste('Actual software; R',getRversion()),TRUE,align='ll',source='run_manifest.json;renv.lock')
  ss<-as.data.frame(table(st$analysis_type,st$status));ss<-ss[ss$Freq>0,];names(ss)<-c('Analysis','Status','Count');table_tex(ss,'S_status_summary','Case status counts by registered analysis',TRUE,align='llr',source='run_status.csv')
  table_tex(data.frame(Method=main$method,`1989-1992`=fmt(main$phase_1989_1992),`1993-1996`=fmt(main$phase_1993_1996),`1997-2000`=fmt(main$phase_1997_2000),`Pre mean`=fmt(main$pre_mean_gap,3),`Pre slope`=fmt(main$pre_gap_slope,3),check.names=FALSE),'S_dynamic','Dynamic phase gaps and pre-period diagnostics',TRUE,size='scriptsize',source='effect_summary.csv')
  wb<-w[w$analysis_type=='main',];wide<-tidyr::pivot_wider(wb[c('donor','method','weight')],names_from='method',values_from='weight');wide[-1]<-lapply(wide[-1],fmt,d=6)
  table_tex(wide,'S_weights','All 38 baseline donor weights including numerical zeros',TRUE,size='scriptsize',align='lrrr',source='donor_weights.csv')
  bb<-bal[bal$analysis_type=='main',];table_tex(data.frame(Predictor=bb$predictor,Target=fmt(bb$target_value,5),Synthetic=fmt(bb$synthetic_value,5),`Equal donor mean`=fmt(bb$equal_weight_donor_mean,5),Gap=fmt(bb$gap,5),check.names=FALSE),'S_balance','Baseline ADH balance in original units; age is a proportion',TRUE,size='scriptsize',source='predictor_balance.csv')
  vv<-V[V$analysis_type=='main',];table_tex(data.frame(Predictor=vv$predictor,V=fmt(vv$V,7),SD=fmt(vv$scale_sd,5),`V / SD squared`=fmt(vv$original_scale_coefficient,7),check.names=FALSE),'S_V','Predictor weights and internal scaling',TRUE,size='scriptsize',note='Sample SD is across target plus donors; V is not causal importance.',source='predictor_weights.csv')
  table_tex(data.frame(Item=pub$item,Published=fmt(pub$published_value,4),Reproduced=fmt(pub$reproduced_value,4),Difference=fmt(pub$difference,4),Approx=pub$approximate_benchmark),'S_published','Complete published-versus-reproduced quantities',TRUE,size='scriptsize',align='p{6cm}rrrr',source='published_vs_replicated.csv')
  metric_table<-function(z,id,caption) {
    table_tex(data.frame(Case=z$case_id,Method=z$method,`Pre RMSPE`=fmt(z$pre_rmspe),`Mean gap`=fmt(z$mean_gap),`2000 gap`=fmt(z$gap_2000),`Post gap RMS`=fmt(z$eval_gap_rms),H=fmt(z$H,3),`1/H`=fmt(z$effective_donors),check.names=FALSE),id,caption,TRUE,size='scriptsize',align='p{4.1cm}lrrrrrr',note='The true California post-policy gap RMS is a deviation statistic, not an observed counterfactual prediction error.',source='effect_summary.csv;robustness_summary.csv')
  }
  cval<-vc[vc$analysis_type=='ca_validation',];table_tex(data.frame(Case=cval$case_id,Method=cval$method,RMSE=fmt(cval$RMSE),MAE=fmt(cval$MAE),`Mean error`=fmt(cval$mean_error),Status=cval$status,check.names=FALSE),'S_CA_validation','All California pre-policy folds',TRUE,size='scriptsize',source='validation_case_metrics.csv')
  dval<-vc[vc$analysis_type=='donor_validation',];table_tex(data.frame(State=dval$target,Method=dval$method,RMSE=fmt(dval$RMSE),MAE=fmt(dval$MAE),`Mean error`=fmt(dval$mean_error),Status=dval$status,check.names=FALSE),'S_donor_validation','Every donor-state held-out validation case; no California',TRUE,size='scriptsize',source='validation_case_metrics.csv')
  dp<-pair[pair$analysis_type=='donor_validation',];table_tex(data.frame(State=dp$target,SCM=fmt(dp$RMSE_SCM_OUTCOME),DID=fmt(dp$RMSE_DID),Delta=fmt(dp$delta_RMSE),Winner=dp$winner),'S_paired','All paired donor RMSE comparisons',TRUE,size='scriptsize',source='paired_method_comparisons.csv')
  table_tex(data.frame(Method=ranks$method,Filter=ranks$filter,`Eligible donors`=ranks$eligible_donors,Denominator=ranks$rank_denominator,Rank=fmt(ranks$rank_fraction,4),`Full set valid`=ranks$full_39_available,check.names=FALSE),'S_ranks','Classic complete and filtered descriptive ranks',TRUE,size='scriptsize',source='placebo_ranks.csv')
  ix<-get('placebo_common_eligible');table_tex(data.frame(Method=ix$method,Filter=ix$filter,`Common donors`=ix$n_common_donors,Denominator=ix$denominator,Rank=fmt(ix$common_set_descriptive_rank,4),check.names=FALSE),'S_intersections','Cross-method common eligible intersections',TRUE,size='scriptsize',note='State lists and all exclusion reasons remain in placebo_common_eligible.csv and placebo_summary.csv.',source='placebo_common_eligible.csv')
  pl<-ps[ps$comparison_set_id==ctx$cfg$placebo$classic_id & ps$filter=='unfiltered',]
  elig<-function(target,method,filter){z<-ps[ps$target==target & ps$method==method & ps$filter==filter & ps$comparison_set_id==ctx$cfg$placebo$classic_id,];if(nrow(z))as.character(z$eligible)else'NA'}
  table_tex(data.frame(State=pl$target,Method=pl$method,`Pre MSPE`=fmt(pl$pre_mspe),Ratio=fmt(pl$mspe_ratio),c20=mapply(elig,pl$target,pl$method,MoreArgs=list(filter='c20')),c5=mapply(elig,pl$target,pl$method,MoreArgs=list(filter='c5')),c2=mapply(elig,pl$target,pl$method,MoreArgs=list(filter='c2')),check.names=FALSE),'S_placebo','Every classic target, ratio, and eligibility threshold',TRUE,size='scriptsize',source='placebo_summary.csv')
  cl<-m[m$analysis_type=='donor_validation',];table_tex(data.frame(State=cl$target,Method=cl$method,`Pre MSPE`=fmt(cl$pre_mspe),`Mean gap`=fmt(cl$mean_gap),RMSE=fmt(cl$eval_rmse),MAE=fmt(cl$eval_mae),check.names=FALSE),'S_clean','Clean descriptive pseudo-effects and prediction errors (same donor-validation fits)',TRUE,size='scriptsize',source='effect_summary.csv')
  metric_table(rob[rob$analysis_type=='loo',],'S_LOO','All 38 donor removals for all three specifications')
  win<-rob[rob$analysis_type=='training_window',];table_tex(data.frame(Train=paste0(win$train_start,'-',win$train_end),Method=win$method,`Own RMSPE`=fmt(win$pre_rmspe),`1980-88 RMSPE`=fmt(win$common_1980_1988_rmspe),`Mean gap`=fmt(win$mean_gap),`2000 gap`=fmt(win$gap_2000),check.names=FALSE),'S_windows','Training windows: own and common-window errors',TRUE,size='scriptsize',source='robustness_summary.csv')
  metric_table(dr,'S_predictors','All seven original-predictor deletion variants')
  tim<-rob[rob$analysis_type %in% c('time_placebo','near_policy','near_policy_diagnostic'),];table_tex(data.frame(Case=tim$case_id,Method=tim$method,Train=paste0(tim$train_start,'-',tim$train_end),Eval=paste0(tim$eval_start,'-',tim$eval_end),n=tim$n_eval,Mean=fmt(tim$mean_gap),`Gap RMS`=fmt(tim$eval_gap_rms),`Mean |gap|`=fmt(tim$eval_mean_abs_gap),check.names=FALSE),'S_timing','Actual training and evaluation roles for every timing exercise',TRUE,size='scriptsize',align='p{3.2cm}l llrrrr',note='For pre-policy holdouts, gap RMS and mean absolute gap are prediction RMSE and MAE. For true California 1989-2000 they are policy-gap deviation summaries, not observed errors in untreated potential outcomes.',source='robustness_summary.csv')
  table_tex(data.frame(Case=reg$case_id,Method=reg$method,Train=paste0(reg$train_start,'-',reg$train_end),Eval=paste0(reg$eval_start,'-',reg$eval_end),J=reg$n_donors,Status=st$status),'S_registry','Complete registered case-method inventory',TRUE,size='scriptsize',align='p{4.2cm}lllrl',source='spec_registry.csv;run_status.csv')
  table_tex(data.frame(Fit=substr(unique_diag$fit_id,5,16),Target=unique_diag$target,Method=unique_diag$method,Status=unique_diag$solver_status,Retry=unique_diag$retries),'S_optimizer','Unique fitted-object optimizer statuses; full hashes and logs are machine readable',TRUE,size='scriptsize',align='lp{3.3cm}lp{4.2cm}r',source='optimization_diagnostics.csv')
  table_tex(data.frame(Fit=substr(qp$fit_id,5,16),Target=qp$target,n=qp$n_train,J=qp$n_donors,Rank=qp$hessian_rank,Active=qp$active_weights,`Active rank`=qp$active_augmented_rank,Loss=fmt(qp$objective,4),check.names=FALSE),'S_qp','PSD outcome-QP ranks and active supports; no ridge',TRUE,size='scriptsize',source='qp_numeric_stability.csv')

  fi<-if(file.exists(file.path(rd,'figure_table_index.csv')))get('figure_table_index')else data.frame()
  if(nrow(fi))fi<-fi[fi$type=='figure',]
  out_csv(ctx,bind_rows(fi,bind_rows(table_index)),'figure_table_index')
}
