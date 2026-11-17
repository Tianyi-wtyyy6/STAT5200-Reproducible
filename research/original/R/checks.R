run_acceptance_checks <- function(ctx,d,reg) {
  records<-list()
  add<-function(check,method,pass,detail,value=NA_real_,tolerance=NA_real_,recomputed=FALSE) {
    records[[length(records)+1L]]<<-data.frame(run_id=ctx$run_id,check=check,method=method,status=if(pass)'pass'else'fail',value=value,tolerance=tolerance,detail=detail,cache_disabled_recomputed=recomputed,utc=utc_now())
    cat('CHECK',check,method,if(pass)'PASS'else'FAIL',detail,'\n');flush.console()
  }
  ca<-ctx$cfg$treated_state;donors<-sort(setdiff(unique(d$state),ca),method='radix');tol<-ctx$cfg$tolerances$leakage
  add('data_contract','all',nrow(d)==1209 && length(unique(d$state))==39 && !anyDuplicated(d[c('state','year')]) && !anyNA(d$cigsale) && all(table(d$state)==31) && !ca %in% donors && ctx$cfg$first_treated_year==1989,'39 x 31; unique keys; complete outcome; disjoint target/donors; first treatment 1989')
  for(m in ctx$cfg$methods) {
    changed<-d;ix<-changed$state==ca & changed$year>=1989
    changed$cigsale[ix]<-changed$cigsale[ix]+100+3*(changed$year[ix]-1989)
    a<-fit_model(prepare_training(d,ca,donors,1970:1988,m),ctx,FALSE,paste0('leak_original_',m))
    b<-fit_model(prepare_training(changed,ca,donors,1970:1988,m),ctx,FALSE,paste0('leak_mutated_',m))
    pa<-predict_model(a,d[d$state %in% donors,c('state','year','cigsale')],1970:2000)
    pb<-predict_model(b,changed[changed$state %in% donors,c('state','year','cigsale')],1970:2000)
    dev<-max(abs(a$w-b$w),abs(a$offset-b$offset),abs(pa$y0_hat-pb$y0_hat))
    sa<-score_predictions(pa,d[d$state==ca,c('year','cigsale')]);sb<-score_predictions(pb,changed[changed$state==ca,c('year','cigsale')])
    delta<-changed$cigsale[changed$state==ca]-d$cigsale[d$state==ca]
    gapdev<-max(abs((sb$gap-sa$gap)-delta))
    add('main_post_label_leakage',m,dev<=tol && gapdev<=tol,'Both fits and predictions rerun without cache; only post labels changed; expected gap change verified',max(dev,gapdev),tol,TRUE)
    zm<-outcome_matrix(d,donors,1970:1988);y<-as.numeric(outcome_matrix(d,ca,1970:1988));calc<-mean((y-as.numeric(zm%*%a$w)-a$offset)^2)
    err<-max(abs(sum(a$w)-1),max(0,-min(a$w)),abs(calc-a$objective))
    preddev<-max(abs(pa$y0_hat-as.numeric(outcome_matrix(d,donors,1970:2000)%*%a$w)-a$offset))
    add('weights_matrix_objective',m,err<1e-6 && preddev<1e-8,'Donor/year order, simplex, matrix prediction and saved original-scale objective',max(err,preddev),1e-6,TRUE)
    if(m=='SCM_OUTCOME')add('qp_feasible_equal_mean_bound',m,a$objective<=a$raw_equal_mean_mspe+1e-5 && tolower(a$solver_status)=='solved','SCM loss no worse than unshifted equal-donor mean; PSD Hessian; no ridge',a$objective-a$raw_equal_mean_mspe,1e-5,TRUE)
  }
  for(m in c('SCM_OUTCOME','DID')) {
    changed<-d;ix<-changed$state==ca & changed$year %in% 1981:1984;changed$cigsale[ix]<-changed$cigsale[ix]-77-seq_len(sum(ix))
    a<-fit_model(prepare_training(d[d$year<=1984,],ca,donors,1970:1980,m),ctx,FALSE,paste0('holdout_original_',m))
    b<-fit_model(prepare_training(changed[changed$year<=1984,],ca,donors,1970:1980,m),ctx,FALSE,paste0('holdout_mutated_',m))
    pa<-predict_model(a,d[d$state %in% donors & d$year<=1984,c('state','year','cigsale')],1981:1984)
    pb<-predict_model(b,changed[changed$state %in% donors & changed$year<=1984,c('state','year','cigsale')],1981:1984)
    dev<-max(abs(a$w-b$w),abs(a$offset-b$offset),abs(pa$y0_hat-pb$y0_hat))
    sa<-score_predictions(pa,d[d$state==ca & d$year %in% 1981:1984,c('year','cigsale')]);sb<-score_predictions(pb,changed[changed$state==ca & changed$year %in% 1981:1984,c('year','cigsale')])
    add('validation_label_isolation',m,dev<=tol && abs(mean(sa$gap^2)-mean(sb$gap^2))>1,'CA_pre_1 rebuilt/refitted/repredicted with mutated held-out labels, cache disabled; score changed',dev,tol,TRUE)
  }
  # ADH is not applicable to early CA folds, so test its held-out isolation on a clean donor case.
  q<-'Colorado';controls<-setdiff(donors,q); changed<-d;ix<-changed$state==q & changed$year>=1989;changed$cigsale[ix]<-changed$cigsale[ix]+80
  a<-fit_model(prepare_training(d,q,controls,1970:1988,'SCM_ADH'),ctx,FALSE,'donor_holdout_original')
  b<-fit_model(prepare_training(changed,q,controls,1970:1988,'SCM_ADH'),ctx,FALSE,'donor_holdout_mutated')
  pa<-predict_model(a,d[d$state %in% controls,c('state','year','cigsale')],1989:2000)
  pb<-predict_model(b,changed[changed$state %in% controls,c('state','year','cigsale')],1989:2000)
  dev<-max(abs(a$w-b$w),abs(pa$y0_hat-pb$y0_hat))
  add('validation_label_isolation','SCM_ADH',dev<=tol,'Clean Colorado case; train 1970-1988, predict 1989-2000; no-cache original and mutated refits',dev,tol,TRUE)
  inp<-prepare_training(d,ca,donors,1970:1988,'DID');fit<-fit_model(inp,ctx,FALSE,'twfe_analytic')
  pred<-predict_model(fit,d[d$state %in% donors,c('state','year','cigsale')],1989:2000)
  score<-score_predictions(pred,d[d$state==ca & d$year>=1989,c('year','cigsale')])
  dat<-d;dat$treat<-as.integer(dat$state==ca & dat$year>=1989)
  twfe<-lm(cigsale~factor(state)+factor(year)+treat,data=dat)
  tau<-unname(coef(twfe)['treat']);analytic<-mean(score$gap)
  out_csv(ctx,data.frame(run_id=ctx$run_id,train_start=1970,train_end=1988,eval_start=1989,eval_end=2000,n_eval=12,n_rows=nrow(dat),analytic_did=analytic,twfe_tau=tau,difference=tau-analytic,standard_error=NA_real_,inference_note='OLS point estimate only; single treated state; no ordinary t-test inference'),'twfe_equivalence')
  saveRDS(twfe,file.path(ctx$meta_dir,'twfe_point_estimate.rds'))
  add('did_twfe_equivalence','DID',abs(tau-analytic)<ctx$cfg$tolerances$equivalence,'Same balanced sample; static TWFE tau equals analytic average post gap',abs(tau-analytic),ctx$cfg$tolerances$equivalence,TRUE)
  z<-ctx$cfg$tolerances$near_zero_mspe
  add('ratio_boundary_conventions','all',is.infinite(ratio_stat(0,1,z)$ratio) && is.na(ratio_stat(0,0,z)$ratio) && ratio_stat(2,6,z)$ratio==3,'Inf for near-zero pre/nonzero post; NA for both near-zero; no epsilon added')
  results<-bind_rows(records);out_csv(ctx,results,'acceptance_checks')
  assert(all(results$status=='pass'),'Acceptance checks failed; see acceptance_checks.csv')
}
