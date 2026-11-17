# Information enters through three distinct interfaces. No fit function receives held-out labels.
outcome_matrix <- function(d,states,years) {
  a<-matrix(NA_real_,length(years),length(states),dimnames=list(as.character(years),states))
  for(s in states) {
    z<-d[d$state==s & d$year %in% years,c('year','cigsale')]
    assert(nrow(z)==length(years) && !anyDuplicated(z$year),paste('Incomplete outcome matrix',s))
    a[,s]<-z$cigsale[match(years,z$year)]
  }
  assert(all(is.finite(a)),'Nonfinite outcome matrix')
  a
}
prepare_training <- function(d,target,donors,train,method,drop_predictor=NULL) {
  donors<-sort(donors,method='radix'); train<-sort(as.integer(train))
  assert(!target %in% donors && length(unique(donors))==length(donors),'Target/donor overlap or duplicate donor')
  cols<-c('state','state_id','year','cigsale')
  if(method=='SCM_ADH')cols<-c(cols,'lnincome','retprice','age15to24','beer')
  x<-d[d$state %in% c(target,donors) & d$year %in% train,cols]
  x<-x[order(x$state_id,x$year),]; rownames(x)<-NULL
  list(data=x,target=target,donors=donors,years=train,method=method,drop_predictor=drop_predictor)
}
fit_adh <- function(inp,ctx,settings) {
  assert(all(1970:1988 %in% inp$years),'Original ADH specification requires original training years')
  pred<-setdiff(names(ctx$cfg$adh$predictors),inp$drop_predictor)
  for(v in pred) {
    var<-if(grepl('^cigsale_',v))'cigsale' else v
    yy<-yrs(ctx$cfg$adh$predictors[[v]])
    assert(all(yy %in% inp$years),paste('Unavailable predictor window',v))
    z<-inp$data[inp$data$year %in% yy,]
    assert(nrow(z)==length(yy)*(length(inp$donors)+1L) && all(is.finite(z[[var]])),paste('Incomplete predictor window',v))
  }
  normal<-intersect(c('lnincome','retprice','age15to24'),pred)
  sp_names<-intersect(c('beer','cigsale_1975','cigsale_1980','cigsale_1988'),pred)
  special<-lapply(sp_names,function(v)list(if(grepl('^cigsale_',v))'cigsale' else v,yrs(ctx$cfg$adh$predictors[[v]]),'mean'))
  ids<-unique(inp$data[c('state','state_id')])
  dp<-Synth::dataprep(foo=as.data.frame(inp$data),predictors=normal,predictors.op='mean',special.predictors=special,
    dependent='cigsale',unit.variable='state_id',time.variable='year',unit.names.variable='state',
    treatment.identifier=ids$state_id[match(inp$target,ids$state)],controls.identifier=ids$state_id[match(inp$donors,ids$state)],
    time.predictors.prior=1980:1988,time.optimize.ssr=inp$years,time.plot=inp$years)
  # dataprep sorts by numeric unit ID; verify and retain that mapping explicitly.
  donors<-ids$state[match(as.integer(colnames(dp$X0)),ids$state_id)]
  assert(identical(donors,inp$donors),'dataprep donor order mismatch')
  rn<-c(normal,sp_names); rownames(dp$X0)<-rownames(dp$X1)<-rn
  args<-c(list(data.prep.obj=dp,optimxmethod=unlist(ctx$cfg$adh$optimxmethod),genoud=FALSE,quadopt='ipop',verbose=TRUE),settings)
  sy<-do.call(Synth::synth,args)
  w<-setNames(as.numeric(sy$solution.w),donors)
  v<-setNames(as.numeric(unlist(sy$solution.v)),rn)
  tol<-ctx$cfg$tolerances$constraint
  assert(all(is.finite(w)) && min(w)>=-tol && abs(sum(w)-1)<=tol,'SCM_ADH infeasible/nonfinite weights')
  assert(all(is.finite(v)) && is.finite(sy$loss.v),'SCM_ADH nonfinite optimization result')
  y<-as.numeric(dp$Z1); z<-dp$Z0
  assert(abs(mean((y-as.numeric(z%*%w))^2)-as.numeric(sy$loss.v))<1e-6,'Synth loss does not match original-scale pre-MSPE')
  scale_sd<-sqrt(apply(cbind(dp$X0,dp$X1),1,stats::var))
  selected<-sy$rgV.optim$out.list[1,,drop=FALSE]
  selected_method<-rownames(selected)[1]
  selected_convcode<-as.integer(selected$convcode)
  if(selected_convcode!=0L)warning(paste('Selected finite pre-loss candidate has outer optimizer convcode',selected_convcode,'; retained without post-outcome-based selection.'))
  list(w=w,raw_w=w,offset=0,objective=as.numeric(sy$loss.v),loss_w=as.numeric(sy$loss.w),
    solver_status=paste('Synth',selected_method,'convcode',selected_convcode),selected_optimizer=selected_method,selected_convcode=selected_convcode,
    optimizer=sy$rgV.optim,V=v,X1=dp$X1,X0=dp$X0,scale_sd=scale_sd,
    X1_scaled=dp$X1/scale_sd,X0_scaled=dp$X0/scale_sd,dataprep=dp,synth=sy,settings=settings,
    constraint_sum=abs(sum(w)-1),constraint_nonneg=max(0,-min(w)),raw_equal_mean_mspe=mean((y-rowMeans(z))^2),
    train=inp$years,donors=donors,target=inp$target,method=inp$method)
}
fit_outcome <- function(inp,ctx,retry=FALSE) {
  z<-outcome_matrix(inp$data,inp$donors,inp$years)
  y<-as.numeric(outcome_matrix(inp$data,inp$target,inp$years))
  n<-length(y); j<-ncol(z)
  # Uniform rescaling of the ENTIRE unpenalized objective, not row standardization or ridge.
  sc<-mean(z^2)
  P<-Matrix::Matrix(2*crossprod(z)/(n*sc),sparse=TRUE)
  q<-as.numeric(-2*crossprod(z,y)/(n*sc))
  A<-rbind(Matrix::Matrix(rep(1,j),nrow=1,sparse=TRUE),Matrix::Diagonal(j))
  pars<-osqp::osqpSettings(verbose=FALSE,eps_abs=if(retry)1e-9 else ctx$cfg$qp$eps_abs,
    eps_rel=if(retry)1e-9 else ctx$cfg$qp$eps_rel,max_iter=if(retry)1000000L else as.integer(ctx$cfg$qp$max_iter),
    polishing=TRUE,adaptive_rho=FALSE,scaled_termination=FALSE)
  sol<-osqp::solve_osqp(P=P,q=q,A=A,l=c(1,rep(0,j)),u=c(1,rep(Inf,j)),pars=pars)
  assert(tolower(sol$info$status)=='solved',paste('OSQP status:',sol$info$status))
  wr<-setNames(as.numeric(sol$x),inp$donors); tol<-ctx$cfg$tolerances$constraint
  assert(all(is.finite(wr)) && min(wr)>=-tol && abs(sum(wr)-1)<=tol,'OSQP infeasible raw weights')
  w<-pmax(wr,0); w<-w/sum(w)
  loss<-mean((y-as.numeric(z%*%w))^2); rawloss<-mean((y-as.numeric(z%*%wr))^2)
  eq<-mean((y-rowMeans(z))^2)
  assert(loss <= eq + ctx$cfg$tolerances$objective_absolute + ctx$cfg$tolerances$objective_relative*eq,'Outcome SCM loss exceeds feasible equal-weight mean')
  assert(abs(loss-rawloss)<1e-4,'Roundoff cleanup materially changed objective')
  list(w=w,raw_w=wr,offset=0,objective=loss,raw_objective=rawloss,objective_scale=sc,raw_equal_mean_mspe=eq,
    solver_status=sol$info$status,solver_info=sol$info,settings=pars,solution=sol,
    constraint_sum=abs(sum(wr)-1),constraint_nonneg=max(0,-min(wr)),
    hessian_rank=as.integer(Matrix::rankMatrix(P)),hessian_dimension=j,
    train=inp$years,donors=inp$donors,target=inp$target,method=inp$method)
}
fit_did <- function(inp,ctx) {
  z<-outcome_matrix(inp$data,inp$donors,inp$years)
  y<-as.numeric(outcome_matrix(inp$data,inp$target,inp$years))
  b<-mean(y-rowMeans(z)); w<-setNames(rep(1/ncol(z),ncol(z)),inp$donors)
  list(w=w,raw_w=w,offset=b,objective=mean((y-rowMeans(z)-b)^2),raw_equal_mean_mspe=mean((y-rowMeans(z))^2),
    solver_status='analytic',constraint_sum=abs(sum(w)-1),constraint_nonneg=0,
    train=inp$years,donors=inp$donors,target=inp$target,method=inp$method)
}
fit_model <- function(inp,ctx,use_cache=TRUE,tag='normal') {
  # Cache keys contain only the effective training inputs. Predict/score do not enter this key.
  key<-hash_obj(list(input=inp,seed=ctx$cfg$seed,adh=ctx$cfg$adh,qp=ctx$cfg$qp,tolerances=ctx$cfg$tolerances,
    model_code=hash_file('R/models.R'),versions=ctx$versions[c('Synth','osqp','Matrix','optimx','kernlab','rgenoud')]))
  fid<-paste0('fit_',substr(key,1,24)); path<-file.path('models',paste0(fid,'.rds'))
  if(use_cache && file.exists(path)) {
    fit<-readRDS(path); fit$cache_reused<-TRUE; return(fit)
  }
  warnings<-character(); attempts<-list(); result<-NULL
  logpath<-file.path(ctx$log_dir,paste0(fid,'_',tag,'.txt'))
  con<-file(logpath,'wt'); sink(con); on.exit({sink();close(con)},add=TRUE)
  cat('fit_id:',fid,' UTC:',utc_now(),' target:',inp$target,' method:',inp$method,'\n')
  for(attempt in seq_len(if(inp$method=='DID')1L else 2L)) {
    t0<-proc.time()[3]
    result<-tryCatch(withCallingHandlers({
      set.seed(ctx$cfg$seed)
      if(inp$method=='SCM_ADH')fit_adh(inp,ctx,if(attempt==1)ctx$cfg$adh$primary else ctx$cfg$adh$retry)
      else if(inp$method=='SCM_OUTCOME')fit_outcome(inp,ctx,retry=attempt==2L)
      else fit_did(inp,ctx)
    },warning=function(w){warnings<<-c(warnings,conditionMessage(w)); cat('WARNING:',conditionMessage(w),'\n'); invokeRestart('muffleWarning')}),error=function(e)e)
    ok<-!inherits(result,'error')
    attempts[[attempt]]<-list(attempt=attempt,success=ok,error=if(ok)'' else conditionMessage(result),elapsed_sec=unname(proc.time()[3]-t0))
    if(ok)break
    cat('Attempt',attempt,'failed:',conditionMessage(result),'\n')
  }
  if(inherits(result,'error'))stop(paste('All numerical attempts failed:',conditionMessage(result)),call.=FALSE)
  result$fit_id<-fid; result$input_hash<-key; result$warnings<-unique(warnings); result$attempts<-attempts
  result$cache_reused<-FALSE; result$computed_utc<-utc_now(); result$input<-inp
  if(use_cache)saveRDS(result,path)
  result
}
predict_model <- function(fit,donor_data,years) {
  assert(all(donor_data$state %in% fit$donors),'Prediction interface accepts donor states only')
  z<-outcome_matrix(donor_data,fit$donors,years)
  y0<-as.numeric(z%*%fit$w)+fit$offset
  assert(all(is.finite(y0)),'Nonfinite counterfactual')
  data.frame(year=as.integer(years),y0_hat=y0)
}
score_predictions <- function(pred,target_labels) {
  assert(!anyDuplicated(target_labels$year) && setequal(pred$year,target_labels$year),'Label/prediction year mismatch')
  y<-target_labels$cigsale[match(pred$year,target_labels$year)]
  assert(all(is.finite(y)),'Missing held-out truth')
  data.frame(year=pred$year,y_observed=y,y0_hat=pred$y0_hat,gap=y-pred$y0_hat)
}
