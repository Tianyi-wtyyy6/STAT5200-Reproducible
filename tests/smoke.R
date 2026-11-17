# Deliberately synthetic data: these checks are not empirical paper results.
run_smoke <- function(original,output) {
  source(file.path(original,'R/common.R'))
  source(file.path(original,'R/models.R'))
  cfg <- yaml::read_yaml(file.path(original,'config/project.yml'))
  ctx <- list(cfg=cfg)
  set.seed(cfg$seed)
  d <- expand.grid(state=c('Target','DonorA','DonorB'),year=1970:2000,stringsAsFactors=FALSE)
  d$state_id <- match(d$state,c('Target','DonorA','DonorB'))
  t <- d$year-1970
  d$cigsale <- ifelse(d$state=='DonorA',80+t,ifelse(d$state=='DonorB',120+2*t,100+1.5*t))
  d$cigsale[d$state=='Target' & d$year>=1989] <- d$cigsale[d$state=='Target' & d$year>=1989]-10
  checks <- list()
  add <- function(name,ok) {checks[[length(checks)+1L]] <<- data.frame(check=name,status=if(isTRUE(ok))'pass'else'fail')}
  for(method in c('SCM_OUTCOME','DID')) {
    input <- prepare_training(d,'Target',c('DonorA','DonorB'),1970:1988,method)
    fit <- if(method=='DID')fit_did(input,ctx)else fit_outcome(input,ctx)
    pred <- predict_model(fit,d[d$state!='Target',c('state','year','cigsale')],1989:2000)
    score <- score_predictions(pred,d[d$state=='Target' & d$year>=1989,c('year','cigsale')])
    add(paste(method,'known_effect'),max(abs(score$gap+10))<1e-5)
    add(paste(method,'simplex'),abs(sum(fit$w)-1)<1e-6 && min(fit$w)>-1e-6)
    changed <- d;changed$cigsale[changed$state=='Target' & changed$year>=1989] <- 9999
    add(paste(method,'heldout_isolation'),identical(input,prepare_training(changed,'Target',c('DonorA','DonorB'),1970:1988,method)))
    rejected <- try(predict_model(fit,d,1989:2000),silent=TRUE)
    add(paste(method,'reject_target_prediction_input'),inherits(rejected,'try-error'))
  }
  add('raw_data_sha256',identical(hash_file(file.path(original,'data/raw/smoking.rda')),cfg$data$reference_sha256))
  result <- do.call(rbind,checks)
  write.csv(result,file.path(output,'smoke_checks.csv'),row.names=FALSE)
  print(result);stopifnot(nrow(result)==9L,all(result$status=='pass'))
}
