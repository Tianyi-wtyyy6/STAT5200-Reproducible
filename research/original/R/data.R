audit_data <- function(ctx) {
  env <- new.env(parent=emptyenv()); load('data/raw/smoking.rda',envir=env)
  assert(exists('smoking',envir=env,inherits=FALSE),'Raw archive lacks smoking')
  raw <- as.data.frame(get('smoking',envir=env)); d <- raw
  required <- c('state','year','cigsale','lnincome','retprice','age15to24','beer')
  assert(setequal(names(d),required),'Unexpected data fields')
  types <- data.frame(variable=names(d),raw_class=vapply(d,function(x)paste(class(x),collapse=';'),''),raw_type=vapply(d,typeof,''))
  assert(all(d$year==as.integer(d$year)),'Noninteger years')
  d$year<-as.integer(d$year); states<-sort(unique(d$state),method='radix')
  ids<-data.frame(state_id=seq_along(states),state=states)
  d$state_id<-ids$state_id[match(d$state,ids$state)]
  d<-d[order(d$state_id,d$year),c('state','state_id','year','cigsale','lnincome','retprice','age15to24','beer')]
  rownames(d)<-NULL
  checks<-list()
  add<-function(check,actual,expected,pass,unit='') checks[[length(checks)+1L]]<<-data.frame(check=check,actual=as.character(actual),expected=as.character(expected),status=if(pass)'pass' else 'fail',unit=unit)
  add('rows',nrow(d),1209,nrow(d)==1209)
  add('states',length(states),39,length(states)==39)
  add('years',length(unique(d$year)),31,setequal(unique(d$year),1970:2000))
  add('unique_state_year',!anyDuplicated(d[c('state','year')]),TRUE,!anyDuplicated(d[c('state','year')]))
  counts<-as.data.frame(table(d$state)); names(counts)<-c('state','n_rows')
  add('balanced_panel',all(counts$n_rows==31),TRUE,all(counts$n_rows==31))
  add('first_treated_year',ctx$cfg$first_treated_year,1989,ctx$cfg$first_treated_year==1989)
  add('raw_hash',ctx$data_hash,ctx$cfg$data$reference_sha256,ctx$data_hash==ctx$cfg$data$reference_sha256)
  for(v in required[-c(1,2)]) {
    ref<-c(cigsale=0,retprice=0,lnincome=195,beer=663,age15to24=390)[v]
    add(paste0('missing_',v),sum(is.na(d[[v]])),ref,sum(is.na(d[[v]]))==ref)
  }
  add('cigsale_mean',mean(d$cigsale),118.8932175,abs(mean(d$cigsale)-118.8932175)<1e-6,'packs/person/year')
  add('cigsale_sd',sd(d$cigsale),32.7674041,abs(sd(d$cigsale)-32.7674041)<1e-6,'packs/person/year')
  for(y in c(1970,1988,1989,2000)) {
    val<-d$cigsale[d$state=='California' & d$year==y]; ref<-c(`1970`=123,`1988`=90.1,`1989`=82.4,`2000`=41.6)[as.character(y)]
    add(paste0('CA_cigsale_',y),val,ref,abs(val-ref)<ctx$cfg$tolerances$data_reference,'packs/person/year')
  }
  audit<-bind_rows(checks); out_csv(ctx,audit,'data_audit'); out_csv(ctx,types,'field_types'); out_csv(ctx,counts,'state_row_counts')
  assert(all(audit$status=='pass'),'Data contract failed; inspect data_audit.csv before modeling')
  miss<-bind_rows(lapply(required[-c(1,2)],function(v)bind_rows(lapply(1970:2000,function(y)data.frame(variable=v,year=y,n_missing=sum(is.na(d[d$year==y,v])),n_total=39)))))
  out_csv(ctx,miss,'missingness_by_year')
  miss_state<-bind_rows(lapply(required[-c(1,2)],function(v)bind_rows(lapply(states,function(s)data.frame(variable=v,state=s,n_missing=sum(is.na(d[d$state==s,v])),n_total=31)))))
  out_csv(ctx,miss_state,'missingness_by_state')
  units<-c(cigsale='packs/person/year',lnincome='log real personal income per capita (1997 dollars); original field retained',retprice='cents/pack',age15to24='proportion (0-1)',beer='gallons/person/year')
  stats<-list()
  for(g in c('all','California','donors')) for(p in c('all_years','pre_1970_1988','post_1989_2000')) {
    sub<-d
    if(g=='California')sub<-sub[sub$state=='California',]
    if(g=='donors')sub<-sub[sub$state!='California',]
    if(p=='pre_1970_1988')sub<-sub[sub$year<=1988,]
    if(p=='post_1989_2000')sub<-sub[sub$year>=1989,]
    for(v in names(units)) {
      a<-sub[[v]][!is.na(sub[[v]])]
      stats[[length(stats)+1L]]<-data.frame(sample_definition=paste(g,p,sep=' / '),group=g,period=p,variable=v,unit=units[v],n_total=nrow(sub),n=length(a),n_missing=sum(is.na(sub[[v]])),mean=mean(a),sd=sd(a),min=min(a),max=max(a))
    }
  }
  out_csv(ctx,bind_rows(stats),'descriptive_statistics')
  write_csv(d,'data/processed/smoking_panel.csv'); write_csv(ids,'data/processed/state_id_mapping.csv'); saveRDS(d,'data/processed/panel.rds')
  writeLines(c('DATA AUDIT (actual file read; no listwise deletion or imputation)',paste('UTC:',utc_now()),paste('SHA256:',ctx$data_hash),'All structural and supplied numeric checks passed.','Canonical CSV contains all 1209 rows and all original missing values.','Income: Appendix A describes logged real state personal income; Table 1 labels it Ln(GDP per capita). The original field lnincome is retained.','Units follow ADH Appendix A and Table 1 notes; the tidysynth per-100,000 wording for cigarettes/beer is not used.','Observed descriptive summaries exclude missing cells explicitly, with n and n_missing reported. Model predictor windows must be complete and are never shortened.'),'data/processed/data_audit_report.txt')
  d
}
