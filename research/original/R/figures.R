# All figures are reconstructed from exported tables, never by fitting models.
make_figures <- function(ctx) {
  library(ggplot2)
  cfg<-ctx$cfg;rd<-ctx$result_dir
  a<-read_csv(file.path(rd,'annual_estimates.csv'));m<-read_csv(file.path(rd,'effect_summary.csv'))
  w<-read_csv(file.path(rd,'donor_weights.csv'));b<-read_csv(file.path(rd,'predictor_balance.csv'))
  vp<-read_csv(file.path(rd,'validation_predictions.csv'));pairs<-read_csv(file.path(rd,'paired_method_comparisons.csv'))
  ps<-read_csv(file.path(rd,'placebo_summary.csv'));rob<-read_csv(file.path(rd,'robustness_summary.csv'))
  cc<-read_csv(file.path(rd,'classic_clean_comparison.csv'))
  cols<-unlist(cfg$figures$colors);lts<-unlist(cfg$figures$linetypes);index<-list()
  theme_set(theme_bw(base_size=cfg$figures$base_size)+theme(panel.grid.minor=element_blank(),legend.position='bottom',
    plot.title=element_text(face='bold'),plot.subtitle=element_text(size=9),plot.caption=element_text(hjust=0,size=8),strip.background=element_rect(fill='grey94')))
  method_scales<-function()list(scale_color_manual(values=cols,drop=FALSE),scale_linetype_manual(values=lts,drop=FALSE))
  line_policy<-function(y=1989)geom_vline(xintercept=y-.5,linetype='dotted',color='grey35',linewidth=.45)
  savefig<-function(id,p,width=7,height=6,scope='main',inputs='',filter='',comparison='CA_main_38',purpose='') {
    dir<-file.path(ctx$figure_dir,scope);dir.create(dir,recursive=TRUE,showWarnings=FALSE)
    pdf<-file.path(dir,paste0(id,'.pdf'));png<-file.path(dir,paste0(id,'.png'))
    buildpath<-file.path(dir,paste0(id,'.build.rds'))
    fingerprint<-hash_obj(list(figure_code=hash_file('R/figures.R'),config=cfg$figures,
      inputs=setNames(vapply(strsplit(inputs,';',fixed=TRUE)[[1]],function(x)hash_file(file.path(ctx$result_dir,x)),''),strsplit(inputs,';',fixed=TRUE)[[1]]),id=id,width=width,height=height))
    reused<-file.exists(pdf)&&file.exists(png)&&file.exists(buildpath)&&identical(readRDS(buildpath),fingerprint)
    if(!reused) {
      ggsave(pdf,p,width=width,height=height,device=grDevices::cairo_pdf,limitsize=FALSE)
      ggsave(png,p,width=width,height=height,dpi=cfg$figures$dpi,bg='white',limitsize=FALSE)
      saveRDS(fingerprint,buildpath)
    }
    index[[length(index)+1L]]<<-data.frame(run_id=ctx$run_id,item_id=id,type='figure',pdf_path=pdf,png_path=png,input_tables=inputs,filter_conditions=filter,comparison_set_id=comparison,scope=scope,purpose=purpose)
    out_csv(ctx,bind_rows(index),'figure_table_index')
    cat(if(reused)'Reused figure'else'Saved figure',id,'\n');flush.console()
    invisible(gc())
  }
  path_data<-function(z,facet_cols=character()) {
    pred<-z;pred$series<-pred$method;pred$value<-pred$y0_hat
    obs<-z[!duplicated(z[c(facet_cols,'year')]),];obs$series<-'Observed';obs$value<-obs$y_observed
    bind_rows(pred,obs)
  }
  main<-a[a$analysis_type=='main',];adh<-main[main$method=='SCM_ADH',]
  pd<-path_data(adh)
  p1<-ggplot(pd,aes(year,value,color=series,linetype=series))+geom_line(linewidth=.8)+line_policy()+method_scales()+
    labs(title='F1. Replication of the California sales path',subtitle='SCM_ADH; train 1970-1988; 38 donors. First treated year: 1989',x=NULL,y='Cigarette sales (packs/person/year)',color=NULL,linetype=NULL)
  p2<-ggplot(adh,aes(year,gap))+geom_hline(yintercept=0,color='grey60')+geom_line(color=cols['SCM_ADH'],linewidth=.8)+line_policy()+
    labs(title='Annual California-minus-synthetic gap',subtitle='Corresponds to ADH Figures 2-3; reproduced from this project',x='Year',y='Gap (packs/person/year)')
  savefig('F1_replication',patchwork::wrap_plots(p1,p2,ncol=1),height=7,inputs='annual_estimates.csv',filter='analysis_type=main; method=SCM_ADH',purpose='ADH Figures 2-3 replication')
  wm<-w[w$analysis_type=='main' & w$method=='SCM_ADH',];ww<-wm[wm$weight>=.001,]
  p<-ggplot(ww,aes(reorder(donor,weight),weight))+geom_col(fill=cols['SCM_ADH'])+coord_flip()+
    geom_text(aes(label=sprintf('%.3f',weight)),hjust=-.1,size=3)+expand_limits(y=max(ww$weight)*1.15)+
    labs(title='F2a. Composition of synthetic California',subtitle='SCM_ADH; train 1970-1988; 38 donors; plotted weights >= 0.1%',x=NULL,y='Donor weight (sum of all 38 weights = 1)')
  savefig('F2a_adh_weights',p,height=3.7,inputs='donor_weights.csv',filter='main SCM_ADH; weight>=0.001',purpose='ADH Table 2 visual comparison')
  bb<-b[b$analysis_type=='main' & b$method=='SCM_ADH',]
  bl<-bind_rows(data.frame(predictor=bb$predictor,difference=(bb$synthetic_value-bb$target_value)/bb$scale_sd,series='SCM_ADH'),
    data.frame(predictor=bb$predictor,difference=(bb$equal_weight_donor_mean-bb$target_value)/bb$scale_sd,series='Equal donor mean'))
  p<-ggplot(bl,aes(difference,reorder(predictor,difference),color=series,shape=series))+geom_vline(xintercept=0,color='black')+geom_point(size=2.6)+
    scale_color_manual(values=c(SCM_ADH=unname(cols['SCM_ADH']),'Equal donor mean'='grey50'))+
    labs(title='F2b. Predictor balance in common visual units',subtitle='Synthetic/equal donor mean minus California; divided by cross-state SD',x='Difference / sample SD across California and 38 donors',y=NULL,color=NULL,shape=NULL,
      caption='California is zero. Original units and all seven windows are preserved in predictor_balance.csv.\nThe equal donor mean is unweighted; it is not the population-weighted ADH Table 1 column.')
  savefig('F2b_predictor_balance',p,height=4.6,inputs='predictor_balance.csv',filter='analysis_type=main; method=SCM_ADH',purpose='Predictor balance without mixing original units')
  pp<-path_data(main)
  p1<-ggplot(pp,aes(year,value,color=series,linetype=series))+geom_line(linewidth=.75)+line_policy()+method_scales()+
    labs(title='F3. Three counterfactual specifications',subtitle='Train 1970-1988; 38 donors. First treated year: 1989',x=NULL,y='Sales (packs/person/year)',color=NULL,linetype=NULL)
  p2<-ggplot(main,aes(year,gap,color=method,linetype=method))+geom_hline(yintercept=0,color='grey60')+geom_line(linewidth=.75)+line_policy()+method_scales()+
    labs(x='Year',y='Gap (packs/person/year)',color=NULL,linetype=NULL,caption='SCM_OUTCOME versus DID is the common-information comparison.\nSCM_ADH also uses pre-policy covariates and selected outcome lags.')
  savefig('F3_method_comparison',patchwork::wrap_plots(p1,p2,ncol=1,guides='collect'),height=7,inputs='annual_estimates.csv',filter='analysis_type=main',purpose='Counterfactual and gap comparison')
  ca<-a[a$analysis_type=='ca_validation' & a$year>=1977,];pc<-path_data(ca,'case_id')
  breaks<-data.frame(case_id=c('CA_pre_1','CA_pre_2'),boundary=c(1980.5,1984.5))
  p1<-ggplot(pc,aes(year,value,color=series,linetype=series))+geom_line(linewidth=.75)+geom_vline(data=breaks,aes(xintercept=boundary),linetype='dotted')+
    facet_wrap(~case_id,scales='free_x')+method_scales()+labs(title='F4a. California pre-policy holdout validation',subtitle='CA_pre_1: train 1970-1980, test 1981-1984; CA_pre_2: train 1970-1984, test 1985-1988',x=NULL,y='Sales (packs/person/year)',color=NULL,linetype=NULL)
  ce<-vp[vp$analysis_type=='ca_validation',]
  p2<-ggplot(ce,aes(year,error,color=method,linetype=method))+geom_hline(yintercept=0,color='grey60')+geom_line(linewidth=.75)+geom_point(size=1.6)+
    facet_wrap(~case_id,scales='free_x')+method_scales()+labs(x='Held-out year',y='Observed - predicted (packs/person/year)',color=NULL,linetype=NULL,
      caption='Only target labels were withheld; contemporaneous donor outcomes were available.\nEight test years, overlapping training sets; SCM_ADH is not applicable to these early folds.')
  savefig('F4a_CA_holdouts',patchwork::wrap_plots(p1,p2,ncol=1,guides='collect'),height=7,inputs='annual_estimates.csv;validation_predictions.csv',filter='ca_validation',comparison='CA_pre_holdout',purpose='Two temporal holdout folds')
  dp<-pairs[pairs$analysis_type=='donor_validation' & pairs$common_success,]
  p<-ggplot(dp,aes(delta_RMSE,reorder(target,delta_RMSE)))+geom_vline(xintercept=0,color='grey40')+
    geom_segment(aes(x=0,xend=delta_RMSE,yend=reorder(target,delta_RMSE)),color='grey70')+geom_point(aes(color=delta_RMSE<0),size=2)+
    scale_color_manual(values=c('TRUE'=cols['SCM_OUTCOME'],'FALSE'=cols['DID']),guide='none')+
    labs(title='F4b. Paired held-out prediction errors by donor state',subtitle=paste0(nrow(dp),'/38 common successes; train 1970-1988; test 1989-2000; 37 donors; California excluded'),
      x='RMSE(SCM_OUTCOME) - RMSE(DID), packs/person/year',y=NULL,caption='Negative favors SCM_OUTCOME. Cases share donors, years and shocks; no independent-case t-test.\nThese predictions are reused as the clean placebo reference.')
  savefig('F4b_donor_paired_RMSE',p,height=8.8,inputs='paired_method_comparisons.csv',filter='donor_validation; common_success=TRUE',comparison=cfg$placebo$clean_id,purpose='All 38 donor paired comparisons')
  placebo_gap<-function(filter,scope,id) {
    eligible<-ps[ps$comparison_set_id==cfg$placebo$classic_id & ps$filter==filter & ps$eligible,]
    z<-merge(a[a$analysis_type=='classic_placebo',],eligible[c('case_id','method')],by=c('case_id','method'))
    count<-aggregate(target~method,eligible,function(x)sum(x!='California'));z$panel<-paste0(z$method,' (',count$target[match(z$method,count$method)],' placebo donors)')
    p<-ggplot(z,aes(year,gap,group=target))+geom_hline(yintercept=0,color='grey70')+
      geom_line(data=z[z$target!='California',],color='grey70',linewidth=.4)+geom_line(data=z[z$target=='California',],color='black',linewidth=.85)+
      line_policy()+facet_wrap(~panel,ncol=1)+labs(title=paste0(if(scope=='main')'F5a'else id,'. Classic placebo gaps'),
        subtitle=paste0('39-target symmetric rotation; train 1970-1988; 38 donors per target; filter: ',filter),x='Year',y='Gap (packs/person/year)',
        caption='Black: California. Gray: eligible placebo states. First treated year: 1989.\nFilter uses pre-MSPE (not RMSPE). California enters other states\' donor pools.')
    savefig(id,p,height=8,scope=scope,inputs='annual_estimates.csv;placebo_summary.csv',filter=paste('classic_placebo; eligible;',filter),comparison=cfg$placebo$classic_id,purpose='Filtered gap comparison')
  }
  placebo_gap('c2','main','F5a_classic_gap_c2')
  r<-ps[ps$comparison_set_id==cfg$placebo$classic_id & ps$filter=='unfiltered',]
  r<-r[order(r$method,-r$mspe_ratio,r$target),];r$rank_order<-ave(r$mspe_ratio,r$method,FUN=function(x)seq_along(x))
  finite<-is.finite(r$mspe_ratio);cap<-if(any(finite))max(r$mspe_ratio[finite])*1.1 else 1;r$plot_ratio<-ifelse(is.infinite(r$mspe_ratio),cap,r$mspe_ratio)
  p<-ggplot(r,aes(rank_order,plot_ratio))+geom_line(color='grey70')+geom_point(aes(shape=target=='California'),color='grey55',size=1.5)+
    geom_point(data=r[r$target=='California',],color='black',size=2.7)+geom_text(data=r[r$target=='California',],aes(label=' CA'),hjust=0,nudge_x=.4,size=3)+
    facet_wrap(~method,ncol=1,scales='free_y')+guides(shape='none')+
    labs(title='F5b. Unfiltered post/pre MSPE ratio ranking',subtitle='Classic comparison; all 39 targets per method; post 1989-2000 / pre 1970-1988',x='Descending ratio order',y='MSPE ratio (dimensionless)',
      caption='This panel uses the full set, unlike F5a. Ratio ignores gap direction; ties use >=.\nInfinite ratios, if present, are labeled Inf at the plotting cap; actual values determine ranks.')
  if(any(is.infinite(r$mspe_ratio)))p<-p+geom_text(data=r[is.infinite(r$mspe_ratio),],aes(label='Inf'),vjust=-.2,size=3)
  savefig('F5b_classic_ratio_unfiltered',p,height=7.8,inputs='placebo_summary.csv',filter='classic; unfiltered',comparison=cfg$placebo$classic_id,purpose='ADH Figure 8 ranking comparison')
  clean<-a[a$analysis_type=='donor_validation',];cm<-main;cm$target<-'California'
  p<-ggplot(clean,aes(year,gap,group=target))+geom_hline(yintercept=0,color='grey70')+geom_line(color='grey75',linewidth=.4)+
    geom_line(data=cm,aes(group=method),color='black',linewidth=.85)+line_policy()+facet_wrap(~method,ncol=1)+
    labs(title='F6a. Clean placebo descriptive reference',subtitle='No California in placebo fits/predictions; train 1970-1988; evaluation 1989-2000',x='Year',y='Gap (packs/person/year)',
      caption='Gray: 38 donor targets, each using 37 donors. Black: main California gap using 38 donors.\nAsymmetric comparison; descriptive reference only, with no formal p-value or causal confidence band.')
  savefig('F6a_clean_gap',p,height=8,inputs='annual_estimates.csv',filter='donor_validation plus CA main reference',comparison=cfg$placebo$clean_id,purpose='Clean descriptive gaps')
  p<-ggplot(cc,aes(mean_gap_classic,mean_gap_clean))+geom_abline(slope=1,intercept=0,color='grey65')+geom_point(size=1.5,color='grey40')+
    facet_wrap(~method,scales='free')+labs(title='F6b. Classical versus clean pseudo-effect comparison',subtitle='All 38 original donor targets; train 1970-1988; evaluation 1989-2000',
      x='Classic mean gap: 38 donors including CA',y='Clean mean gap: 37 donors excluding CA',caption='Units: packs/person/year. Both versions refit each target; clean uses the donor-validation cache.\nDifferences reflect removing California from a changing donor pool.')
  savefig('F6b_classic_clean',p,height=4.2,width=8,inputs='classic_clean_comparison.csv',filter='all donor targets',comparison=paste(cfg$placebo$classic_id,cfg$placebo$clean_id,sep=' versus '),purpose='Classic/clean contrast')
  loo<-rob[rob$analysis_type=='loo' & rob$highlight_loo,];baseline<-m[m$analysis_type=='main',]
  p<-ggplot(loo,aes(mean_gap,reorder(perturbation,mean_gap),color=method))+geom_vline(data=baseline,aes(xintercept=mean_gap,color=method),linetype='dotted')+
    geom_point(size=2)+facet_wrap(~method,nrow=1,scales='free_x')+scale_color_manual(values=cols,guide='none')+
    labs(title='F7a. Leave-one-donor-out sensitivity',subtitle='California; train 1970-1988; evaluation 1989-2000; 37 donors after removal',x='Mean post gap (packs/person/year)',y='Removed donor',
      caption='Shown donors have >=1% baseline weight in either SCM; dotted lines are baseline estimates.\nAll 38 removals were refitted and exported. The span is a sensitivity range, not a confidence interval.')
  savefig('F7a_LOO_highlights',p,height=4.5,width=8,inputs='robustness_summary.csv',filter='loo; highlight_loo=TRUE',comparison='CA_LOO_37',purpose='Prespecified pre-weight highlight rule')
  win<-rob[rob$analysis_type=='training_window',]
  wl<-bind_rows(data.frame(win,metric='Mean post gap',value=win$mean_gap),data.frame(win,metric='2000 gap',value=win$gap_2000),data.frame(win,metric='Common 1980-1988 RMSPE',value=win$common_1980_1988_rmspe))
  p<-ggplot(wl,aes(factor(train_start),value,color=method,linetype=method,group=method))+geom_line()+geom_point(size=2)+facet_wrap(~metric,scales='free_y')+method_scales()+
    labs(title='F7b. Training-window sensitivity',subtitle='Three registered windows ending 1988; common-fit evaluation 1980-1988; policy evaluation 1989-2000',
      x='Training start year',y='Packs/person/year',color=NULL,linetype=NULL,caption='SCM_ADH remains at its original specification. Window-specific training errors are exported separately.')
  savefig('F7b_windows',p,height=4.2,width=8,inputs='robustness_summary.csv',filter='training_window',comparison='CA_window_38',purpose='Common versus own training window')
  tp<-a[a$analysis_type=='time_placebo',];tp$pseudo<-ifelse(tp$case_id=='time_1981',1981,1985)
  cut<-unique(tp[c('case_id','pseudo')]);cut$boundary<-cut$pseudo-.5
  p<-ggplot(tp,aes(year,gap,color=method,linetype=method))+geom_hline(yintercept=0,color='grey60')+geom_line(linewidth=.7)+
    geom_vline(data=cut,aes(xintercept=boundary),linetype='dotted',inherit.aes=FALSE)+facet_wrap(~pseudo,labeller=labeller(pseudo=function(x)paste0('Pseudo-treatment: ',x)))+method_scales()+
    labs(title='F7c. Time placebos before the real intervention',subtitle='California; 38 donors; train 1970-1980 or 1970-1984; evaluate through 1988',x='Year',y='Gap (packs/person/year)',color=NULL,linetype=NULL,
      caption='No data from 1989 onward enter these experiments. Boundaries: 1980.5 and 1984.5.\nThe 1981 placebo evaluates eight years; CA_pre_1 uses only its first four years.')
  savefig('F7c_time_placebos',p,height=4.3,width=8,inputs='annual_estimates.csv',filter='time_placebo',comparison='CA_time_pre_only',purpose='Pre-policy time placebos')
  for(f in c('unfiltered','c20','c5'))placebo_gap(f,'supplement',paste0('S_classic_gap_',f))
  ld<-rob[rob$analysis_type=='loo',]
  p<-ggplot(ld,aes(mean_gap,reorder(perturbation,mean_gap),color=method))+geom_vline(data=baseline,aes(xintercept=mean_gap,color=method),linetype='dotted')+
    geom_point(size=1.5)+facet_wrap(~method,nrow=1,scales='free_x')+scale_color_manual(values=cols,guide='none')+
    labs(title='All 38 donor removals',subtitle='California; refitted weights/V or offset; train 1970-1988; evaluation 1989-2000',x='Mean post gap (packs/person/year)',y='Removed donor')
  savefig('S_LOO_all',p,height=9,width=8,scope='supplement',inputs='robustness_summary.csv',filter='loo',comparison='CA_LOO_37',purpose='Complete LOO sensitivity')
  dr<-rob[rob$analysis_type=='predictor_deletion',]
  dl<-bind_rows(data.frame(dr,metric='Mean post gap',value=dr$mean_gap),data.frame(dr,metric='2000 gap',value=dr$gap_2000),data.frame(dr,metric='Pre-RMSPE',value=dr$pre_rmspe))
  p<-ggplot(dl,aes(value,perturbation))+geom_point(color=cols['SCM_ADH'],size=2)+facet_wrap(~metric,scales='free_x')+
    labs(title='SCM_ADH: all seven predictor deletions',subtitle='Remaining windows unchanged; V and w reestimated with train 1970-1988',x='Packs/person/year',y='Removed predictor',caption='Specification sensitivity added by this project; no best subset selected.')
  savefig('S_predictor_deletions',p,height=4.5,width=8,scope='supplement',inputs='robustness_summary.csv',filter='predictor_deletion',comparison='CA_predictor_38',purpose='All seven predictor deletions')
  near<-a[a$analysis_type=='near_policy',]
  p<-ggplot(near,aes(year,gap,color=method,linetype=method))+geom_hline(yintercept=0,color='grey60')+geom_line(linewidth=.8)+line_policy()+
    geom_vline(xintercept=1986.5,linetype='dashed',color='grey60')+method_scales()+labs(title='Exclude the last two pre-policy years from fitting',
      subtitle='Train 1970-1986; diagnose 1987-1988; true policy evaluation 1989-2000; 38 donors',x='Year',y='Gap (packs/person/year)',color=NULL,linetype=NULL,
      caption='Gray dashed: training cutoff; dotted: first treated year 1989.\nThe held-out 1987-1988 diagnostic does not establish anticipation.')
  savefig('S_near_policy',p,height=4.5,scope='supplement',inputs='annual_estimates.csv',filter='near_policy',comparison='CA_near_policy_38',purpose='Last pre-policy years sensitivity')
  wm<-w[w$analysis_type=='main',]
  p<-ggplot(wm,aes(weight,reorder(donor,weight)))+geom_col(fill='grey40')+facet_wrap(~method,nrow=1)+
    labs(title='All baseline donor weights, including numerical zeros',subtitle='Train 1970-1988; 38 donors; DID offset is reported separately',x='Weight',y=NULL)
  savefig('S_weights_all',p,height=9,width=8,scope='supplement',inputs='donor_weights.csv',filter='main',purpose='Complete baseline weights')
  # One page for every state, with classic and clean paths/gaps side by side for donors.
  for(q in sort(unique(a$target[a$analysis_type=='classic_placebo']),method='radix')) {
    z<-a[a$target==q & a$analysis_type %in% c('classic_placebo','donor_validation'),]
    z$scheme<-ifelse(z$analysis_type=='classic_placebo','Classic: 38 donors','Clean: 37 donors, no CA')
    pz<-path_data(z,'scheme')
    p1<-ggplot(pz,aes(year,value,color=series,linetype=series))+geom_line(linewidth=.55)+line_policy()+facet_wrap(~scheme)+method_scales()+
      labs(title=paste('State counterfactuals:',q),subtitle='Train 1970-1988; evaluate 1989-2000. First treated year: 1989',x=NULL,y='Sales (packs/person/year)',color=NULL,linetype=NULL)
    p2<-ggplot(z,aes(year,gap,color=method,linetype=method))+geom_hline(yintercept=0,color='grey65')+geom_line(linewidth=.55)+line_policy()+facet_wrap(~scheme)+method_scales()+
      labs(x='Year',y='Gap (packs/person/year)',color=NULL,linetype=NULL,caption='All planned states shown, irrespective of prediction performance. Clean results reuse donor validation.\nPlacebo post gaps measure pseudo-effects/prediction deviations; they are not observed policy effects.')
    savefig(paste0('state_',slug(q)),patchwork::wrap_plots(p1,p2,ncol=1,guides='collect'),height=7,width=8,scope='supplement/states',inputs='annual_estimates.csv',filter=paste('target=',q,'; classic and clean'),comparison='classic_39_symmetric;clean_38_no_CA',purpose='Complete state pagination')
  }
  out_csv(ctx,bind_rows(index),'figure_table_index')
  writeLines('Figures regenerated from CSV tables only; no fit calls in this stage.',file.path(ctx$log_dir,'figure_reconstruction.txt'))
}
