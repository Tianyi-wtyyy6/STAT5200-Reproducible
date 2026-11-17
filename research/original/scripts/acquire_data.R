# Never overwrite an existing raw snapshot.
dir.create('data/raw',recursive=TRUE,showWarnings=FALSE)
dest<-'data/raw/smoking.rda'
if(!file.exists(dest)) {
  url<-'https://raw.githubusercontent.com/edunford/tidysynth/master/data/smoking.rda'
  log<-character();ok<-FALSE
  for(i in 1:3) {
    ans<-tryCatch({download.file(url,paste0(dest,'.part'),mode='wb',method='libcurl',quiet=FALSE);TRUE},error=function(e){log<<-c(log,paste(Sys.time(),conditionMessage(e)));FALSE})
    if(ans){file.rename(paste0(dest,'.part'),dest);ok<-TRUE;break}
  }
  writeLines(log,'logs/data_download_retries.txt')
  if(!ok)stop('Data unavailable. Place verified original smoking.rda in data/raw/; see logs/data_download_retries.txt')
  h<-digest::digest(file=dest,algo='sha256',serialize=FALSE)
  jsonlite::write_json(list(url=url,acquired_utc=format(Sys.time(),tz='UTC'),bytes=file.info(dest)$size,sha256=h,method='R libcurl',transformations='see R/data.R; raw snapshot unchanged'),
    'metadata/data_provenance.json',pretty=TRUE,auto_unbox=TRUE)
  if(h!='a946d153e8faee62e6deba7a16db6f4c1251d9e56d5b41de7e79a593eb8110d9')stop('New raw hash: audit serialization and numeric differences before use; raw file preserved')
}
