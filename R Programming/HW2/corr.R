corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  basedir = getwd()
  fullpath=paste(basedir,directory,sep="/")
  files=list.files(fullpath)
  c1<-numeric(0)
  c2<-numeric(0)
  maxfiles<-length(files)
  
  
  ## Get all the data in x
  for (i in 1:maxfiles) { 
    x<-data.frame(
      read.csv( paste(fullpath,'/',files[i],sep=''),header=TRUE)
    )
    
    good<-complete.cases(x)
    
    y<-as.data.frame(x[good,]) 
    if(nrow(y)>threshold){     
      n<-as.vector(y["nitrate"])
      s<-as.vector(y["sulfate"])
      mcor<-cor(n,s)
      
      c1<-c(c1,mcor)              
    } 
      
  } 
  c1  
  
  
}