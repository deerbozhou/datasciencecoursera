
pollutantmean <- function(directory = "specdata", pollutant = "nitrate", id = 1:332) {
  
  if(pollutant=="nitrate"){
    polcol=3
  } else {
    polcol=2
  }
 
  basedir = getwd()
  fullpath=paste(basedir,directory,sep="/")
  files=list.files(fullpath)

  x=data.frame(Date="Date",sulfate="sulfate",nitrate="nitrate", ID="ID")
  first<-TRUE
  for (i in id) {   
    if (first) {  
      x<-data.frame(
          read.csv(
            paste(fullpath,'/',files[i],sep=''),header=T)
      )
      first<-FALSE
    ## print(x)
    ## print(dim(x))
    }else { 
      x<-rbind(
        x,data.frame(
          read.csv(
            paste(fullpath,'/',files[i],sep=''),header=T)))
    } ##end if
  } ## end for loop  
  relevantdata=x[,polcol]
  mean(relevantdata, na.rm = TRUE)   
}


