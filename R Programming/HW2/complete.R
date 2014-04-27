
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
    
  complete <- function(directory,id=1:332){
     
    basedir = getwd()
    fullpath=paste(basedir,directory,sep="/")
    files=list.files(fullpath)
    vect1<-numeric(0)    
    vect2<-numeric(0)
    
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
  
  data2<-x
  complete_cases<-data2[complete.cases(data2), ]
  n.cases<-nrow(complete_cases) 
  vect1 <- c(vect1, i)  
  vect2 <- c(vect2, n.cases) 
  z <- as.data.frame(cbind(id=vect1, nobs=vect2))
      }  
  else {
    vect1<-numeric(0)    
    vect2<-numeric(0)
    x<-data.frame(
      read.csv(
        paste(fullpath,'/',files[i],sep=''),header=T))
      data2<-x
      complete_cases<-data2[complete.cases(data2), ]
      n.cases<-nrow(complete_cases) 
      vect1 <- c(vect1, i)  
      vect2 <- c(vect2, n.cases) 
      z1 <- as.data.frame(cbind(id=vect1, nobs=vect2))
      z<-as.data.frame(rbind(z,z1))     
  }
} ## end for
  print(z)

}
