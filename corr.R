addzeros <- function(value){
  l<-nchar(as.character(value))
  if(l==1){
    return(paste("00",as.character(value),sep=""))
  } else if(l==2){
    return(paste("0",as.character(value),sep=""))
  }else if(l==3){
    return(as.character(value))
  }
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  id<-1:332
  correlacion<-vector()
  completos<-replicate(length(id),0)
  idc<-replicate(length(id),0)
  idfix<-sapply(id,addzeros)
  a<-0
  j<-1
  file <-paste(getwd(),"/",directory,"/",as.character(idfix),".csv",sep="")
  
  for(i in 1:length(file)){
    data <- read.csv(file[i])
    x<- !is.na(data$nitrate)&!is.na(data$sulfate)
    completos[i] <- sum(x)
    idc[i] <- as.numeric(idfix[i])
    if(sum(x)>threshold){
      correlacion[j]<- cor(data$nitrate[x],data$sulfate[x])
      j<-j+1
    }
    
 
  }
  out<-data.frame(idc,completos)
  colnames(out)<-c("id","nobs")
  return(correlacion)
  
}