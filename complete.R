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
complete <- function(directory, id = 1:332) {
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
    completos<-replicate(length(id),0)
    idc<-replicate(length(id),0)
    idfix<-sapply(id,addzeros)
    file <-paste(getwd(),"/",directory,"/",as.character(idfix),".csv",sep="")
    for(i in 1:length(file)){
      data <- read.csv(file[i])
      completos[i] <- sum(!is.na(data$nitrate)&!is.na(data$sulfate))
      idc[i] <- as.numeric(idfix[i])
    }
    out<-data.frame(idc,completos)
    colnames(out)<-c("id","nobs")
    return(out)
}
