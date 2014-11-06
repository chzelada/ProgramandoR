
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
getmonitor <- function(directory=getwd(), pullutant="nitrate", id=1:332 ) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
        datax=replicate(length(id),0)
        idfix<-sapply(id,addzeros)
        a<-0
        file <-paste(getwd(),"/",directory,"/",as.character(idfix),".csv",sep="")
        
      
        for(i in 1:length(file)){
        	data <- read.csv(file[i])
        	datax[i] <- sum(data[pollutant][!is.na(data[pollutant])])
        	a <- a+length(data[pollutant][!is.na(data[pollutant])])
        }
       
     return(sum(datax)/a)
        	
}
