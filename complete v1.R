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
        flag<-F
        for(i in id){
        	x<-nrow(na.omit(getmonitor(i,directory,F)))
        	
        	if(flag==T){
        	M<-rbind(M,c(i,x))
        	}
        	if(flag==F){
        		M <- matrix(c(i,x),ncol=2)
        		flag<-T
        		}		
        	}
        	DATA <- data.frame(M)
        	colnames(DATA) <- c('id','nobs')
       	return(DATA)
}
