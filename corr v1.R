corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        flag<-F
        v <- complete(directory)
        for(i in seq_len(nrow(v))){
        	if(v$nobs[i] > threshold){
        		data<-getmonitor(i,directory)
        		if(flag==T){
        			out<-append(out,cor(data$sulfate,data$nitrate, use = "na.or.complete"))
        			}
        		if(flag==F){
        		  out<-cor(data$sulfate,data$nitrate, use = "na.or.complete")
        		  flag<-T
        		  }
        		}
        }
if(flag==F){
	out<-NULL
	}
return(out)
}
