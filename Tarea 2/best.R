
best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## Rate
        state_list<-state.abb
        outcome_list<-c("heart attack","heart failure","pneumonia")
        scheck<-length(state_list[state_list==state])
        ocheck<-length(outcome_list[outcome_list==outcome])
        if (scheck==0)
                stop("invalid state")
        if (ocheck==0)
                stop("invalid outcome")
        data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
        data<-data[c(7,2,11,17,23)]
        outcome_index<-match(outcome,outcome_list)+2
        x<-data[,1]==state & data[,outcome_index]!="Not Available"
        
        y<-data[x,outcome_index]==as.character( min( as.numeric(data[x,outcome_index] ) ) )
        z<-data[x,2][y]
        return(z[1])
}