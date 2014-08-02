rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        attack = suppressWarnings(as.numeric(outcomedata[, 11]))
        failure = suppressWarnings(as.numeric(outcomedata[, 17]))
        pneumonia = suppressWarnings(as.numeric(outcomedata[, 23]))
        hospital = outcomedata[, 2]
        states = names(table(outcomedata$State)) # vector of states
        
        ## test data
        # num = "best"
        # outcome = "heart attack"
        # num = 20
        
        ## Check that state and outcome are valid
        validoutcomes = c("heart attack","heart failure","pneumonia")
        y = length(as.numeric(which(outcome == validoutcomes)))
        if(y==0) stop ("invalid outcome")
        
        ## initialize dframe
        results <- data.frame(statename = c(), hospname=c())
        
        ## For each state, find the hospital of the given rank
        if(outcome == 'heart attack'){
                for(n in states){
                        start = cbind(outcomedata$State,attack,hospital)
                        mask = complete.cases(start)
                        complete = as.data.frame(start)[mask,]
                        names(complete) = c('st','value','hospital')
                        subsetted = subset(complete,st==n)    
                        sorted = subsetted[order(as.numeric(levels(subsetted$value)[subsetted$value]),subsetted$hospital),]
                        if(num =="best"){index = 1} else if (num == "worst"){index = nrow(sorted)} else {index = num}
                        result = cbind(as.character(sorted[index,3]),as.character(n))
                        results = rbind(results,result)                            
                }
                names(results)=c("hospital","state")  
                return(results)
        }
        
        else if(outcome == 'heart failure'){
                for(n in states){
                        start = cbind(outcomedata$State,failure,hospital)
                        mask = complete.cases(start)
                        complete = as.data.frame(start)[mask,]
                        names(complete) = c('st','value','hospital')
                        subsetted = subset(complete,st==n)    
                        sorted = subsetted[order(as.numeric(levels(subsetted$value)[subsetted$value]),subsetted$hospital),]
                        if(num =="best"){index = 1} else if (num == "worst"){index = nrow(sorted)} else {index = num}
                        result = cbind(as.character(sorted[index,3]),as.character(n))
                        results = rbind(results,result)                            
                }
                names(results)=c("hospital","state")  
                return(results)
        }
        
        else if(outcome=='pneumonia') {
                for(n in states){
                        start = cbind(outcomedata$State,pneumonia,hospital)
                        mask = complete.cases(start)
                        complete = as.data.frame(start)[mask,]
                        names(complete) = c('st','value','hospital')
                        subsetted = subset(complete,st==n)    
                        sorted = subsetted[order(as.numeric(levels(subsetted$value)[subsetted$value]),subsetted$hospital),]
                        if(num =="best"){index = 1} else if (num == "worst"){index = nrow(sorted)} else {index = num}
                        result = cbind(as.character(sorted[index,3]),as.character(n))
                        results = rbind(results,result)                            
                }

                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                names(results)=c("hospital","state")  
                return(results)
        }
        
}  

