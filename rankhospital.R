rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        attack = suppressWarnings(as.numeric(outcomedata[, 11]))
        failure = suppressWarnings(as.numeric(outcomedata[, 17]))
        pneumonia = suppressWarnings(as.numeric(outcomedata[, 23]))
        hospital = outcomedata[, 2]
        
        ## Check that state and outcome are valid
        states = names(table(outcomedata$State)) # vector of states
        validoutcomes = c("heart attack","heart failure","pneumonia")
        x = length(as.numeric(which(state == states)))
        if(x==0) stop ("invalid state")
        y = length(as.numeric(which(outcome == validoutcomes)))
        if(y==0) stop ("invalid outcome")
        
        ## Return hospital name in that state with the given rank
        
        if(outcome == 'heart attack'){
                start = cbind(outcomedata$State,attack,hospital)
                mask = complete.cases(start)
                complete = as.data.frame(start)[mask,]
                names(complete) = c('st','value','hospital')
                subsetted = subset(complete,state==st)    
                sorted = subsetted[order(as.numeric(levels(subsetted$value)[subsetted$value]),subsetted$hospital),]
                if(num =="best"){num = 1}
                else if (num == "worst"){num = nrow(sorted)}
                else {num = num}
                answer = as.character(sorted[num,3])
        } 
        
        else if(outcome == 'heart failure'){ 
                start = cbind(outcomedata$State,failure,hospital)
                mask = complete.cases(start)
                complete = as.data.frame(start)[mask,]
                names(complete) = c('st','value','hospital')
                subsetted = subset(complete,state==st)    
                sorted = subsetted[order(as.numeric(levels(subsetted$value)[subsetted$value]),subsetted$hospital),]
                if(num =="best"){num = 1}
                else if (num == "worst"){num = nrow(sorted)}
                else {num = num}
                answer = as.character(sorted[num,3])
        }
        
        else if(outcome=='pneumonia') { 
                start = cbind(outcomedata$State,pneumonia,hospital)
                mask = complete.cases(start)
                complete = as.data.frame(start)[mask,]
                names(complete) = c('st','value','hospital')
                subsetted = subset(complete,state==st)    
                sorted = subsetted[order(as.numeric(levels(subsetted$value)[subsetted$value]),subsetted$hospital),]
                if(num =="best"){num = 1}
                else if (num == "worst"){num = nrow(sorted)}
                else {num = num}
                answer = as.character(sorted[num,3])
        }
        print(answer)
}
