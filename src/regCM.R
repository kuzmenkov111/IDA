source("src/mlUtils.R")
set.seed(1)
data = rnorm(100,0,1)
fit = rnorm(100,1,1)
probs = c(0.1,0.25,0.75)

actual = data
fitted = fit
probs = sort(probs)
# update 0 and 1 if necessary
if(probs[1]>0) probs = c(0,probs)
if(probs[length(probs)]<1) probs = c(probs,1)
quan = quantile(actual, probs, na.rm=TRUE)
actualCnt = data.frame(probs=c(),count=c())
for(i in 2:length(quan)) {
  prob = paste0(probs[i]*100,"%")
  if(i==2) {
    cnt = length(actual[actual >= quan[i-1] & actual <= quan[i]])
  } else {
    cnt = length(actual[actual > quan[i-1] & actual <= quan[i]])
  }
  actualCnt = rbind(actualCnt,data.frame(prob,cnt))
}    
fittedCnt = data.frame(probs=c(),count=c())
for(i in 2:length(quan)) {
  prob = paste0(probs[i]*100,"%")
  if(i==2) {
    cnt = length(fitted[fitted >= quan[i-1] & fitted <= quan[i]])
  } else {
    cnt = length(fitted[fitted > quan[i-1] & fitted <= quan[i]])
  }
  fittedCnt = rbind(fittedCnt,data.frame(prob,cnt))
}  

table(actualCnt,fittedCnt)



regCM = function(actual, fitted, probs, type="Pred") {
  if(length(probs) < 1) {
    message("probs not set")
  } else if(class(probs[1]) != "numeric") {
    message("probs not numeric")
  } else if(max(probs) > 1 | min(probs) < 0) {
    message("probs not in valid range")
  } else {    
    probs = sort(probs)
    # update 0 and 1 if necessary
    if(probs[1]>0) probs = c(0,probs)
    if(probs[length(probs)]<1) probs = c(probs,1)
    # update counts in each interval
    quan = quantile(actual, probs, na.rm=TRUE)
    
    actualCnt = data.frame(probs=c(),count=c())
    for(i in 2:length(quan)) {
      prob = paste0(probs[i]*100,"%")
      if(i==2) {
        cnt = length(actual[actual >= quan[i-1] & actual <= quan[i]])
      } else {
        cnt = length(actual[actual > quan[i-1] & actual <= quan[i]])
      }
      actualCnt = rbind(actualCnt,data.frame(prob,cnt))
    }    
    fittedCnt = data.frame(probs=c(),count=c())
    for(i in 2:length(quan)) {
      prob = paste0(probs[i]*100,"%")
      if(i==2) {
        cnt = length(fitted[fitted >= quan[i-1] & fitted <= quan[i]])
      } else {
        cnt = length(fitted[fitted > quan[i-1] & fitted <= quan[i]])
      }
      fittedCnt = rbind(fittedCnt,data.frame(prob,cnt))
    }    
    #cm = table(actualCnt,fittedCnt)  
  }
  #cm
}






