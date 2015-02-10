source("src/mlUtils.R")
set.seed(1)
data = rnorm(100,0,1)
fit = rnorm(100,1,1)
probs = c(0.9)

getRegCM = function(actual, fitted, probs, type="Pred") {
  if(length(probs) < 1) {
    message("probs not set")
  } else if(class(probs[1]) != "numeric") {
    message("probs not numeric")
  } else if(max(probs) > 1 | min(probs) < 0) {
    message("probs not in valid range")
  } else {    
    conv = function(data, probs) {
      probsUp = sort(probs)
      # update 0 and 1 if necessary
      if(probsUp[1]>0) probsUp = c(0,probsUp)
      if(probsUp[length(probsUp)]<1) probsUp = c(probsUp,1)
      quans = quantile(actual, probsUp, na.rm=TRUE)      
      dat = c()
      for(i in 1:length(data)) {
        for(j in 1:length(probsUp)) {
          if(data[i] <= quans[j]) {
            dat = c(dat,probsUp[j])
            break
          }
        }
      }
      
      datUp = dat
      tmp1 = datUp[datUp <= probs[1]]
      tmp1 = paste0("<=",probs[1]*100,"%")
      tmp2 = datUp[datUp > probs[1]]
      tmp2 = paste0(">",probs[1]*100,"%")
      datUp = c(tmp1,tmp2)
      
      cbind(dat, datUp)
    }    
    actualConv = conv(actual, probs)
  }
  actualConv
}
