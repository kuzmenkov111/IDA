source("src/mlUtils.R")
set.seed(1)
data = rnorm(20,0,1)
fit = rnorm(20,1,1)
probs = c(0.5)

getRegCM = function()

probs = sort(probs)
probsUp = probs
if(probsUp[1]>0) probsUp = c(0,probsUp)
if(probsUp[length(probsUp)]<1) probsUp = c(probsUp,1)
qval = quantile(data, probsUp, na.rm=TRUE)

ins = c()
for(i in 1:length(data)) {
  for(j in 1:length(probsUp)) {
    if(data[i] <= qval[j]) {
      ins = c(ins,probsUp[j])
      break
    }
  }
}

conv = function(ins, probs) {
  outs = c()
  if(length(probs)==1) {    
    tmp = ins[ins<=probs[1]]
    tmp[tmp<=probs[1]] = probs[1]
    tmp = paste0(tmp*100,"%-")
    outs = c(outs,tmp)
    
    tmp = ins[ins>probs[1]]
    tmp[tmp>probs[1]] = probs[1]
    tmp = paste0(tmp*100,"%+")
    outs = c(outs,tmp)
  } else {
    
  }
  outs
}

conv(data,probs)





outs = c()
tmp = ins[ins<=probs[1]]
tmp[tmp<=probs[1]] = probs[1]
tmp = paste0(tmp*100,"%-")
outs = c(outs,tmp)

tmp = ins[ins>probs[1]]
tmp[tmp>probs[1]] = probs[1]
tmp = paste0(tmp*100,"%+")
outs = c(outs,tmp)

table(outs,outs)

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










getRegCM = function(actual, fitted, probs, type="Pred") {
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
  actualConv
}
