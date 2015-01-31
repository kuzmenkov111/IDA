library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(mlr)
data(stagec)
dat <- transform(stagec, progstat=as.factor(ifelse(stagec$pgstat==0,"No","Prog")))
dat <- subset(dat, select=c(-pgtime,-pgstat))

cfit <- rpart(progstat ~ ., data=dat, method="class")

cols <- ifelse(cfit$frame$yval == 1, "darkred", "green4") # green if survived
prp(cfit, main="assorted arguments",
    extra=106,           # display prob of survival and percent of obs
    nn=TRUE,             # display the node numbers
    fallen.leaves=TRUE,  # put the leaves on the bottom of the page
    branch=.5,           # change angle of branch lines
    faclen=0,            # do not abbreviate factor levels
    trace=1,             # print the automatically calculated cex
    shadow.col="gray",   # shadows under the leaves
    branch.lty=3,        # draw branches using dotted lines
    split.cex=1.2,       # make the split text larger than the node text
    split.prefix="is ",  # put "is " before split text
    split.suffix="?",    # put "?" after split text
    col=cols, border.col=cols,   # green if survived
    split.box.col="lightgray",   # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5)    

# expected loss: 0.15 = 9/16 (0.1475410)
# prob(node): 42% (61/146)

# pfit <- as.party(cfit)
# plot(pfit)

task = makeClassifTask(data=dat, target="progstat")
lrn = makeLearner("classif.rpart")
mod = train(lrn,task)

#########
data(cu.summary)
dat = cu.summary
str(dat)

fit1 = rpart(Reliability ~ ., data=dat, parms=list(split="gini"))
fit2 = rpart(Reliability ~ ., data=dat, parms=list(split="information"))
par(mfrow=c(1,2), mar=rep(0.1,4))
plot(fit1, margin=0.05)
text(fit1, use.n=TRUE, cex=0.8)
plot(fit2, margin=0.05)
text(fit2, use.n=TRUE, cex=0.8)







