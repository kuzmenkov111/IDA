#####This is the R code for the LISA CART short course
#March 26, 2013

#Install and call the 'tree' package
library(tree)

#Example 1:  iris data
#The iris data is included with R

#iris.dat can be viewed in the Data field in RStudio
?iris
iris.dat<-iris

#Setosa is plotted in black
#versicolor is plotted in red
#Virginica is plotted in green
pairs(iris.dat,col=(as.factor(iris.dat$Species)),pch=16)

#Question:  Which of these species do you think will be easiest to classify based on sepal and petal measurements?
#why?

#Create a classification tree
iris.tree<-tree(Species~.,data=iris)
iris.tree

#What type of object is iris.tree?  (E.g. data frame, list, matrix, vector?)
class(iris.tree)

#Visually represent tree
plot(iris.tree,lwd=2)
text(iris.tree,cex=1.3)

#for each of the 150 flowers, the tree provides a top-to-bottem sequence of classification rules
#If the condition at each node is TRUE, that flower takes the left path, otherwise right.

#summarize iris.tree
summary(iris.tree)

#Take a closer look at the plot.
#Anything unusual about the terminal (ie bottom or final) nodes?

#Look at the tree object to locate any redundant rules
iris.tree

#Snip.tree tells R to remove all subsequent rules from a specified branch
#Use this to eliminate classification redundancies
iris.snip<-snip.tree(iris.tree,c(7,12))
plot(iris.snip,lwd=2)
title(main='Classification tree for iris data')
text(iris.snip,cex=1.3)

#build a wire frame - works well when 2 variables involved in classification
l<-seq(min(iris$Petal.Length),max(iris$Petal.Length),length=30)
w<-seq(min(iris$Petal.Width),max(iris$Petal.Width),length=30)
g<-expand.grid(x=l,y=w)

g$s<-predict(iris.snip,data.frame(
  Petal.Length=g$x,
  Petal.Width=g$y,
  Sepal.Length=0,
  Sepal.Width=0),type="class")

library(lattice)
wireframe(s~x+y,g,aspect=c(1,1),
          scales=list(cex=1,arrows=FALSE),
          lwd=2,xlab="Petal Length",
          ylab="Petal Width",zlab='Species',main='Wireframe representation of CART')

predict_frame<-data.frame(iris,prediction=predict(iris.snip,iris,type='class'))

#tabulate classifications using a confusion matrix
table(predict_frame$Species,predict_frame$prediction)

#Compare prediction a
data.frame(predict_frame$Species,predict_frame$prediction)


######################Skull example
##Tibetan skull example
skull.dat<-read.csv('C:/Users/FDI/Desktop/Tskull.csv')

dim(skull.dat)

skull.tree<-tree(as.factor(Type)~.,data=skull.dat)
skull.tree
summary(skull.tree)

#plot tree
plot(skull.tree,lwd=2)
text(skull.tree,cex=1.5)

#Snipping the tree
skull.snip<-snip.tree(skull.tree,2)
summary(skull.snip)

#plot tree
plot(skull.snip,lwd=2)
text(skull.snip,cex=1.5)

skull.snip2<-snip.tree(skull.tree,c(2,3))
summary(skull.snip2)

#plot tree
plot(skull.snip2,lwd=2)
text(skull.snip2,cex=1.5)

#confusion matrix - original tree
predict_frame<-data.frame(skull.dat,prediction=predict(skull.tree,skull.dat,type='class'))
table(predict_frame$Type,predict_frame$prediction)

#confusion matrix - snipped tree
predict_frame<-data.frame(skull.dat,prediction=predict(skull.snip2,skull.dat,type='class'))
table(predict_frame$Type,predict_frame$prediction)

#classifying new observations
skull.snip2

new.skull<-list(Length=0,Breadth=0,Fheight=0,Fbreadth=0,Height=130)

predict(skull.snip2,new.skull,type='class')

#Deviance:
#comes from confusion matrix, probabilities in tree object, and formula in slides
-2*((15*log(0.7895) + 4*log(0.2105)) + (2*log(0.1538) +  11*log(0.8462)))
summary(skull.snip2)

################################################################################
##########################Regression Trees######################################
################################################################################
#Let's demonstrate a regression tree on some simulated data

#the seed controls the random number generator
set.seed(12345657)

#X1, X2, X3 control data behavior in different regions (See plot)
X1<-runif(25,10,20) #generate predictor variables uniformly in each region
X2<-runif(10,20,29)
X3<-runif(15,29,40)
X<-c(X1,X2,X3)
Xsq<-X^2  #we will compare tree with a quadratic regression model later

#Y1, Y2, and Y3 control behavior in different regions of Y.  Normal errors (see plot).
Y1<-rnorm(25,30,5)
Y2<-rnorm(10,10,5)
Y3<-rnorm(15,50,5)
Y<-c(Y1,Y2,Y3)
plot(X,Y)

sim.dat<-list(X,Y)

#Before we were using data frames to store the data
#'tree' can accept either data frames or lists
sim.tree<-tree(Y~X)

summary(sim.tree)
plot(sim.tree)
text(sim.tree)
title('Regression tree on simulated data')

#Obtain predicted y for the range of x
sim.tree_pred<-predict(sim.tree)
sim.tree_pred

pred.frame<-data.frame(X,Yhat=sim.tree_pred)
pred.frame
sort.pred.frame<-pred.frame[order(pred.frame$X),]

par(mfrow=c(1,1),las=1)
plot(X,Y,main='Performance of CART compared to 2 regression models',axes=FALSE)
axis(1)
axis(2)

partition.tree(sim.tree,add=TRUE,lwd=2)

SLR<-lm(Y~X)
summary(SLR)
abline(summary(SLR)$coefficient[1,1],summary(SLR)$coefficient[2,1],col='red',lty=2,lwd=2)

QR<-lm(Y~X+Xsq)
summary(QR)
Xseq<-seq(9,41,.01)

lines(Xseq, (summary(QR)$coefficient[1,1] + summary(QR)$coefficient[2,1]*Xseq + summary(QR)$coefficient[3,1]*Xseq^2 ),lwd=2,lty=3,col='blue' )

legend(10,58,legend=c('Regression tree','Simple linear regression','Quadratic regression'),lty=c(1,2,3),col=c(1,2,4),lwd=2)

#Is SS the same as deviance?
summary(sim.tree)
#residual SS
sum((predict(sim.tree,sim.dat)-sim.dat[2][[1]])^2)/47


#Cars example
##create a data set for motor trend car data
cars.dat<-mtcars

#learn about the data set
?mtcars

#This allows us to call the variables in the data set without needing to preface them with 'cars.dat$'
attach(cars.dat)

class(cars.dat)

#Quick plot of all variables
pairs(cars.dat,pch=16)

#EDA
#create a twelve paneled plot, 4 rows three columns
par(mfrow=c(4,3))

#plot everything by miles per gallon
plot(as.factor(am),mpg,ylab='mpg',xlab='am',main='automatic transmission')
plot(carb,mpg,ylab='mpg',xlab='carb',main='Carburator?')
plot(as.factor(cyl),mpg,ylab='mpg', xlab='cyl',main='Number cylinders')
plot(disp,mpg,ylab='mpg',xlab='disp',main='Engine displacement')
plot(drat,mpg,ylab='mpg',xlab='drat',main='Distributed Raman ... transm.?')
plot(as.factor(gear),mpg,ylab='mpg',xlab='gear',main='# gears')
plot(hp,mpg,ylab='mpg',  xlab='hp',main='Horsepower')
plot(mpg,mpg,ylab='mpg', xlab='mpg',main='miles per gallon')
plot(qsec,mpg,ylab='mpg',xlab='qsec',main='qseq')
plot(as.factor(vs),mpg,ylab='mpg',  xlab='vs',main='?')
plot(wt,mpg,ylab='mpg',  xlab='wt',main='Weight')

#Make the tree
cars.tree<-tree(mpg~carb+ cyl+ disp+ drat+ gear+ hp+ qsec+ vs+ wt,data=cars.dat)
class(cars.tree)


par(mfrow=c(1,1))
plot(cars.tree)
text(cars.tree)
title('Regression tree on simulated data')


summary(cars.tree)
