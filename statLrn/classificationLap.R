library(ISLR)
data <- Smarket
str(data)
cor(subset(data, select=-Direction))

## logistic regression
# no distinct ways to set up task and learner
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=data, family=binomial)
summary(glm.fit)

coef(glm.fit)
summary(glm.fit)$coef[,4] # summary(glm.fit)$coef

# train
glm.probs = predict(glm.fit, type="response") 
# type = c("link","response","terms"), default - "link"
contrasts(data$Direction) # check which level is 1 or 0

# prediction
glm.pred = sapply(glm.probs, function(p) { ifelse(p>.5,"Up","Down") })

# performance
glm.conf = table(glm.pred, data$Direction)
glm.conf

glm.conf.correct = sum(diag(glm.conf))
glm.conf.total = sum(glm.conf)

glm.conf.correct / glm.conf.total
mean(glm.pred==data$Direction)

# training error rate
1 - glm.conf.correct/glm.conf.total # 47.84% overly optimistic












