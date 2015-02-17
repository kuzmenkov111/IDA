attr(x,"class")
inherits(x,"integer")
inherits(x,"matrix")

x = list(name="Josephine Biologist"
         ,origin="SEA"
         ,destination="XYZ")
class(x) = "Passenger"
y = list(name="Josephine Physicist"
         ,origin="SEA"
         ,destination="YVR"
         ,ffnumber=10)
class(y) = c("FreqFlyer","Passenger")

inherits(x,"Passenger")
inherits(x,"FreqFlyer")
inherits(y,"FreqFlyer")

is.object(x) # has class attribute

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())

### expression data example in S3
ex1VL = c("Sex, M=MALE, F=FEMALE", "Age in years")
names(ex1VL) = c("Sex","Age")
class(ex1VL) = "VARLS3"

set.seed(123)
simExprs = matrix(rgamma(1000,500),nc=10,nr=100)
simS = sample(c("M","F"), 10, rep=TRUE)
simA = sample(30:45, 10, rep=TRUE)
simPD = data.frame(Sex=simS, Age=simA)

new.EXPRS3 = function(Class, eData, pData, cDesc) {
  if(!is.matrix(eData)) stop("invalid expression data")
  if(!is.data.frame(pData)) stop("invalid phenotypic data")
  if(!inherits(cDesc,"VARLS3")) stop("invalid cov description")
  if(ncol(eData) != nrow(pData)) stop("incorrect dimensions")
  
  pD = list(pData=pData, varLabels=cDesc)
  class(pD) = "PHENODS3"
  ans = list(exprs=eData, phenoData=pD)
  class(ans) = class(pD)
  ans
}

myES3 = new.EXPRS3("EXPRS3", simExprs, simPD, ex1VL)
myES3

### Generic
fun = function(x, ...) UseMethod("fun")
fun.default = function(x, ...) print("In the default method")
fun(2)

fun.Foo = function(x) {
  print("start of fun.Foo")
  NextMethod()
  print("end of fun.Foo")
}

fun.Bar = function(x) {
  print("start of fun.Bar")
  NextMethod()
  print("end of fun.Bar")
}

x = 1
class(x) = c("Foo","Bar")
fun(x)
















