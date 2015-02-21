## intro
# construct base class
# http://www.mat.uc.cl/~susana/CE/oopV2.pdf
emp = function(name) {
  if(length(name) != 1 && !is.character(name))
    stop("single string value of name necessary")
  # assign name to result
  result = name
  class(result) = "employee"
  return(result)
}

# extend base class
# manager 'is-a' employee
man = function(name, members) {
  # create base class (employee)
  result = emp(name)
  # arguments should be carefully checked
  if(length(sapply(members,class)[sapply(members,class)=="character"]) != length(members))
    stop("a vector employees necessary")
  # combine name and members as a list
  result = list(name=result,members=members)
  class(result)=c("manager","employee")
  return(result)
}

tom = emp("Tom")
alice = emp("Alice")
nick = man("Nick",c(tom,alice))

# without print function
tom
nick

# base method can be extended
# better not to include full stop(.) in class name
print.employee = function(obj) {
  cat("Employee Name:",obj,"\n")
}

print.manager = function(obj) {
  cat("Employee Name:",obj$name,"\n")
  cat("Members:",obj$members,"\n")
}

# methods that extends print
head(methods("print"),3)
methods("print")[grep("*employee",methods("print"))]

# methods that associated with manager class
methods(class="employee")

# with print function
tom
nick

# class can be checked/modified
class(tom) = NULL
tom # character vector

# list names and assigned classes can be checked/modified
attributes(nick)
attributes(nick) = NULL
nick

# incorrectly assigned class should be handled
foo = c(1)
class(foo) = "employee"
class(foo)
man("John",foo)