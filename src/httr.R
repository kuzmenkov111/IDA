library(XML)
library(httr)
user <- "dottami@gmail.com"
pass <- ""
authURL <- "http://openml.org/api/?f=openml.authenticate"
auth <- POST(url, body = list(username = user, password = pass), encode = "form", verbose())

r <- GET(authURL, authenticate(user, pass))

r <- GET("http://httpbin.org/get")

r <- GET("http://httpbin.org/cookies/set", query = list(a = 1))
cookies(r)

r <- GET("http://httpbin.org/cookies/set", query = list(b = 1))
cookies(r)

r <- GET("http://httpbin.org/get", query = list(key1 = "value1", key2 = "value2"))
content(r)$args

r <- GET("http://httpbin.org/get", add_headers(Name = "Jaehyeon"))
str(content(r)$headers)

r <- GET("http://httpbin.org/cookies", set_cookies("MeWant" = "cookies"))
content(r)$cookies

r <- POST("http://httpbin.org/post", body = list(a = 1, b = 2, c = 3))

url <- "http://httpbin.org/post"
body <- list(a = 1, b = 2, c = 3)
r <- POST(url, body = body, encode = "multipart", verbose()) # default
r <- POST(url, body = body, encode = "form", verbose())
r <- POST(url, body = body, encode = "json", verbose())