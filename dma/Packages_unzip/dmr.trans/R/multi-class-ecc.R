## error-correcting multiclass encoding and decoding function generator
## for k classes with up to maxbits bits
multi.ecc <- function(k, maxbits=Inf)
{
  code <- sapply(1:k, ecc, k)
  if (nrow(code)>maxbits)
    code <- code[sample.int(nrow(code), maxbits),]

  enc <- function(d, class, clabs=levels(d))
  {
    `colnames<-`(t(code[,d]), paste(class, 1:nrow(code), sep = "."))
  }

  dec <- function(pred, clabs)
  {
    clabs[apply(pred, 1, function(p) which.min(colSums(p!=code)))]
  }
  list(enc=enc, dec=dec)
}


if (FALSE)
{

  # error correcting encoding/decoding for 4 and 6 classes
multi.ecc4 <- multi.ecc(4)
multi.ecc6 <- multi.ecc(6)

  # error-correcting encoding applied to rpart
rp.e4 <- multi.class(rpart, predf=function(...) predict(..., type="c"),
                     encode=multi.ecc4$enc, decode=multi.ecc4$dec)
rp.e6 <- multi.class(rpart, predf=function(...) predict(..., type="c"),
                     encode=multi.ecc6$enc, decode=multi.ecc6$dec)
v.tree.e <- rp.e4$alg(Class~., v.train)
v.tree.e.pred <- rp.e4$predict(v.tree.e, v.test)
g.tree.e <- rp.e6$alg(Type~., g.train)
g.tree.e.pred <- rp.e6$predict(g.tree.e, g.test)

  # error-correcting encoding applied to naive Bayes
nb.e4 <- multi.class(naiveBayes, encode=multi.ecc4$enc, decode=multi.ecc4$dec)
nb.e6 <- multi.class(naiveBayes, encode=multi.ecc6$enc, decode=multi.ecc6$dec)

v.nb.e <- nb.e4$alg(Class~., v.train)
v.nb.e.pred <- nb.e4$predict(v.nb.e, v.test)
g.nb.e <- nb.e6$alg(Type~., g.train)
g.nb.e.pred <- nb.e6$predict(g.nb.e, g.test)

}
