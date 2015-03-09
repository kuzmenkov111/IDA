wconfmat <- function(pred.y, true.y, w=rep(1, length(true.y)))
{ weighted.table(pred.y, true.y, w=w, dnn=c("predicted", "true")) }


if (FALSE)
{

  # double weight for the brown-spot class
s01.w1test <- ifelse(s01.test$Class=="brown-spot", 2, 1)
s01.w1cm <- wconfmat(predict(s01.tree, s01.test, type="c"),
                     s01.test$Class, s01.w1test)
tpr(s01.w1cm)
fpr(s01.w1cm)

  # 10 times less weight for instances with plant.stand=1
s01.w2test <- ifelse(!is.na(s01.test$plant.stand) & s01.test$plant.stand=="1",
                     0.1, 1)
s01.w2cm <- wconfmat(predict(s01.tree, s01.test, type="c"),
                     s01.test$Class, s01.w2test)
tpr(s01.w2cm)
fpr(s01.w2cm)

}
