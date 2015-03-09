weather <- read.table(text="
      outlook temperature humidity   wind play
   1    sunny         hot     high normal   no
   2    sunny         hot     high   high   no
   3 overcast         hot     high normal  yes
   4    rainy        mild     high normal  yes
   5    rainy        cold   normal normal  yes
   6    rainy        cold   normal   high   no
   7 overcast        cold   normal   high  yes
   8    sunny        mild     high normal   no
   9    sunny        cold   normal normal  yes
  10   rainy        mild   normal normal  yes
  11    sunny        mild   normal   high  yes
  12 overcast        mild     high   high  yes
  13 overcast         hot   normal normal  yes
  14    rainy        mild     high   high   no")

summary(weather)
