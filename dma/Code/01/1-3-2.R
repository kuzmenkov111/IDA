weatherc <- read.table(text="
      outlook temperature humidity   wind play
   1     sunny          27       80 normal   no
   2     sunny          28       65   high   no
   3  overcast          29       90 normal  yes
   4     rainy          21       75 normal  yes
   5     rainy          17       40 normal  yes
   6     rainy          15       25   high   no
   7  overcast          19       50   high  yes
   8     sunny          22       95 normal   no
   9     sunny          18       45 normal  yes
  10     rainy          23       30 normal  yes
  11     sunny          24       55   high  yes
  12  overcast          25       70   high  yes
  13  overcast          30       35 normal  yes
  14     rainy          26       85   high   no")

summary(weatherc)
