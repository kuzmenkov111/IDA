weatherr <- read.table(text="
       outlook temperature humidity   wind playability
   1     sunny          27       80 normal        0.48
   2     sunny          28       65   high        0.46
   3  overcast          29       90 normal        0.68
   4     rainy          21       75 normal        0.52
   5     rainy          17       40 normal        0.54
   6     rainy          15       25   high        0.47
   7  overcast          19       50   high        0.74
   8     sunny          22       95 normal        0.49
   9     sunny          18       45 normal        0.64
  10     rainy          23       30 normal        0.55
  11     sunny          24       55   high        0.57
  12  overcast          25       70   high        0.68
  13  overcast          30       35 normal        0.79
  14     rainy          26       85   high        0.33")

summary(weatherr)
