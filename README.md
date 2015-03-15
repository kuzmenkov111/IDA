A repo to research and implement techniques for intelligent data analysis (IDA). Here IDA is a generic term that covers machine learning, statistical analysis, data mining, statistical learning, predictive analytics, econometrics, actuarial modelling ...

R is used to implement these techniques using the following wrappers or frameworks: 
- [mlr](http://berndbischl.github.io/mlr/tutorial/html/index.html) package ([GitHub repo](https://github.com/berndbischl/mlr))
- [caret](http://topepo.github.io/caret/index.html) package ([GitHub repo](https://github.com/topepo/caret))
- [forecast](https://www.otexts.org/fpp/2/a) package for time series analysis ([GitHub repo](https://github.com/robjhyndman/forecast))

Some of the outputs are going to be shared in [my blog](http://jaehyeon-kim.github.io/).

Update as of 2015-03-15

Recently I become interested in R package development. Specifically I'm planning to develop a package that implements popular machine learning algorithms where the two most noticeiable weaknesses of base R is overcome - these weeknesses are single-threading and memory limitation.

Although multi-threading is not difficult to achieve, provided that multiple cores are equipped, and existing R libraries can be utilized as usual, it has to be adopting distributed computing if both the weaknesses can be tackled down. There are a couple of tools/solutions such as H2O, RHadoop, RHipe and Distribute R, none of them provides a comprehensive set of algorithms yet as far as I've searched.

I'm going to develop algorithmes that supports [Apache Spark](http://spark.apache.org/) through the [SparkR](https://github.com/amplab-extras/SparkR-pkg) package, which provides the RDD API of Apache Spark. For algorithms, I find [Data Mining Algorithms: Explained Using R](http://au.wiley.com/WileyCDA/WileyTitle/productCd-111833258X.html) by Pawel Cichosz useful.

This repo will still be used as my main playground until a separate package is created.