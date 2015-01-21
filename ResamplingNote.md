
### Notes on data split

#### single, independent test set is not recommended
- single model evaluation is limited to characterize undertainty in results
- proportionally large test sets tend to divide data in a way to increase bias
- with small data sizes
-- model may need all records to adequately determine model values
-- uncertainty of test set can be high

#### resampling methods can produce reasonable predictions
- with small sample sizes
-- repeated 10-fold cross-validation is recommended
-- good bias and variance property
-- computational cost is not large
- for choosing between models (not best performance indicator)
-- bootstrap strategies can be used for low variance
- with large sample sizes
-- differences between resampling strategies less propound
-- computational efficiency becomes more important
-- 10-fold cross-validation should provide acceptable bias-variance profile

#### potential bias when estimating model performance during parameter tuning

### example in Ch 4 of [Applied Predictive Modeling](http://appliedpredictivemodeling.com/)

#### selecting hyper- (or tuning) parameter
- German credit scoring data
- baseline accuracy rate to beat: 70% (70% good, 30% bad)
- non-linear support vector machine with candiate hyperparameters from 2^2 to 2^7
- repeated cross validation - 5 times 10-fold cross-validation
- chossing parameter
-- 'one-standard error' - from numerically best case, accept the simplest case within one standard error of accuracy (eg mean accuracy 75% at 8 with 0.7% std, select the simplest no lower than 75%-0.7%)
-- set acceptable loss of accuracy (4%), accpet the simplest no lower than 75% - 4%

#### choosing between models
- begin with several most flexible models and then try simpler models
- consider simpler models that can approximate more complex models
- paired comparison (eg t-test for mean difference) can be used
- sensitivity and specificity can be used if binary response
