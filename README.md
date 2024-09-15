# predicting-variables

Prediction

There are two parts -

a) predicting numerical variable 

b) predicting categorical variable.

For prediction of numerical variable you can use lm(), rpart() or freestyle of course. For prediction of categorical variable use rpart() or freestyle. You are not allowed to use any other packages like random forest etc. 

Run cross-validation of your models using 1-step cross-validation for rpart() or for lm() from active textbook. Run it several times and report the results. 

GOAL: Benchmark for numerical variable prediction is the better of two MSEs - one by straight application of rpart() and another by straight application of lm() (that is lm(Target ~..., data=YourData)). You need to beat this benchmark MSE at least by reducing it by 50%.   Notice, that your goal is RELATIVE to benchmark MSE obtained from straight application of rpart or lm. 

For categorical variable - beat the single application of rpart() - rpart(Target~..., data=YourData) by increasing accuracy by 10% (so beat 80% by delivering 85%).  

Since there is no testing data set in our possession, we will evaluate your model by selecting a random subset of your data set. 
