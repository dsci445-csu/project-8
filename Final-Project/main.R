set.seed(1)
library(tidyverse)
library(GGally)


#w3 ten year compiled file: w3tenyear.csv
w3df<-read.csv("w3tenyear.csv")

#####################################################
#variable selection: via class resources and correlation 
#############################################
#ggpairs(df2) #wayy too much data, lets get rid of some useless variables
#these columns are just zero
w3df<-read.csv("w3tenyear.csv")
w3df = subset(w3df, select = -c(1,12,15) )
#also get rid of non-predictor columns: load and yield 
w3df=subset(w3df, select = -c(3,4))

#ggpairs(w3df)#still too crammed to get any good info

ggcorr(w3df, palette = "RdBu", label = TRUE)
#theres high correlation for a lot of variables. lets get rid of st.deviation for these 
w3df=subset(w3df, select = -c(8,10,14,15,17,18,20,21))

ggcorr(w3df, palette = "RdBu", label=TRUE)
ggpairs(w3df)
#trends from ggpairs: ph_mean, gpp_mean, precip mean, rest are kind of ambiguous 
#correlation map: get rid of month, cc_cumulative precip, and vb_fpar_mean: all have zero as a correlation
w3df=subset(w3df, select = -c(1,7,11))

ggcorr(w3df, palette = "RdBu", label=TRUE)
ggpairs(w3df)




#########################################
#random forest for w3tenyear
########################################
sample <- sample(c(TRUE, FALSE), nrow(w3df), replace=TRUE, prob=c(0.8,0.2))
train  <- w3df[sample, ]
test   <- w3df[!sample, ]

#helpful website for continuous variable prediction in r. most resources are on classification
#http://rstudio-pubs-static.s3.amazonaws.com/156481_80ee6ee3a0414fd38f5d3ad33d14c771.html

library(randomForest)
#mtry means include a certian number of variables into the trees. 9 is all variables (other load_yield_adj)
#when mtry isn't included, only 3 varaibles are used. 
dfrf=randomForest(load_yield_adj~.,data=train, mtry=9, importance=TRUE)
dfrf
#check importance of variables used. 
importance(dfrf)
#plot it 
varImpPlot(dfrf)


#prediction
preddf = predict(dfrf,newdata=test)
#plot trend
plot(preddf, test$load_yield_adj)
abline(0,1)
mean((test$load_yield_adj-preddf)^2)#mse here is extremely high


#lets try bagging and boosting
library(MASS)
library(tree)

#bagging
bag=randomForest(load_yield_adj~.,data=train,mtry=9,importance=TRUE)
bag
yhat.bag = predict(bag,newdata=test)
plot(yhat.bag, test$load_yield_adj)
abline(0,1)
mean((test$load_yield_adj-yhat.bag)^2)#mse here is extremely high
#boosting
library(gbm)
boost=gbm(load_yield_adj~.,data=train,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost)
yhat.boost=predict(boost,newdata=test,n.trees=5000)
mean((yhat.boost-test$load_yield_adj)^2)#mse extremely high 

################################################
#gradient boosting with lower rmse. 
#cv doesn't work on here, got a weird error, but cv works on my locaal r studio with lower RMSE
##################################################
library(gbm)

#################################
#gbm finally shows smaller rmse

test_x = test[, -3] # feature and target array
test_y = test[, 3] 
model_gbm = gbm(load_yield_adj ~.,
                data = train,
                cv.folds = 10)

print(model_gbm)

summary(model_gbm)

pred_y = predict.gbm(model_gbm, test_x)
pred_y
residuals = test_y - pred_y
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

y_test_mean = mean(test_y)
# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

# visualize the model, actual and predicted data
x_ax = 1:length(pred_y)
plot(x_ax, test_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_y, col="red", pch=20, cex=.9) 


