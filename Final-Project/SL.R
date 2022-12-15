library(ggally)
library(tidyverse)
library(GGally)
#parsing data down to readable files 
w<-read.csv(file = 'w3.csv')
#filter by year: i tried to write a loop to do this for however many years we want but 
#that was taking too long. Input the year you want, save as dfyear, and compile all df's to one dataframe

w1<- w %>% filter(
  wy %in% c("2021" )
)
#get rid of columns with greater than 50% na in them: 
#this isn't super important, since we need to remove all na for random forest, but 
#i was curious to see how much data is missing and if there are just certain months/years with missing data
w1<-w1[, which(colMeans(!is.na(w1)) > 0.5)]
#get rid of last two columns: data not useful
w1 = subset(w1, select = -c(28,29) )
#average values within each month for each column 
df2021<-aggregate(. ~ month, w1, mean)

#etc. 
#need bind rows because of some missing data. 
df2<-bind_rows(df2021,df2020, df2019,df2018,df2017,df2016,df2015,df2014,df2013,df2012, df2011)

#get rid of columns with greater than 50% na in them: again just to see how the dataset changes, not important
df2<-df2[, which(colMeans(!is.na(df2)) > 0.5)]

##################################################################################
#look at past 10 years for all months
##################################################################################
#get rid of na data 
df2<-df2 %>% select_if(~ !any(is.na(.)))
#save file so i don't have to compile the years again 
#write.csv(df2,"w3tenyear.csv")











library(tidyverse)
library(tidymodels)
library(here)

data1 <- read_csv(here('out/GREEN4.csv')) %>%
  select(wy, month, load_yield_adj, yield, pH_mean, gpp_mean, precip_mean) %>%
  mutate(site = 'Colorado')
data2 <- read_csv(here('out/w3.csv')) %>%
  select(wy, month, load_yield_adj, yield, pH_mean, gpp_mean, precip_mean) %>%
  mutate(site = 'Vermont')
data3 <- read_csv(here('out/GSMACK.csv')) %>%
  select(wy, month, load_yield_adj, yield, pH_mean, gpp_mean, precip_mean) %>%
  mutate(site = 'California')

data <- rbind(data1, data2, data3) %>%
  mutate(month_adj = ifelse(month == 10, 1, 
                            ifelse(month == 11, 2,
                                   ifelse(month == 12, 3, 
                                          ifelse(month == 1, 4, 
                                                 ifelse(month == 2, 5, 
                                                        ifelse(month == 1, 4, 
                                                               ifelse(month == 3, 6, 
                                                                      ifelse(month == 4, 7, 
                                                                             ifelse(month == 5, 8, 
                                                                                    ifelse(month == 6, 9, 
                                                                                           ifelse(month == 7, 10, 
                                                                                                  ifelse(month == 8, 11, 
                                                                                                         ifelse(month == 9, 12, month)))))))))))))) %>%
  select(-month)

pre_caa <- data %>%
  filter(wy < 1990,
         wy > 1987) %>%
  select(-wy)

pre_caa %>%
  select(-site) %>%
  pairs() %>%
  ggsave(filename = here('out/pre_pairs.png'))

post_caa <- data %>%
  filter(wy < 2013,
         wy > 2010) %>%
  select(-wy)

post_caa %>%
  select(-site) %>%
  pairs()
#ggsave(filename = here('out/post_pairs.png'))

write.csv(post_caa,"post_caa.csv")
write.csv(pre_caa,"pre_caa.csv")












##################
#variable selection: via class resources and correlation 
###################
#ggpairs(df2) #wayy too much data, lets get rid of some useless variables
#these columns are just zero
df2<-read.csv("w3tenyear.csv")

df2 = subset(df2, select = -c(1,12,15) )
#also get rid of non-predictor columns: load and yield 
df2=subset(df2, select = -c(3,4))

#ggpairs(df2)#still too crammed to get any good info

ggcorr(df2, palette = "RdBu", label = TRUE)
#theres high correlation for a lot of variables. lets get rid of st.deviation for these 
df2=subset(df2, select = -c(8,10,14,15,17,18,20,21))

#ggcorr(df2, palette = "RdBu", label=TRUE)
#ggsave(filename = here('corrheatmap.png'))
ggpairs(df2)

#trends from ggpairs: ph_mean, gpp_mean, precip mean, rest are kind of ambiguous 
#correlation map: get rid of month, cc_cumulative precip, and vb_fpar_mean: all have zero as a correlation
df2=subset(df2, select = -c(1,7,11))
#now that variable selection is complete, can move on to ML methods

###############
#ML code 
###############

#split code 80% train 20%test
post_sample <- sample(c(TRUE, FALSE), nrow(post_caa), replace=TRUE, prob=c(0.8,0.2))
post_train  <- post_caa[post_sample, ]
post_test   <- post_caa[!post_sample, ]

pre_sample <- sample(c(TRUE, FALSE), nrow(pre_caa), replace=TRUE, prob=c(0.8,0.2))
pre_train  <- pre_caa[pre_sample, ]
pre_test   <- pre_caa[!pre_sample, ]

#random forest#
###############
#http://rstudio-pubs-static.s3.amazonaws.com/156481_80ee6ee3a0414fd38f5d3ad33d14c771.html

library(randomForest)
set.seed(1)
#mtry means include a certian number of variables into the trees. 9 is all variables (other load_yield_adj)
#when mtry isn't included, only 3 varaibles are used. 
Post=randomForest(load_yield_adj~.,data=post_train, mtry=8, importance=TRUE)
Post
#check importance of variables used. 
importance(Post)
varImpPlot(Post)
ggsave(filename = here('post_rf_importance.png'))

library(randomForest)
set.seed(1)
#mtry means include a certian number of variables into the trees. 9 is all variables (other load_yield_adj)
#when mtry isn't included, only 3 varaibles are used. 
pre=randomForest(load_yield_adj~.,data=pre_train, mtry=8, importance=TRUE)
pre
#check importance of variables used. 
importance(pre)
varImpPlot(pre)
ggsave(filename = here('Pre_rf_importance.png'))

#lets check with all relevant variables
df3<-read.csv("w3tenyear.csv")
df3 = subset(df3, select = -c(1,12,15) )
df3=subset(df3, select = -c(3,4))
sample <- sample(c(TRUE, FALSE), nrow(df3), replace=TRUE, prob=c(0.8,0.2))
train  <- df3[sample, ]
test   <- df3[!sample, ]
dfrf=randomForest(load_yield_adj~.,data=train, mtry=20, importance=TRUE)
dfrf
#check importance of variables used. 
importance(dfrf)
#Varaibles 3 or greater %incMSE: month, wy, pH_mean, gpp_mean, precip_mean, cc_temp_sd_year,vb_fpar_sd_year
varImpPlot(dfrf)

#decision: keep all variables and let random forest decide what to use
dfrf=randomForest(load_yield_adj~.,data=train, importance=TRUE)
dfrf
importance(dfrf)
varImpPlot(dfrf)

preddf = predict(dfrf,newdata=train)
plot(preddf, test)
abline(0,1)

#split code 80% train 20%test
sample <- sample(c(TRUE, FALSE), nrow(df3), replace=TRUE, prob=c(0.8,0.2))
train  <- df3[sample, ]
test   <- df3[!sample, ]

library(MASS)
library(tree)
###bagging###
prebag=randomForest(load_yield_adj~.,data=pre_train,importance=TRUE)
prebag
preyhat.bag = predict(prebag,newdata=pre_test)
plot(preyhat.bag, pre_test$load_yield_adj)
abline(0,1)
mean((pre_test$load_yield_adj-preyhat.bag)^2)#mse here is extremely high: 72933232

postbag=randomForest(load_yield_adj~.,data=post_train,importance=TRUE)
postbag
postyhat.bag = predict(postbag,newdata=post_test)
plot(postyhat.bag, post_test$load_yield_adj)
abline(0,1)
mean((post_test$load_yield_adj-postyhat.bag)^2)#mse here is extremely high: 7196922

#boosting: need to convert site to factor
pre_train$site <- as.factor(pre_train$site)
pre_test$site <- as.factor(pre_test$site)
library(gbm)
preboost=gbm(load_yield_adj~.,data=pre_train,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(preboost)
preyhat.boost=predict(preboost,newdata=pre_test,n.trees=5000)
mean((preyhat.boost-pre_test$load_yield_adj)^2) #high mse: 57388950

bag=randomForest(load_yield_adj~.,data=train,importance=TRUE)
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
mean((yhat.boost-test$load_yield_adj)^2)


#class code# 
library(ISLR) ## data package
library(tidyverse) ## data manipulation
library(tidymodels) ## tidy modeling
library(knitr) ## tables
library(rpart.plot) ## tree diagrams
library(vip) ## plotting variable importance

bagging_spec <- rand_forest(mtry = .cols(),trees = 1000) |>
  set_engine("randomForest", importance = TRUE) |>
  set_mode("regression")

bagging_fit <- bagging_spec |>
  fit(load_yield_adj ~ ., data = train)
library(vip)
vip(bagging_fit)

predbag<-bagging_fit |>
  augment(new_data = test) 
mean((predbag$load_yield_adj-predbag$.pred)^2) #incredibly high mse

boost_spec <- boost_tree(trees = 1000, learn_rate = 0.01) |>
  set_engine("xgboost") |>
  set_mode("regression")

boost_fit <- boost_spec |>
  fit(load_yield_adj ~ ., data = train)
pred<-boost_fit |> 
  augment(new_data=test)
mean((pred$load_yield_adj-pred$.pred)^2) #incredibly high mse

#lets repeat with only top 5 variables: still incredibly high mse for all models tried
df4<-subset(df3, select = c(3,2,4,10,6,7))
sample <- sample(c(TRUE, FALSE), nrow(df4), replace=TRUE, prob=c(0.8,0.2))
train  <- df4[sample, ]
test   <- df4[!sample, ]

bagging_spec <- rand_forest(mtry = .cols(),trees = 1000) |>
  set_engine("randomForest", importance = TRUE) |>
  set_mode("regression")

bagging_fit <- bagging_spec |>
  fit(load_yield_adj ~ ., data = train)
library(vip)
vip(bagging_fit)

predbag<-bagging_fit |>
  augment(new_data = test) 
mean((predbag$load_yield_adj-predbag$.pred)^2) #incredibly high mse

boost_spec <- boost_tree(trees = 1000, learn_rate = 0.01) |>
  set_engine("xgboost") |>
  set_mode("regression")

boost_fit <- boost_spec |>
  fit(load_yield_adj ~ ., data = train)
pred<-boost_fit |> 
  augment(new_data=test)
mean((pred$load_yield_adj-pred$.pred)^2) #incredibly high mse


#################################
#gbm finally shows smaller rmse

test_x = test[, -2] # feature and target array
test_y = test[, 2] 
model_gbm = gbm(load_yield_adj ~.,
                data = train)

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





#lasso/ridge regression 





#other? needs to be continuous variable compliant







##################################################################
#look at seasonal sections for as far back as there is decent data.
#################################################################

w<-read.csv(file = 'w3.csv')

first<- w %>% filter(month < 4)
#get rid of columns with greater than 50% na
first<-first[, which(colMeans(!is.na(first)) > 0.5)]
first<-na.omit(first) 
#looks like it goes back as far as 1986

#get rid of columns with greater than 50% 0
first<-first[,colSums(first==0,na.rm = T)/nrow(first) < 0.5]
   
firstw3<-aggregate(. ~ month + wy,first , mean)
#write.csv(firstw3,"firstw3.csv")

second<- w%>% filter(month <7)
second<- second %>% filter(month >3)
#get rid of columns with greater than 50% na
second<-second[, which(colMeans(!is.na(second)) > 0.5)]
second<-na.omit(second) 

secondw3<-aggregate(. ~ month + wy,second , mean)

#get rid of columns with greater than 50% 0
second<-second[,colSums(second==0,na.rm = T)/nrow(second) < 0.25]
#write.csv(secondw3,"secondw3.csv")



third<- w%>% filter(month <10)
third<- third %>% filter(month >6)
#get rid of columns with greater than 50% na
third<-third[, which(colMeans(!is.na(third)) > 0.5)]
third<-na.omit(third) 

thirdw3<-aggregate(. ~ month + wy,third, mean)

#get rid of columns with greater than 50% 0
thirdw3<-thirdw3[,colSums(thirdw3==0,na.rm = T)/nrow(thirdw3) < 0.25]
#write.csv(thirdw3,"thirdw3.csv")


fourth<- w %>% filter(month >9)
#get rid of columns with greater than 50% na
fourth<-fourth[, which(colMeans(!is.na(fourth)) > 0.5)]
fourth<-na.omit(fourth) 

fourthw3<-aggregate(. ~ month + wy,fourth, mean)

#get rid of columns with greater than 50% 0
fourthw3<-fourthw3[,colSums(fourthw3==0,na.rm = T)/nrow(fourthw3) < 0.25]
#write.csv(fourthw3,"foruthw3.csv")

















