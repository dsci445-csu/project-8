
w<-read.csv(file = 'w3.csv')
#filter by month
w1<-w%>% filter(
  month %in% c("1" )
)
#filter by year 
w1<-w1%>% filter(
  wy %in% c("2021" )
)
#get rid of columns with greater than 50% na in them

w1<-w1[, which(colMeans(!is.na(w1)) > 0.5)]
#get rid of last two columns 
w1 = subset(w1, select = -c(28,29) )












####this code was for agggregating all months data and looking at it monthly ###############
#some years don't have 12 months worth of data. should be fine? 
#would it be better to look at month 1 over 20 years and use that data or better to combine last 10 years and look at them
w<-read.csv(file = 'w3.csv')
#filter by year: i tried to write a loop to do this for however many years we want but 
#that was taking too long. Input the year you want, save as dfyear, and compile all df's to one dataframe

w1<-w%>% filter(
  wy %in% c("2016" )
)
#get rid of columns with greater than 50% na in them: 
#this isn't super important, since we need to remove all na for random forest, but 
#i was curious to see how much data is missing and if there are just certain months/years with missing data
w1<-w1[, which(colMeans(!is.na(w1)) > 0.5)]
#get rid of last two columns: data not useful
w1 = subset(w1, select = -c(28,29) )
#average values within each month for each column 
df2016<-aggregate(. ~ month, w1, mean)
df2021<-aggregate(. ~ month, w1, mean)
df2020<-aggregate(. ~ month, w1, mean)
#etc. 
#need bind rows because of some missing data. 
df2<-bind_rows(df2020, df2019,df2018,df2017,df2016)

#get rid of columns with greater than 50% na in them: again just to see how the dataset changes, not important
df2<-df2[, which(colMeans(!is.na(df2)) > 0.5)]





#random forest code

#in paper, random forest was used to predict cluster assignments. 

#we can still use RF for this tho

#cant have any na in rf. 
#remove all na 
df3<-df2 %>% select_if(~ !any(is.na(.)))
set.seed(18)

par(mfrow=c(1,2))
library(caret)
# we are not going to do any cross-validatin
# and rely on OOB error
trctrl <- trainControl(method = "none")

# we will now train random forest model
rfregFit <- train(load~., 
                  data = df3, 
                  method = "ranger",
                  trControl=trctrl,
                  # calculate importance
                  importance="permutation", 
                  tuneGrid = data.frame(mtry=2,
                                        min.node.size = 5,
                                        splitrule="variance")
)




lot(df3$load,rfregFit$finalModel$predictions,
    pch=19,xlab="load",
    ylab="OOB predicted load")
mtext(paste("R-squared",
            format(rfregFit$finalModel$r.squared,digits=2)))

# plot residuals
plot(ameth$Age,(rfregFit$finalModel$predictions-ameth$Age),
     pch=18,ylab="residuals (predicted-observed)",
     xlab="observed Age",col="blue3")
abline(h=0,col="red4",lty=2)


























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
library(ggplot2)
#ggpairs(w3df)#still too crammed to get any good info
library(GGally)
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


post_caa <- data %>%
  filter(wy < 2013,
         wy > 2010) %>%
  select(-wy)

post_caa %>%
  select(-site) %>%
  pairs()





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




