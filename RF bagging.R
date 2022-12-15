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






#pre bagging 
#bagging
pre_sample <- sample(c(TRUE, FALSE), nrow(pre_caa), replace=TRUE, prob=c(0.8,0.2))
pre_train  <- pre_caa[pre_sample, ]
pre_test   <- pre_caa[!pre_sample, ]
library(randomForest)
bag=randomForest(load_yield_adj~.,data=pre_train,mtry=7,importance=TRUE)
bag
yhat= predict(bag,newdata=pre_test)
plot(yhat, pre_test$load_yield_adj)
abline(0,1)
mean((pre_test$load_yield_adj-yhat)^2)#mse here is extremely high

#post bagging 
#bagging
post_sample <- sample(c(TRUE, FALSE), nrow(post_caa), replace=TRUE, prob=c(0.8,0.2))
post_train  <- post_caa[post_sample, ]
post_test   <- post_caa[!post_sample, ]
library(randomForest)
bag=randomForest(load_yield_adj~.,data=post_train,mtry=7,importance=TRUE)
bag
yhat= predict(bag,newdata=post_test)
plot(yhat, post_test$load_yield_adj)
abline(0,1)
mean((post_test$load_yield_adj-yhat)^2)#mse here is extremely high

