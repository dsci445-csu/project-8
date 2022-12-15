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





################################################
#gradient boosting with lower rmse. 
##################################################
library(gbm)

#################################
post_caa<-read.csv("post_caa.csv")
post_caa$site<-as.factor(post_caa$site)
pre_caa<-read.csv("pre_caa.csv")
pre_caa$site<-as.factor(pre_caa$site)
#gbm finally shows smaller rmse

#split code 80% train 20%test
post_sample <- sample(c(TRUE, FALSE), nrow(post_caa), replace=TRUE, prob=c(0.8,0.2))
post_train  <- post_caa[post_sample, ]
post_test   <- post_caa[!post_sample, ]

pre_sample <- sample(c(TRUE, FALSE), nrow(pre_caa), replace=TRUE, prob=c(0.8,0.2))
pre_train  <- pre_caa[pre_sample, ]
pre_test   <- pre_caa[!pre_sample, ]


#pre
pretest_x = pre_test[, -2] # feature and target array
pretest_y = pre_test[, 2] 
model_gbm = gbm(load_yield_adj ~.,
                data = pre_train,
                cv.folds = 10)

print(model_gbm)

summary(model_gbm)
pre_importance<-summary(model_gbm)
pre_importance<-pre_importance[-4,]
ggplot(pre_importance)
ggplot(pre_importance, aes(x = var, y = rel.inf)) +
  geom_col() +
  ggtitle("Pre-CAA")


prepred_y = predict.gbm(model_gbm, pretest_x)
prepred_y
preresiduals = pretest_y - prepred_y
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')#rmse:6476.693 

prey_test_mean = mean(pretest_y)
# Calculate total sum of squares
pretss =  sum((pretest_y - prey_test_mean)^2 )
# Calculate residual sum of squares
prerss =  sum(preresiduals^2)
# Calculate R-squared
rsq  =  1 - (prerss/pretss)
cat('The R-square of the test data is ', round(rsq,3), '\n')#r squre:  0.975 

# visualize the model, actual and predicted data
x_ax = 1:length(prepred_y)
plot(x_ax, pretest_y, col="blue", pch=20, cex=.9)
lines(x_ax, prepred_y, col="red", pch=20, cex=.9) 


#post 
posttest_x = post_test[, -2] # feature and target array
posttest_y = post_test[, 2] 
model_gbm = gbm(load_yield_adj ~.,
                data = post_train,
                cv.folds = 10)

print(model_gbm)

summary(model_gbm)

summary(model_gbm)
pre_importance<-summary(model_gbm)
pre_importance<-pre_importance[-4,]
ggplot(pre_importance)
ggplot(pre_importance, aes(x = var, y = rel.inf)) +
  geom_col() +
  ggtitle("Post-CAA")

pred_y = predict.gbm(model_gbm, posttest_x)
pred_y
residuals = posttest_y - pred_y
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')#rmse:  6667.378 

y_test_mean = mean(posttest_y)
# Calculate total sum of squares
tss =  sum((posttest_y - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')#r square: 0.929 

# visualize the model, actual and predicted data
x_ax = 1:length(pred_y)
plot(x_ax, posttest_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_y, col="red", pch=20, cex=.9) 



