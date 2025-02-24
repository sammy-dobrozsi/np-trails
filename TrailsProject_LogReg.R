trails.df <- read.csv("~/trails_file_location/Trails.xlsx.csv")

trails.df$trail_id <- as.character(trails.df$trail_id) #recode trail ID so it doesn't get analyzed
trails.df$visitor_usage <- as.numeric(trails.df$visitor_usage)
trails.df$visitor_usage[is.na(trails.df$visitor_usage)] <- mean(trails.df$visitor_usage,na.rm=TRUE)
trails.df$visitor_usage <- signif(trails.df$visitor_usage, digits = 1)
trails.df$visitor_usage <- as.factor(trails.df$visitor_usage) #making it a factor for regression)
trails.df$difficulty_rating <- as.factor(trails.df$difficulty_rating) #factor for regression)
trails.df$avg_rating <- as.factor(trails.df$avg_rating)
trails.df$route_type <- as.factor(trails.df$route_type)
trails.df <- trails.df[,-1] #just getting rid of trail_id to make this simpler
##visitor_usage was a mess, needed to be recoded numeric, mean imputed missing values, then had to convert the mean imputed values to whole numbers.  I probably should have done nothing
##all ordinal numeric variables have been recoded to factors in order to perform regression analysis on them
##recoding popularity to binary variable
trails.df$popularity <- ifelse(trails.df$popularity<6.53,0,1)

View(trails.df)
summary(trails.df)
str(trails.df)

set.seed(420) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 60% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(trails.df), size = floor(.6*nrow(trails.df)), replace = F)
train.df <- trails.df[sample, ]
valid.df  <- trails.df[-sample, ]

##full logistic model
logit.reg <- glm(popularity ~ ., data = train.df, family = "binomial")
summary(logit.reg)

library(e1071)   # misc functions in stat, prob
library(Rcpp)    # R and C++ integration
library(caret)    # Classfication And REgression Training

varImp(logit.reg)
plot(varImp(logit.reg))
names(logit.reg)

##set up validation set
pred <- predict(logit.reg, valid.df, type = "response")

library(pastecs)
stat.desc(pred)


c.mat <- table(ifelse(pred > 0.5, 1, 0), valid.df[,1])    #2 way table: row by column
#Confusion Matrix
c.mat
#Accuracy
sum(diag(c.mat))/sum(c.mat)

table(valid.df[,8])

library(InformationValue)
plotROC(valid.df[,1], pred)
#Confusion Matrix
confusionMatrix(valid.df[,1], pred, threshold = 0.5)
#Misclassification Error
misClassError(valid.df[,1], pred, threshold = 0.5)
#Overall Model Specificity
1 - misClassError(valid.df[,1], pred, threshold = 0.5)
optimalCutoff(valid.df[,1], pred, optimiseFor = "misclasserror",
              returnDiagnostics = TRUE)

##change threshold to optimal .42)
#Confusion Matrix
confusionMatrix(valid.df[,1], pred, threshold = 0.42)
#Misclassification Error
misClassError(valid.df[,1], pred, threshold = 0.42)
#Overall Model Specificity
1 - misClassError(valid.df[,1], pred, threshold = 0.42)
##first 5 predicted records
logit.reg.pred <- predict(logit.reg, valid.df[,-1], type = "response")
data.frame(actual = valid.df$popularity[1:5], predicted = logit.reg.pred[1:5])

##lift chart
library(gains)

gain <- gains(valid.df$popularity, logit.reg.pred, groups=10)

data.frame(c(0,gain$cume.pct.of.total*sum(valid.df$popularity)),
           c(0,gain$cume.obs) )

data.frame( c(0,sum(valid.df$popularity)) , c(0, dim(valid.df)[1]) )

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$popularity))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart", type="l")
lines(c(0,sum(valid.df$popularity))~c(0, dim(valid.df)[1]), lty=2)

##decile-wise lift chart

heights <- gain$mean.resp/mean(valid.df$popularity)

midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)


####logistic model w/o elevation_gain
logit.reg2 <- glm(popularity ~ length + difficulty_rating + route_type + visitor_usage + avg_rating + num_reviews,
                  data = train.df, family = "binomial")
summary(logit.reg2)

pred2 <- predict(logit.reg2, valid.df, type = "response")


plotROC(valid.df[,1], pred2)
#Confusion Matrix
confusionMatrix(valid.df[,1], pred2, threshold = 0.5)
#Misclassification Error
misClassError(valid.df[,1], pred2, threshold = 0.5)
#Overall Model Specificity
1 - misClassError(valid.df[,1], pred2, threshold = 0.5)
optimalCutoff(valid.df[,1], pred2, optimiseFor = "misclasserror",
              returnDiagnostics = TRUE)

##Using optimal cutoff of .4
#Confusion Matrix
confusionMatrix(valid.df[,1], pred2, threshold = 0.4)
#Misclassification Error
misClassError(valid.df[,1], pred2, threshold = 0.4)
#Overall Model Specificity
1 - misClassError(valid.df[,1], pred2, threshold = 0.4)

##lift chart
gain2 <- gains(valid.df$popularity, logit.reg.pred2, groups=10)

data.frame(c(0,gain2$cume.pct.of.total*sum(valid.df$popularity)),
           c(0,gain2$cume.obs) )

data.frame( c(0,sum(valid.df$popularity)) , c(0, dim(valid.df)[1]) )

# plot lift chart
plot(c(0,gain2$cume.pct.of.total*sum(valid.df$popularity))~c(0,gain2$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart", type="l")
lines(c(0,sum(valid.df$popularity))~c(0, dim(valid.df)[1]), lty=2)

##decile-wise lift chart

heights2 <- gain2$mean.resp/mean(valid.df$popularity)

midpoints2 <- barplot(heights2, names.arg = gain2$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
text(midpoints, heights2+0.5, labels=round(heights2, 1), cex = 0.8)


###stepwise variable selection
library(MASS)

step1<-stepAIC(logit.reg, direction = "backward") #, trace=FALSE)    #stepwise, default is backward
step1$anova

step2<-stepAIC(logit.reg, direction="forward")  #forward
step2$anova

###model based on stepwise variable selection
logit.reg3 <- glm(popularity ~ length + elevation_gain + avg_rating + num_reviews,
                  data = train.df, family = "binomial")
summary(logit.reg3)

pred3 <- predict(logit.reg3, valid.df, type = "response")


plotROC(valid.df[,1], pred3)
#Confusion Matrix
confusionMatrix(valid.df[,1], pred3, threshold = 0.5)
#Misclassification Error
misClassError(valid.df[,1], pred3, threshold = 0.5)
#Overall Model Specificity
1 - misClassError(valid.df[,1], pred3, threshold = 0.5)
optimalCutoff(valid.df[,1], pred3, optimiseFor = "misclasserror",
              returnDiagnostics = TRUE)

##Using optimal cutoff of .41
#Confusion Matrix
confusionMatrix(valid.df[,1], pred3, threshold = 0.41)
#Misclassification Error
misClassError(valid.df[,1], pred3, threshold = 0.41)
#Overall Model Specificity
1 - misClassError(valid.df[,1], pred3, threshold = 0.41)

##lift chart
gain3 <- gains(valid.df$popularity, logit.reg.pred2, groups=10)

data.frame(c(0,gain3$cume.pct.of.total*sum(valid.df$popularity)),
           c(0,gain3$cume.obs) )

data.frame( c(0,sum(valid.df$popularity)) , c(0, dim(valid.df)[1]) )

# plot lift chart
plot(c(0,gain3$cume.pct.of.total*sum(valid.df$popularity))~c(0,gain3$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Lift Chart", type="l")
lines(c(0,sum(valid.df$popularity))~c(0, dim(valid.df)[1]), lty=2)

##decile-wise lift chart

heights3 <- gain3$mean.resp/mean(valid.df$popularity)

midpoints3 <- barplot(heights3, names.arg = gain3$depth, ylim = c(0,9), 
                      xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
text(midpoints, heights3+0.5, labels=round(heights3, 1), cex = 0.8)


###for real what's up with the elevation
sum(trails.df$elevation_gain[trails.df$popularity==0]) #1227548
sum(trails.df$elevation_gain[trails.df$popularity==1]) #898755.2
sum(trails.df$elevation_gain[trails.df$popularity==1])-sum(trails.df$elevation_gain[trails.df$popularity==0])
#-328792.6
mean(trails.df$elevation_gain[trails.df$popularity==0]) #744.8713
mean(trails.df$elevation_gain[trails.df$popularity==1]) #539.7929
mean(trails.df$elevation_gain[trails.df$popularity==1])-mean(trails.df$elevation_gain[trails.df$popularity==0])
#-205.0789

mean(trails.df$length[trails.df$popularity==0]) 
mean(trails.df$length[trails.df$popularity==1]) 
mean(trails.df$length[trails.df$popularity==1])-mean(trails.df$length[trails.df$popularity==0])

mean(as.numeric(trails.df$difficulty_rating[trails.df$popularity==0])) 
mean(as.numeric(trails.df$difficulty_rating[trails.df$popularity==1]))
mean(as.numeric(trails.df$difficulty_rating[trails.df$popularity==1]))-mean(as.numeric(trails.df$difficulty_rating[trails.df$popularity==0]))

mean(trails.df$num_reviews[trails.df$popularity==0]) 
mean(trails.df$num_reviews[trails.df$popularity==1]) 
mean(trails.df$num_reviews[trails.df$popularity==1])-mean(trails.df$num_reviews[trails.df$popularity==0])