trails.df <- read.csv("~/trails_file_location/Trails.xlsx.csv")

trails.df$trail_id <- as.character(trails.df$trail_id) #recode trail ID so it doesn't get analyzed
trails.df$visitor_usage <- as.integer(trails.df$visitor_usage)
trails.df$visitor_usage[is.na(trails.df$visitor_usage)] <- mean(trails.df$visitor_usage,na.rm=TRUE)
trails.df$visitor_usage <- signif(trails.df$visitor_usage, digits = 1)
trails.df <- trails.df[,-1] #just getting rid of trail_id to make this simpler
##recode route_type to ordered levels labeled 1, 2, 3 for out and back, loop, and point to point
trails.df$route_type <- factor(trails.df$route_type, levels=c("out and back", "loop", "point to point"), 
                               labels=c(1, 2, 3))
##visitor_usage was a mess, needed to be recoded numeric, mean imputed missing values, then had to convert the mean imputed values to whole numbers.  I probably should have done nothing
##all ordinal numeric variables have been recoded to factors in order to perform regression analysis on them
##recoding popularity to binary variable
trails.df$popularity <- ifelse(trails.df$popularity<6.53,0,1)
trails.df$Popular <- trails.df$popularity=="1"
trails.df$Unpopular <- trails.df$popularity=="0"
##recode vars to integers because the function wasn't applying properly to numeric variables????
trails.df$popularity <- as.integer(trails.df$popularity)
trails.df$length <- as.integer(trails.df$length)
trails.df$popularity <- as.integer(trails.df$popularity)
trails.df$elevation_gain <- as.integer(trails.df$elevation_gain)
trails.df$difficulty_rating <- as.integer(trails.df$difficulty_rating)
trails.df$visitor_usage <- as.integer(trails.df$visitor_usage)
trails.df$num_reviews <- as.integer(trails.df$num_reviews)
trails.df$avg_rating <- as.integer(trails.df$avg_rating)
trails.df$route_oab <- as.numeric(trails.df$route_type=="1")
trails.df$route_loop <- as.numeric(trails.df$route_type=="2")
trails.df$route_ptp <- as.numeric(trails.df$route_type=="3")

trails.df <- trails.df[ ,c(1:8, 11:13)]

View(trails.df)
summary(trails.df)
str(trails.df)

###required packages
library(e1071)       #for calculating variable importance
library(caret)       #for general model fitting
library(rpart)       #for fitting decision trees
library(ipred)       #for fitting bagged decision trees
library(adabag)
library(rpart) 
library(caret)
library(rpart.plot)
library(randomForest)
library(gbm) 


set.seed(420) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 60% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(trails.df), size = floor(.6*nrow(trails.df)), replace = F)
train.df <- trails.df[sample, ]
valid.df  <- trails.df[-sample, ]

View(train.df)





#######tree model
#trails.tr <- rpart(popularity ~ length + elevation_gain + difficulty_rating + visitor_usage +
#                   avg_rating + num_reviews + route_oab + route_loop + route_ptp, data = train.df, 
#                   method = "class", cp=0, minsplit = 1)
trails.tr <- rpart(popularity ~ . , data = train.df, method = "class", cp = 0.001, minsplit = 7)
prp(trails.tr, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(trails.tr$frame$var == "<leaf>", 'red', 'green'))

trails.tr.point.pred.train <- predict(trails.tr, train.df,type = "class")
c.mat1 <- table(trails.tr.point.pred.train, train.df$popularity)
c.mat1

sum(diag(c.mat1))/sum(c.mat1) #accuracy
c.mat1[2,2]/sum(c.mat1[,2])   #sensitivity
c.mat1[1,1]/sum(c.mat1[,1])   #specificity

trails.tr.point.pred.valid <- predict(trails.tr, valid.df,type = "class")
c.mat2 <- table(trails.tr.point.pred.valid, valid.df$popularity)
c.mat2

sum(diag(c.mat2))/sum(c.mat2) #accuracy
c.mat2[2,2]/sum(c.mat2[,2])   #sensitivity
c.mat2[1,1]/sum(c.mat2[,1])   #specificity


trails.pr <- prune(trails.tr, 
                   cp = trails.tr$cptable[which.min(trails.tr$cptable[,"xerror"]),"CP"])
length(trails.pr$frame$var[trails.pr$frame$var == "<leaf>"])
prp(trails.tr, type = 1, extra = 1, split.font = 2, varlen = -10) 



trails.tr2 <- rpart(popularity ~ avg_rating + num_reviews , data = train.df, method = "class", cp = 0.001, minsplit = 3)
prp(trails.tr2, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(trails.tr2$frame$var == "<leaf>", 'red', 'green'))

trails.tr.point.pred.train2 <- predict(trails.tr2, train.df,type = "class")
c.mat12 <- table(trails.tr.point.pred.train2, train.df$popularity)
c.mat12

sum(diag(c.mat12))/sum(c.mat12) #accuracy
c.mat12[2,2]/sum(c.mat12[,2])   #sensitivity
c.mat12[1,1]/sum(c.mat12[,1])   #specificity

trails.tr.point.pred.valid2 <- predict(trails.tr2, valid.df,type = "class")
c.mat22 <- table(trails.tr.point.pred.valid2, valid.df$popularity)
c.mat22

sum(diag(c.mat22))/sum(c.mat22) #accuracy
c.mat22[2,2]/sum(c.mat22[,2])   #sensitivity
c.mat22[1,1]/sum(c.mat22[,1])   #specificity


trails.tr3 <- rpart(popularity ~ length + elevation_gain + difficulty_rating + route_oab + route_loop + 
                      route_ptp , data = train.df, method = "class", cp = 0.001, minsplit = 5)
#prp(trails.tr3, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
#    box.col=ifelse(trails.tr3$frame$var == "<leaf>", 'red', 'green'))

trails.tr.point.pred.train3 <- predict(trails.tr3, train.df,type = "class")
c.mat13 <- table(trails.tr.point.pred.train3, train.df$popularity)
c.mat13

sum(diag(c.mat13))/sum(c.mat13) #accuracy
c.mat13[2,2]/sum(c.mat13[,2])   #sensitivity
c.mat13[1,1]/sum(c.mat13[,1])   #specificity

trails.tr.point.pred.valid3 <- predict(trails.tr3, valid.df,type = "class")
c.mat23 <- table(trails.tr.point.pred.valid3, valid.df$popularity)
c.mat23

sum(diag(c.mat23))/sum(c.mat23) #accuracy
c.mat23[2,2]/sum(c.mat23[,2])   #sensitivity
c.mat23[1,1]/sum(c.mat23[,1])   #specificity


####Random forest model
trails.rf <- randomForest(as.factor(popularity) ~ ., data = train.df, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)

## variable importance plot
varImpPlot(trails.rf, type = 1)
varImpPlot(trails.rf, type = 2)

## confusion matrix
trails.rf.pred <- predict(trails.rf, train.df)
c.mat4 <- table(trails.rf.pred, train.df$popularity)
c.mat4

trails.rf.pred2 <- predict(trails.rf, train.df)
table(trails.rf.pred2, train.df$popularity)
confusionMatrix(factor(trails.rf.pred2), factor(train.df$popularity))

trails.rf.pred <- predict(trails.rf, valid.df)
table(trails.rf.pred, valid.df$popularity)
confusionMatrix(factor(trails.rf.pred), factor(valid.df$popularity))


####b000st
trails.bs <- gbm(popularity ~ ., data = train.df, distribution = "multinomial")
trails.bs.pred <- predict(trails.bs, valid.df, type="response")
head(trails.bs.pred)
predicted.class= apply(trails.bs.pred,1,which.max)-1
boostt1 <- table(predicted.class, valid.df$popularity)
trails.bs.pred2 <- predict(trails.bs, train.df, type="response")
predicted.class= apply(trails.bs.pred2,1,which.max)-1
boostt2 <- table(predicted.class, train.df$popularity)
boostt1
sum(diag(boostt1))/sum(boostt1) #accuracy
boostt1[2,2]/sum(boostt1[,2])   #sensitivity
boostt1[1,1]/sum(boostt1[,1])   #specificity

boostt2
sum(diag(boostt2))/sum(boostt2) #accuracy
boostt2[2,2]/sum(boostt2[,2])   #sensitivity
boostt2[1,1]/sum(boostt2[,1])   #specificity


str(train.df)
train.df <- train.df[,c(1:4,6:11)]
train.df$popularity <- as.factor(train.df$popularity)
train.df$difficulty_rating <- as.factor(train.df$difficulty_rating)
train.df$visitor_usage <- as.factor(train.df$visitor_usage)
train.df$route_loop <- as.factor(train.df$route_loop)
train.df$route_oab <- as.factor(train.df$route_oab)
train.df$route_ptp <- as.factor(train.df$route_ptp)

valid.df <- valid.df[,c(1:4,6:11)]
valid.df$popularity <- as.factor(valid.df$popularity)
valid.df$difficulty_rating <- as.factor(valid.df$difficulty_rating)
valid.df$visitor_usage <- as.factor(valid.df$visitor_usage)
valid.df$route_loop <- as.factor(valid.df$route_loop)
valid.df$route_oab <- as.factor(valid.df$route_oab)
valid.df$route_ptp <- as.factor(valid.df$route_ptp)

####baag
trails.bag <- bagging(popularity ~ ., data=train.df)

pred1 <- predict(trails.bag, train.df, type = "class")
bagg1 <- pred1$confusion
bagg1
sum(diag(bagg1))/sum(bagg1) #accuracy
bagg1[2,2]/sum(bagg1[,2])   #sensitivity
bagg1[1,1]/sum(bagg1[,1])   #specificity



pred2 <- predict(trails.bag, valid.df, type = "class")   # validation data
bagg2 <- pred2$confusion
bagg2

bagg2
sum(diag(bagg2))/sum(bagg2) #accuracy
bagg2[2,2]/sum(bagg2[,2])   #sensitivity
bagg2[1,1]/sum(bagg2[,1])   #specificity