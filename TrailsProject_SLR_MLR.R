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
View(trails.df)
summary(trails.df)
str(trails.df)

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 60% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(trails.df), size = floor(.6*nrow(trails.df)), replace = F)
train <- trails.df[sample, ]
test  <- trails.df[-sample, ]

lmtrain <- lm(popularity ~ ., data = train)
options(scipen=99)
summary(lmtrain)
options(scipen=0)
plot(lmtrain)

##residual, qqnorm, and residual density plots
resmod <- resid(lmtrain)
plot(fitted(lmtrain), resmod, main = "Residual vs. Fitted of Full Linear Regression Model")
abline(0,0)
plot(density(resmod), main = "Density curve of residuals in full linear regression model")

##a plot that looks neat so I wanna keep it in here even though I didn't use it
library(ggplot2)
ggplotregression <- function(fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
    }
ggplotregression(lmtrain)
library(forecast)

library(settings)
require(settings)

lmpred <- predict(lmtrain, test)
lmpred
predict(lmtrain, test, se.fit = T, interval = "prediction", level = 0.95)
lmtrain.ic <- predict(lmtrain, train)
options(scipen=999, digits=3)
accuracy(lmtrain.ic, train$popularity)
accuracy(lmpred, test$popularity)
settings::reset(options)

all.residuals <- test$popularity - lmpred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 15, xlab = "Residuals", main = "")


##exhaustive variable selection

library(leaps)
head(train)
selection <- regsubsets(popularity ~ ., data = train, nbest = 1, nvmax = dim(train)[2],
                        method = "exhaustive")
plot(selection)
plot(selection, scale = "adjr2", main = "Exhaustive Search")
plot(selection, scale = "Cp")

sum <- summary(selection)
sum$rsq
sum$adjr2
sum$cp
sum$bic

##new linear model with min BIC variable selection
lmtrain2 <- lm(popularity ~ length + visitor_usage + avg_rating + num_reviews, data = train)
options(scipen=99)
summary(lmtrain2)
options(scipen=0)
plot(lmtrain2)

lmpred2 <- predict(lmtrain2, test)
lmtrain2.ic <- predict(lmtrain2, train)
options(scipen=999, digits=3)
accuracy(lmtrain2.ic, train$popularity)
accuracy(lmpred2, test$popularity)
settings::reset(options)

##forward selection
selection2 <- regsubsets(popularity ~ ., data = train, nbest = 1, nvmax = dim(train)[2],
                        method = "forward")
plot(selection2)
plot(selection2, scale = "adjr2", main = "Forward Selection")
plot(selection2, scale = "Cp")

sum2 <- summary(selection2)
sum2$rsq
sum2$adjr2
sum2$cp
sum2$bic

##backward selection
selection3 <- regsubsets(popularity ~ ., data = train, nbest = 1, nvmax = dim(train)[2],
                        method = "backward")
plot(selection3)
plot(selection3, scale = "adjr2",  main = "Backward Selection")
plot(selection3, scale = "Cp", main = "Backward Selection")

sum3 <- summary(selection3)
sum3$rsq
sum3$adjr2
sum3$cp
sum3$bic

##Stepwise, because that was all the same lmao
trails.df.step <- step(lmtrain, direction = "both")
summary(trails.df.step)
trails.df.step.train <- predict(trails.df.step, train)
trails.df.step.pred <- predict(trails.df.step, test)
accuracy(trails.df.step.train, train$popularity)
accuracy(trails.df.step.pred, test$popularity)