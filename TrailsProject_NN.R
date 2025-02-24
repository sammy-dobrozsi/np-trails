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

##scale large numeric variables for better processing of model
trails.df[,c(2, 3, 4, 6, 7, 8)] <- scale(trails.df[,c(2, 3, 4, 6, 7, 8)])

View(trails.df)
summary(trails.df)
str(trails.df)

##get required packages
#install.packages("neuralnet")
#install.packages("nnet")
#install.packages("caret")

library(neuralnet)
library(nnet)
library(caret)

set.seed(69) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 60% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(trails.df), size = floor(.6*nrow(trails.df)), replace = F)
train.df <- trails.df[sample, ]
valid.df  <- trails.df[-sample, ]

train=sample(row.names(trails.df), dim(trails.df)[1]*0.6)
valid=setdiff(row.names(trails.df), train)



View(train.df)


##Full model
#nn1 <- neuralnet(Popular + Unpopular ~ length + elevation_gain + difficulty_rating + 
#                  route_oab + route_loop + route_ptp + visitor_usage + avg_rating + num_reviews, 
#                data = train.df, hidden = 3, stepmax = 1e+06, linear.output = FALSE)
nn1$weights
plot(nn1)

pred1 <- predict(nn1, train.df)

head(pred1)
summary(pred1)


###classification matrix 
c.mat1 <- table(ifelse(pred1[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat1   # row: predicted ;   column: actual 

sum(diag(c.mat1))/sum(c.mat1) #accuracy
c.mat1[2,2]/sum(c.mat1[,2])   #sensitivity
c.mat1[1,1]/sum(c.mat1[,1])   #specificity

##validate
pred11 <- predict(nn1, valid.df)

head(pred11)
summary(pred11)


###classification matrix 
c.mat11 <- table(ifelse(pred11[,1] > 0.5, 1, 0), valid.df$popularity)    #2 way table: row by column
c.mat11   # row: predicted ;   column: actual 

sum(diag(c.mat11))/sum(c.mat11) #accuracy
c.mat11[2,2]/sum(c.mat11[,2])   #sensitivity
c.mat11[1,1]/sum(c.mat11[,1])   #specificity






##more hidden layers
#nn111 <- neuralnet(Popular + Unpopular ~ length + elevation_gain + difficulty_rating + 
#                   route_oab + route_loop + route_ptp + visitor_usage + avg_rating + num_reviews, 
#                 data = train.df, hidden = 6, stepmax = 1e+05, rep = 2, linear.output = FALSE)

pred12 <- predict(nn111, train.df)

head(pred12)
summary(pred12)


###classification matrix 
c.mat12 <- table(ifelse(pred12[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat12   # row: predicted ;   column: actual 

sum(diag(c.mat12))/sum(c.mat12) #accuracy
c.mat12[2,2]/sum(c.mat12[,2])   #sensitivity
c.mat12[1,1]/sum(c.mat12[,1])   #specificity

##validate
pred121 <- predict(nn111, valid.df)

head(pred121)
summary(pred121)


###classification matrix 
c.mat121 <- table(ifelse(pred121[,1] > 0.5, 1, 0), valid.df$popularity)    #2 way table: row by column
c.mat121   # row: predicted ;   column: actual 

sum(diag(c.mat11))/sum(c.mat11) #accuracy
c.mat121[2,2]/sum(c.mat121[,2])   #sensitivity
c.mat121[1,1]/sum(c.mat121[,1])   #specificity





###MORE layers
#nn112 <- neuralnet(Popular + Unpopular ~ length + elevation_gain + difficulty_rating + 
#                     route_oab + route_loop + route_ptp + visitor_usage + avg_rating + num_reviews, 
#                   data = train.df, hidden = 9, stepmax = 1e+05, rep = 2, linear.output = FALSE)

pred112 <- predict(nn112, train.df)

head(pred112)
summary(pred112)


###classification matrix 
c.mat112 <- table(ifelse(pred112[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat112   # row: predicted ;   column: actual 

sum(diag(c.mat112))/sum(c.mat112) #accuracy
c.mat112[2,2]/sum(c.mat112[,2])   #sensitivity
c.mat112[1,1]/sum(c.mat112[,1])   #specificity

##validate
pred131 <- predict(nn112, valid.df)

head(pred131)
summary(pred131)


###classification matrix 
c.mat131 <- table(ifelse(pred131[,1] > 0.5, 1, 0), valid.df$popularity)    #2 way table: row by column
c.mat131   # row: predicted ;   column: actual 

sum(diag(c.mat131))/sum(c.mat131) #accuracy
c.mat131[2,2]/sum(c.mat131[,2])   #sensitivity
c.mat131[1,1]/sum(c.mat131[,1])   #specificity


###model won't converge with more hidden layers :(

##without elevation_gain
#nn2 <- neuralnet(Popular + Unpopular ~ length  + difficulty_rating + 
#                   route_oab + route_loop + route_ptp + visitor_usage + avg_rating + num_reviews, 
#                 data = train.df, hidden = 3, stepmax = 1e+05, rep = 2, linear.output = FALSE)

nn2$weights
plot(nn2)

pred2 <- predict(nn2, train.df)

head(pred2)
summary(pred2)


###classification matrix 
c.mat2 <- table(ifelse(pred2[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat2   # row: predicted ;   column: actual 

sum(diag(c.mat2))/sum(c.mat2) #accuracy
c.mat2[2,2]/sum(c.mat2[,2])   #sensitivity
c.mat2[1,1]/sum(c.mat2[,1])   #specificity

##validate
pred21 <- predict(nn2, valid.df)

head(pred21)
summary(pred21)


###classification matrix 
c.mat21 <- table(ifelse(pred21[,1] > 0.5, 1, 0), valid.df$popularity)    #2 way table: row by column
c.mat21   # row: predicted ;   column: actual 

sum(diag(c.mat21))/sum(c.mat21) #accuracy
c.mat21[2,2]/sum(c.mat21[,2])   #sensitivity
c.mat21[1,1]/sum(c.mat21[,1])   #specificity


##more layers
#nn22 <- neuralnet(Popular + Unpopular ~ length  + difficulty_rating + 
#                   route_oab + route_loop + route_ptp + visitor_usage + avg_rating + num_reviews, 
#                 data = train.df, hidden = 6, linear.output = FALSE)

nn22$weights
plot(nn22)

pred22 <- predict(nn22, train.df)

head(pred22)
summary(pred22)


###classification matrix 
c.mat22 <- table(ifelse(pred22[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat22   # row: predicted ;   column: actual 

sum(diag(c.mat22))/sum(c.mat22) #accuracy
c.mat22[2,2]/sum(c.mat22[,2])   #sensitivity
c.mat22[1,1]/sum(c.mat22[,1])   #specificity

##validate
pred221 <- predict(nn22, valid.df)

head(pred221)
summary(pred221)

###classification matrix 
c.mat221 <- table(ifelse(pred221[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat221   # row: predicted ;   column: actual 

sum(diag(c.mat221))/sum(c.mat221) #accuracy
c.mat221[2,2]/sum(c.mat221[,2])   #sensitivity
c.mat221[1,1]/sum(c.mat221[,1])   #specificity

###MORE layers
#nn23 <- neuralnet(Popular + Unpopular ~ length  + difficulty_rating + 
#                    route_oab + route_loop + route_ptp + visitor_usage + avg_rating + num_reviews, 
#                  data = train.df, hidden = 9, linear.output = FALSE)

nn23$weights
plot(nn23)

pred23 <- predict(nn23, train.df)

head(pred23)
summary(pred23)


###classification matrix 
c.mat23 <- table(ifelse(pred23[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat23   # row: predicted ;   column: actual 

sum(diag(c.mat23))/sum(c.mat23) #accuracy
c.mat23[2,2]/sum(c.mat23[,2])   #sensitivity
c.mat23[1,1]/sum(c.mat23[,1])   #specificity

##validate
pred213 <- predict(nn23, valid.df)

head(pred213)
summary(pred213)


###classification matrix 
c.mat213 <- table(ifelse(pred213[,1] > 0.5, 1, 0), valid.df$popularity)    #2 way table: row by column
c.mat213   # row: predicted ;   column: actual 

sum(diag(c.mat213))/sum(c.mat213) #accuracy
c.mat213[2,2]/sum(c.mat213[,2])   #sensitivity
c.mat213[1,1]/sum(c.mat213[,1])   #specificity

###stewpwise AIC model from logistric regression project
##Uses Elevation Gain, Average Rating, and Number of Reviews

#nn3 <- neuralnet(Popular + Unpopular ~ elevation_gain  + avg_rating + num_reviews, 
#                 data = train.df, hidden = 3, stepmax = 1e+05, rep = 2, linear.output = FALSE)

nn3$weights
plot(nn3)

pred3 <- predict(nn3, train.df)

head(pred3)
summary(pred3)


###classification matrix 
c.mat3 <- table(ifelse(pred3[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat3   # row: predicted ;   column: actual 

sum(diag(c.mat3))/sum(c.mat3) #accuracy
c.mat3[2,2]/sum(c.mat3[,2])   #sensitivity
c.mat3[1,1]/sum(c.mat3[,1])   #specificity

##validate
pred31 <- predict(nn3, valid.df)

head(pred31)
summary(pred31)


###classification matrix 
c.mat31 <- table(ifelse(pred31[,1] > 0.5, 1, 0), valid.df$popularity)    #2 way table: row by column
c.mat31   # row: predicted ;   column: actual 

sum(diag(c.mat31))/sum(c.mat31) #accuracy
c.mat31[2,2]/sum(c.mat31[,2])   #sensitivity
c.mat31[1,1]/sum(c.mat31[,1])   #specificity




###more layers
#nn33 <- neuralnet(Popular + Unpopular ~ elevation_gain  + avg_rating + num_reviews, 
#                 data = train.df, hidden = 6, stepmax = 1e+05, rep = 2, linear.output = FALSE)

nn33$weights
plot(nn33)

pred33 <- predict(nn33, train.df)

head(pred33)
summary(pred33)


###classification matrix 
c.mat33 <- table(ifelse(pred33[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat33   # row: predicted ;   column: actual 

sum(diag(c.mat33))/sum(c.mat33) #accuracy
c.mat33[2,2]/sum(c.mat33[,2])   #sensitivity
c.mat33[1,1]/sum(c.mat33[,1])   #specificity

##validate
pred331 <- predict(nn33, valid.df)

head(pred331)
summary(pred331)


###classification matrix 
c.mat331 <- table(ifelse(pred331[,1] > 0.5, 1, 0), valid.df$popularity)    #2 way table: row by column
c.mat331   # row: predicted ;   column: actual 

sum(diag(c.mat331))/sum(c.mat331) #accuracy
c.mat331[2,2]/sum(c.mat331[,2])   #sensitivity
c.mat331[1,1]/sum(c.mat331[,1])   #specificity





###MORE layers
#nn34 <- neuralnet(Popular + Unpopular ~ elevation_gain  + avg_rating + num_reviews, 
#                  data = train.df, hidden = 9, stepmax = 1e+05, rep = 2, linear.output = FALSE)

nn34$weights
plot(nn34)

pred34 <- predict(nn34, train.df)

head(pred34)
summary(pred34)


###classification matrix 
c.mat34 <- table(ifelse(pred34[,1] > 0.5, 1, 0), train.df$popularity)    #2 way table: row by column
c.mat34   # row: predicted ;   column: actual 

sum(diag(c.mat33))/sum(c.mat33) #accuracy
c.mat34[2,2]/sum(c.mat34[,2])   #sensitivity
c.mat34[1,1]/sum(c.mat34[,1])   #specificity

##validate
pred331 <- predict(nn34, valid.df)

head(pred331)
summary(pred331)


###classification matrix 
c.mat331 <- table(ifelse(pred331[,1] > 0.5, 1, 0), valid.df$popularity)    #2 way table: row by column
c.mat31   # row: predicted ;   column: actual 

sum(diag(c.mat331))/sum(c.mat331) #accuracy
c.mat331[2,2]/sum(c.mat331[,2])   #sensitivity
c.mat331[1,1]/sum(c.mat331[,1])   #specificity

##print all classification matrices together neatly
c.mat1
c.mat11
c.mat2
c.mat21
c.mat3
c.mat31

c.mat1[2,2]/sum(c.mat1[,2])   #sensitivity
c.mat1[1,1]/sum(c.mat1[,1])   #specificity
c.mat11[2,2]/sum(c.mat11[,2])   #sensitivity
c.mat11[1,1]/sum(c.mat11[,1])   #specificity
c.mat2[2,2]/sum(c.mat2[,2])   #sensitivity
c.mat2[1,1]/sum(c.mat2[,1])   #specificity
c.mat21[2,2]/sum(c.mat21[,2])   #sensitivity
c.mat21[1,1]/sum(c.mat21[,1])   #specificity
c.mat3[2,2]/sum(c.mat3[,2])   #sensitivity
c.mat3[1,1]/sum(c.mat3[,1])   #specificity
c.mat31[2,2]/sum(c.mat31[,2])   #sensitivity
c.mat31[1,1]/sum(c.mat31[,1])   #specificity