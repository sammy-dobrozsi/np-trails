trails.df <- read.csv("~/trails_file_location/Trails.xlsx.csv")

trails.df$trail_id <- as.character(trails.df$trail_id)
trails.df$visitor_usage <- as.numeric(trails.df$visitor_usage)
trails.df$visitor_usage[is.na(trails.df$visitor_usage)] <- mean(trails.df$visitor_usage,na.rm=TRUE)
trails.df$visitor_usage <- signif(trails.df$visitor_usage, digits = 1)
##visitor_usage was a mess, needed to be recoded numeric, mean imputed missing values, then had to convert the mean imputed values to whole numbers.  I probably should have done nothing
View(trails.df)
summary(trails.df)
str(trails.df)

library(mice)
library(reshape2)
library(ggplot2)

cor(trails.df[, c(2, 3, 4, 5, 7, 8, 9)])
cormat <- round(cor(trails.df[, c(2, 3, 4, 5, 7, 8, 9)]), 3)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
} ##get just the upper triangle of correlation values to reduce noise in cor matrix
upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri) #reshape2 correlation matrix melted values for visual

ggheatmap <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() ##melted correlation matrix, upper triangle only


ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1.11, 0.41),
    legend.direction = "vertical") + ##add text with values
    labs(title = "Correlation Matrix of Numeric Variables",
         subtitle = "Excludes Route Type")

##Histograms
h1 <- ggplot(trails.df, aes(x=popularity)) +
  geom_histogram(binwidth=2) +
  theme_minimal()+
  xlim(0,100) +
  labs(x="Trail Popularity Index",
       y= "Number of Trails")
h2 <- ggplot(trails.df, aes(x=elevation_gain)) +
  geom_histogram(binwidth=250) +
  xlim(0,10000) +
  theme_minimal()+
  labs(x="Trail Elevation Gain",
       y=NULL)
h3 <- ggplot(trails.df, aes(x=length)) +
  geom_histogram(binwidth=3000) +
  theme_minimal()+
  xlim(0,70000) +
  labs(x="Trail Length",
       y=NULL)
h4 <- ggplot(trails.df, aes(x=difficulty_rating)) +
  geom_histogram() +
  scale_x_continuous(breaks=c(1,3,5,7), lim=c(0.5,7.5)) +
  theme_minimal()+
  labs(x="Trail Difficulty Rating",
       y=NULL)
h5 <- ggplot(trails.df, aes(x=visitor_usage)) +
  geom_histogram() +
  theme_minimal() +
  labs(x="Amount of Visitor Usage",
       y="Number of Trails")
h6 <- ggplot(trails.df, aes(x=avg_rating)) +
  geom_histogram() +
  theme_minimal() +
  labs(x="Average User Rating",
       y=NULL)
h7 <- ggplot(trails.df, aes(x=num_reviews)) +
  geom_histogram() +
  theme_minimal() +
  labs(x="Number of Visitor Reviews (all)",
       y=NULL)
h8 <- ggplot(trails.df, aes(x=num_reviews)) +
  geom_histogram() +
  xlim(1,4001) +
  ylim(0, 500) +
  theme_minimal() +
  labs(x="Number of Visitor Reviews (n > 0)",
       y=NULL)

##arrange histograms into compact single plot
library(gridExtra)
h_all <- grid.arrange(h1, h2, h3, h4, h5, h6, h7, h8, nrow = 2)
h_all

##parallel coordinate plots by visitor usage level
par(mfcol=c(4,1))
parcoord(trails.df[trails.df$visitor_usage == 1, c(-1, -6, -7)], main = "Parallel coordinate plots for different levels of visitor usage", sub = "Visitor Usage = 1")
parcoord(trails.df[trails.df$visitor_usage == 2, c(-1, -6, -7)], sub = "Visitor Usage = 2")
parcoord(trails.df[trails.df$visitor_usage == 3, c(-1, -6, -7)], sub = "Visitor Usage = 3")
parcoord(trails.df[trails.df$visitor_usage == 4, c(-1, -6, -7)], sub = "Visitor Usage = 4")
par(mfcol=c(1,1))

##boxplots

library(tidyverse)
library(gridExtra)
library(ggthemes)
library(magrittr)
trails.df.temp <- trails.df
trails.df.temp %>%
  group_by(route_type) %>%
  mutate(count = n()) %>%
  ggplot(trails.df, mapping = aes(popularity, route_type, group = as.numeric(route_type))) +
  theme_tufte(base_size = 15) +
  theme(line=element_blank()) +
  geom_violin(fill = "white") +
  geom_boxplot(fill = "black", alpha = 0.3, width = 0.1) +
    ylab("Route Type") +
    xlab("Trail Popularity") +
  labs(title = "Violin + Boxplot of Trail Popularity by Route Type",
       subtitle = "Means are close together, but most popular trails are Out-And-Back or a Loop") +
    geom_text(aes(label = paste0("n =", count),
                  x = (as.numeric(route_type) + 30),
                  y = (as.numeric(route_type) + 0.1)), size = 5)
  
  

library(RColorBrewer)
ggplot(trails.df, aes(popularity, visitor_usage)) +
  geom_point(position = position_jitter(width = 0.23), aes(color = as.factor(avg_rating)), size = 2.7, alpha = 0.75) +
  scale_color_hue() +
  xlim(0,50) +
  labs(color="Average User Rating",
       x = "Trail Popularity Index",
       y = "Amount of Visitor Usage",
       title = "Trail Popularity of Different Visitor Usage Levels",
       subtitle = "Average user rating increases with Visitor Usage and Popularity")

p1 <- ggplot(trails.df, aes(num_reviews, popularity)) +
  geom_point(position = position_jitter(width = 0.33), alpha = 0.75) +
  geom_smooth(method=lm, se=TRUE) +
  ylim(0,100) +
  labs(x="Number of User Reviews",
       y="Trail Popularity Index")

p2 <- ggplot(trails.df, aes(num_reviews, popularity)) +
  geom_point(position = position_jitter(width = 0.33), alpha = 0.6) +
  geom_smooth(method=lm, se=TRUE) +
  ylim(0,50) + 
  xlim(0, 500) +
  labs(x="Trails With <500 User Reviews",
       y="Trail Popularity Index")

p3 <- ggplot(data=trails.df, aes(elevation_gain, popularity)) +
  geom_point(position = position_jitter(width = 0.33)) +
  geom_smooth(method=lm, se=TRUE) + 
  ylim(0,100) +
  labs(x="Elevation Gain",
       y="Trail Popularity Index")

p4 <- ggplot(data=trails.df, aes(length, popularity)) +
  geom_point(position = position_jitter(width = 0.33)) +
  geom_smooth(method=lm, se=TRUE) + 
  ylim(0,100) +
  labs(x="Length",
       y="Trail Popularity Index")

p5 <- ggplot(data=trails.df, aes(elevation_gain, num_reviews)) +
  geom_point(position = position_jitter(width = 0.33)) +
  geom_smooth(method=lm, se=TRUE) + 
  ylim(0,2500) +
  labs(x="Elevation Gain",
       y="Number of User Reviews")

p6 <- ggplot(data=trails.df, aes(length, num_reviews)) +
  geom_point(position = position_jitter(width = 0.33)) +
  geom_smooth(method=lm, se=TRUE) + 
  ylim(0,2500) +
  labs(x="Length",
       y="Number of User Reviews")

p_two <- grid.arrange(p1, p2, nrow = 1)
p_two

p_six <- grid.arrange(p3, p4, p5, p6, nrow = 2)
p_six

ggplot(data = trails.df, aes(visitor_usage, popularity)) +
  geom_point(position = position_jitter(width = 0.3)) + 
  facet_wrap(~route_type)

  
ggplot(data = trails.df, aes(visitor_usage, avg_rating)) +
  geom_point(position = position_jitter(width = 0.3)) + 
  facet_wrap(~route_type) 