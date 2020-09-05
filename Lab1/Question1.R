#Question-1
before<-c(56, 47, 49, 37, 38, 60, 50, 43, 43, 59, 50, 56, 54, 58)
after<-c(53, 21 , 32, 49, 45, 38, 44, 33, 32, 43, 53, 46, 36, 48, 39, 35, 37, 36,
         39,45)

#a Stem and Leaf plot

stem(before)
stem(after)
stem.leaf.backback(before,after)

#(b) compute the five-point summaries and plot parallel boxplot
summary(before)
summary(after)
boxplot(before,after,main = "Parallel Boxplots",
        xlab=("no of blocked intrusion attempts"),
        names=c('Before','Af'),col = c("orange","red"),
        border = "brown",horizontal = TRUE)

