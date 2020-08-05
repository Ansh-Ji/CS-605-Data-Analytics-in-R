user<-c(17.2, 22.1, 18.5, 17.2, 18.6, 14.8, 21.7, 15.8, 16.3, 22.8,  24.1, 13.3, 
        16.2, 17.5, 19.0,23.9, 14.8, 22.2, 21.7, 20.7 ,13.5 ,15.8, 13.1, 16.1, 
        21.9, 23.9, 19.3, 12.0, 19.9, 19.4, 15.4, 16.7, 19.5, 16.2, 16.9, 17.1, 
        20.2, 13.4, 19.8, 17.7, 19.7 ,18.7, 17.6, 15.9, 15.2 ,17.1 ,15.0, 18.8, 
        21.6 ,11.9)
#a Compute the sample mean, variance, and standard deviation of the
#number of concurrent users.
explore<-function(x){
  data<-c("Mean"=mean(x),"Median"=median(x), 
          "Standard Deviation" = sd(x))
  return(data)
}
#explore(user)

#b Estimate the standard error of the sample mean.
#Standard Error = s.D/sqrt(n)
standard_error<-function(x){
  se<-("Standard Error" = sd(x)/sqrt(length(x)))
  return(se)
}
standard_error(user)
#can use psych package
describe(user,IQR=T)

#c Five point summary and box plot
summary(user)
boxplot(user,  
        main="Network Load",
        xlab = "Location",
        ylab = "No of concurrent users in thousands",
        col = "orange"
        )

#d Compute the interquartile range. Are there any outliers?
# IQR is measure of stastical dispersion IQR=Q3-Q1
Q1<-quantile(user,0.25)
Q3<-quantile(user,0.75)
IQR<-Q3-Q1
left<- (Q1-(1.5*IQR))
right<- (Q3+(1.5*IQR))
outliers<-c(user[user <left],user[user>right])
#outliers
# NO outliers

#e
plot_histogram(user)
#YES

