#Question-03

dataset1<-c(19, 24, 12, 19, 18, 24, 8, 5, 9, 20, 13, 11, 1, 12, 11, 10, 22, 21, 7, 16,
        15, 15, 26, 16, 1, 13, 21, 21, 20, 19)
dataset2<-c(17, 24, 21, 22, 26, 22, 19, 21, 23, 11, 19, 14, 23, 25, 26, 15, 17, 26,
        21, 18, 19, 21, 24, 18, 16, 20, 21, 20, 23, 33)
dataset3<-c(56, 52, 13, 34, 33, 18, 44, 41, 48, 75, 24, 19, 35, 27, 46, 62, 71, 24,
        66, 94, 40, 18, 15, 39, 53, 23, 41, 78, 15, 35)
#function to plot histogram
plot_histogram<-function(x){
  p<-ggplot(data=data.frame(data=x),aes(x=data))+
    geom_histogram(aes(y=stat(ndensity)),bins=10)+
    geom_density(aes(y=stat(scaled)),fill = "red",alpha = 0.1)
  plot(p)
}
print(skew<-describe(dat1)) # Uses psych package
#function to calculate sample mean and median
sample_mean_median<-function(x){
  data<-c("Mean"=mean(x),"Median"=median(x))
  return(data)}
