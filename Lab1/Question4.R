#Question4
dataset<-c(43, 37, 50, 51, 58, 105, 52, 45, 45, 10)


#Function to find mean,median,sd,all quartiles and return a vector
explore<-function(x){
  data<-c("Mean"=mean(x),"Median"=median(x), 
          "Standard Deviation" = sd(x),"Quartile-" =quantile(x))
  
  return(data)
}
explore(dataset)

##Below Q1-1.5*IQR  or above Q3+1.5*IQR, where Q1 and Q3 are the first and third 
#quartiles, respectively, of the variable distribution and IQR=Q3-Q1 is the 
#interquartile range.

IQR_outliers <- function(x) {
  Q1<-quantile(x,0.25)
  Q3<-quantile(x,0.75)
  IQR<-(Q3-Q1) #inter-quartile range
  left<- (Q1-(1.5*IQR))
  right<- (Q3+(1.5*IQR))
  outliers<-c(x[x <left],x[x>right])
  return(outliers)
}

outliers<-IQR_outliers(dataset)
##Recomputing explore function for dataset without any outliers
reconstructed_dataset<-dataset[!dataset%in%outliers]
print(reconstructed_dataset)
explore(reconstructed_dataset)

