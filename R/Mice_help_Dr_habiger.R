###################################

### Multiple Imputation via FCS ###

###################################

install.packages("mice")

library(mice)

help(mice)

data(nhanes2)

help(nhanes2)

## see defaultMethod in the help file for details

## create 5 complete data sets

comp<-mice(nhanes2)

cbind(nhanes2, complete(comp,1))[1:5,]

cbind(nhanes2, complete(comp,2))[1:5,]

cbind(nhanes2, complete(comp,5))[1:5,]



### Get theta_d and V_d's

summaries<-matrix(nrow = 5, ncol = 2)

colnames(summaries)<-c("theta_d","V_d")

for(i in 1:5){
  
  summaries[i,1]<-mean(complete(comp,i)$chl)
  
  summaries[i,2]<-var(complete(comp,i)$chl)/25
  
  plot(complete(comp,i)$chl)
  
  print(summaries[i,])
  
  readline()
  
}



summaries



### get Theta hat and Vbar below

means<-apply(summaries, 2, mean)



## get B

B<-var(summaries[,1])



thet.hat<-means[1]

se.hat<-sqrt(means[2] + 6*B/5)



c(thet.hat - 2*se.hat, thet.hat + 2*se.hat)