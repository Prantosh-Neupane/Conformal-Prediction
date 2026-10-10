install.packages("mice")
library(mice)
#Saved data part as csv file
hospital <- read.csv(
  "C:/Users/srgovar/Downloads/hospital_dataset.csv",
  na.strings = c("", "NA"),
)
names(hospital)<-c("ID","Age","Sex","CP","BP","Chl","FBS","ECG","EAng","Dep","Sl","MV","ST","Diag","hosp"                   )
hospital
#check few rows
head(hospital)
#variable names
names(hospital)

#checks number of missing values in each variable
colSums(is.na(hospital))

#Cholestrol values are 0 especially for switzerland site. 
#Make it NA so that we can impute data for that variable.
hospital[[6]][hospital[[6]] == 0 & !is.na(hospital[[6]])] <- NA

# Outcome is column 15
Y <- hospital[[15]]

# Remove ID and outcome
hospital.x <- hospital[, -c(1, 15)]

# Convert categorical predictors to factors
for (j in c(3, 4, 7, 8, 10, 12, 13, 14, 16)) {
  hospital.x[[names(hospital)[j]]] <- factor(hospital[[j]])
}

# Create 5 completed datasets
comp <- mice::mice(hospital.x, m = 5, seed = 123)

#first of the five completed datasets
hospital.complete <- complete(comp, 1)
#check whether mice imputed all values
colSums(is.na(hospital.complete))
par(mfrow = c(2, 3))

#do a boxplot for numeric variables
for (j in 1:ncol(hospital.x)) {
  
  if (is.numeric(hospital.x[[j]])) {
    
    boxplot(
      list(
        Original = hospital.x[[j]],
        Completed = hospital.complete[[j]]
      ),
      main = names(hospital.x)[j],
      col = c("skyblue", "orange")
    )
    
  }
}

 

par(mfrow = c(1, 1))
#design matrix
X <- model.matrix(~ ., data = hospital.complete)

# Standardize predictors
X[, -1] <- scale(X[, -1])

#assign n and p
n <- nrow(X)
p <- ncol(X)
#keep original value for y comparison 
Y.actual <- Y[n]
# CRR algorithm start
I <- diag(1, p)
a <- 5
#Hat matrix
H <- X %*% solve(t(X) %*% X + a * I) %*% t(X)

In <- diag(1, n)
C <- In - H

Y[n] <- 0

A <- C %*% Y
#make all n-1 o's and last one as 1.
zero <- c(rep(0, n - 1), 1)
B <- C %*% zero

epsilon <- 0.05

l <- numeric(n - 1)
u <- numeric(n - 1)

for (i in 1:(n - 1)) {
  
  if (B[n] - B[i] > 0) {
    
    l[i] <- (A[i] - A[n]) / (B[n] - B[i])
    u[i] <- l[i]
    
  } else {
    
    l[i] <- -Inf
    u[i] <- Inf
    
  }
}

l <- sort(l)
u <- sort(u)

k.lower <- floor((epsilon / 2) * n)
k.upper <- ceiling((1 - epsilon / 2) * n)
#smallest lower cutoff and smallest upper cutoff
lower <- l[k.lower]
upper <- u[k.upper]
#display together
c(lower = lower, upper = upper)

Y.actual
#check whether test patient actual diagnosis lies inside interval
Y.actual >= lower & Y.actual <= upper

