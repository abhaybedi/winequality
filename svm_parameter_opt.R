library(kernlab)
library(caret)
#2500 sample size (95% confidence , 2% error p = 0.5)
data <- read.csv("winequality-white.csv")

# Set seed for reproducibility
set.seed(98)

# Create 10 samples of size 2500
sample_size <- 2000
num_samples <- 10
samples <- list()

for (i in 1:num_samples) {
  sample_indices <- sample(1:nrow(data), sample_size)
  sample_data <- data[sample_indices, ]
  samples[[i]] <- sample_data
}
# 
# Save samples as separate CSV files
for (i in 1:num_samples) {
  write.csv(samples[[i]], paste0("sample_", i, ".csv"), row.names = FALSE)
}

kernelList<-c('rbfdot','polydot','vanilladot','tanhdot','laplacedot','anovadot')

result <- data.frame(Sample=character(),best_accuracy=numeric(),bestKernel=character(),bestNu=numeric(),bestEpsilon=numeric())

fitFunc<-function(k,n,e,df)
{
  #k=kernel , n = nu, e epsilon
  df$quality <- as.factor(df$quality)
  trainIndex <- createDataPartition(df$quality, p = 0.7, list = FALSE)
  train <- df[trainIndex, ]
  test <- df[-trainIndex, ]
  model <-ksvm(quality ~ ., data=train,kernel=k,nu=n,epsilon=e,kpar=list())
  prediction=predict(model,test)
  
  conf_mat <- table(prediction, test$quality)
  acc <- sum(diag(conf_mat)) / sum(conf_mat)
  return (acc)
}

for(i in 1:num_samples)

{
  df<-read.csv(paste0("sample_",i,".csv"))
  bestAccuracy<-0
  bestNu<-0
  bestEpsilon<-0
  bestKernel<-''

  for(j in 1:1000)
  {
    k=sample(kernelList,1)
    n=runif(1)
    e=runif(1)
    tryCatch({
      Accuracy = fitFunc(k,n,e,df)
      if(Accuracy>bestAccuracy)
      {
        bestKernel=k
        bestNu=n
        bestEpsilon=e
        bestAccuracy=Accuracy
      }
    }, warning = function(w) {
      message("Warning: ", w$message)
    }, error = function(e) {
      message("Error: ", e$message)
    })
    
    print(paste0("iteration :",i," ",j," completed"))

  }
  new_row=c(paste0("S",i),bestAccuracy,bestKernel,bestNu,bestEpsilon)
  result=rbind(result,new_row)

}
colnames(result)=c("sample","best_accuracy","best_kernel","best_nu","best_epsilon")
write.csv(result,"results.csv",row.names = FALSE)

#best sample S2
iter <- c()
accuracies <- c()
df <- read_csv("sample_2.csv")
for (i in 1:1000) {
  tryCatch({
    iter <- c(iter, i)
    k <- sample(kernelList, 1)
    n <- runif(1)
    e <- runif(1)
    
    accuracies <- c(accuracies, fitFunc(k, n, e, df))
    print(paste0("iterations : ",i," completed"))
  }, error = function(e) {
    message("Error occurred at iteration ", i, ": ", e$message)
  })
}
iter=iter[1:996]
plot(iter, accuracies,type="l")
length(iter)
length(accuracies)
plot(accuracies)
