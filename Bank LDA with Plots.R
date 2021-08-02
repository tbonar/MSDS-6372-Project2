library(tidyverse)
library(MASS)
library(caret)
library(naniar)
library(GGally)
library(funModeling)
library(Hmisc)

#find numeric columns
str(full_bank_2)

#create data frame with only numeric columns and response variable
bank.num <- full_bank_2[,c(1,12:14,16:21)]

#remove 999 from pdays column as this is an outlier representing customers not contacted in previous campaign 
bank.num <- bank.num %>% filter(pdays != 999)

#plot correlation between predictors
ggpairs(bank.num, columns = c(1:9), aes(colour = y))

#create training test split
set.seed(123)
training.samples <- bank.num$y %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- bank.num[training.samples,]
test.data <- bank.num[-training.samples,]

#center and scale data, produced nearly identical results so probably not needed

#preproc.param <- train.data %>% preProcess(method= c("center", "scale"))
#train.transformed <-preproc.param %>% predict(train.data)
#test.transformed <-preproc.param %>% predict(test.data)

#create lda model
mylda <- lda(y ~ ., data = train.data)
mylda

#make predictions and find accuracy
predictions <- mylda %>% predict(test.data)
mean(predictions$class==test.data$y)
CM = confusionMatrix(table(predictions$class,test.data$y))
CM

#plot ld1
plot(mylda)

#overlay ld1 plot for each category
lda.data <- cbind(train.data, predict(mylda)$x)
ggplot(lda.data, aes(x=LD1)) + geom_histogram(aes(fill=y))


#From https://towardsdatascience.com/linear-discriminant-analysis-lda-101-using-r-6a97217a55a6

#grab the posterior from the predictions
predictions.posteriors <- as.data.frame(predictions$posterior)

#create values for and plot ROC 
pred <- prediction(predictions.posteriors[,2], test.data$y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
# Plot
plot(roc.perf)
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
