getwd()
setwd("C:/Users/hyadaval/Downloads")
train = read.csv('train2.csv')
test = read.csv('test.csv')
lol = which(train$is_promoted == 0)
lol1 = which(train$is_promoted == 1)


# Handling class imbalance problem using unbalanced package and 
# using the method ubSMOTE which will perform minority oversampling


library(unbalanced)
n = ncol(train)
output = train$is_promoted
output = as.factor(output)
input = train[,-n]

data_train = ubBalance(X= input, Y=output, type="ubSMOTE", verbose= TRUE)
View(data_train)

View(data_train$X)
View(data_train$Y)

is_promoted = data_train$Y
# finally creating our dataset which does not have class imbalanced

balanced_train = cbind(data_train$X , is_promoted)



# -------------- Random Forest Implementation -------------

library(tidyverse)


train_data <- balanced_train %>% sample_frac(.70)
summary(train_data)

val_data = anti_join(balanced_train, train_data)












#head(balanced_train)
#ummary(balanced_train)
#str(balanced_train)

# not considering employee_id and gender

train_data = train_data %>%
  select(-c(employee_id,gender))
  
# Convert categorical variables into factors

train_data = train_data %>%
  mutate_at(c("department", "region", "education", "recruitment_channel" ),factor)

str(train_data)


library(randomForest)

#train_data <- head(balanced_train, n = 22000)
#summary(train_data)

rf <-randomForest(is_promoted~., data=train_data, ntree=151, na.action=na.exclude, importance=T
                  
                  ) 
print(rf)


rf2 <-randomForest(is_promoted~., data=val_data, ntree=151, na.action=na.exclude, importance=T
                  
) 
print(rf2)


# predicted values


pred1 = predict(rf)

pred2 = predict(rf2)

pred3 = predict(rf, test2)

print(pred3)

write.csv(pred3, file = "final.csv")


library(MLmetrics)

F1_Score(y_pred = pred3, y_true=val_data$is_promoted, positive = "0")


# ----------- test data ---------------

#getwd()
#setwd("C:/Users/hyadaval/Downloads")
#train = read.csv('train2.csv')
test2 = read.csv('test2.csv')


# Handling class imbalance problem using unbalanced package and 
# using the method ubSMOTE which will perform minority oversampling


#library(unbalanced)
#n = ncol(test2)
#output2 = test2$is_promoted
#output2 = as.factor(output2)
#input = test2[,-n]
#data_test = ubBalance(X= input, Y=output2, type="ubSMOTE", verbose= TRUE)
#View(data_test)

#View(data_test$X)
#View(data_test$Y)

#is_promoted = data_test$Y
# finally creating our dataset which does not have class imbalanced

#balanced_test = cbind(data_test$X , is_promoted)




pred2 = predict(rf, test2)




F1_Score(y_pred = pred1, y_true=pred2, positive = "0")


