diab <- read.csv(choose.files())
str(diab)
diab$Is_Diabetic <- as.factor(diab$Is_Diabetic)
str(diab)
diab$Is_Diabetic <- as.numeric(diab$Is_Diabetic)
str(diab)
diab$Is_Diabetic <- ifelse(diab$Is_Diabetic==2,1,0)
str(diab)

############splitting the data
library(caTools)
set.seed(56)
split <- sample.split(diab$Is_Diabetic, SplitRatio = 0.75)
training <- subset(diab, split==T)
test <- subset(diab, split==F)

###################Decision Tree
library(rpart)
names(diab)
dec_tree <- rpart(Is_Diabetic~., data = training)

dec_tree_pred <- predict(dec_tree, newdata = test)
dec_tree_pred
dec_tree_pred_thr <- ifelse(dec_tree_pred>=0.5,1,0)
dec_tree_pred_thr

#####Confusion Matrix

cm <- table(test$Is_Diabetic,dec_tree_pred_thr)
cm
library(caret)
library(e1071)
cm_dt <- confusionMatrix(cm)
cm_dt

plot(dec_tree)
text(dec_tree)
library(rattle)
fancyRpartPlot(dec_tree)

# Decision Tree : Accuracy : 0.7135

#######RandomForest

install.packages("randomForest")
library(randomForest)
names(diab)
ran_for <- randomForest(Is_Diabetic~., data=training, ntree=1000)

ran_for
ran_for_pred <- predict(ran_for, newdata = test)
ran_for_pred
ran_for_pred_thr <- ifelse(ran_for_pred>=0.5,1,0)

cm <- table(test$Is_Diabetic, ran_for_pred_thr)
confusionMatrix(cm)

# Decision Tree : Accuracy : 0.7135
# Random Forest : Accuracy : 0.7344

#################Naive Bayes Theorem 
install.packages('class')
library(class)
library(e1071)
training$Is_Diabetic <- factor(training$Is_Diabetic)
test$Is_Diabetic <- factor(test$Is_Diabetic)
naive_classifier <- naiveBayes(Is_Diabetic~., data=training)
naive_classifier
naive_pred <- predict(naive_classifier, newdata = test) 
naive_pred
cm <- table(test$Is_Diabetic, naive_pred)
confusionMatrix(cm)

# Decision Tree : Accuracy : 0.7135
# Random Forest : Accuracy : 0.7344
# Naive Bayes Theorem : Accuracy : 0.7135

################SUPPORT VECTOR MACHINE#################333

# KERNAL - APPROACH
library(class)

SVM_LINEAR <- svm(Is_Diabetic~., data=training, kernel='linear')
SVM_LINEAR_pred <- predict(SVM_LINEAR, newdata = test)
SVM_LINEAR_pred

cm <- table(test$Is_Diabetic, SVM_LINEAR_pred)
confusionMatrix(cm)


# Decision Tree : Accuracy : 0.7135
# Random Forest : Accuracy : 0.7344
# Naive Bayes Theorem : Accuracy : 0.7135
# SVM (Linear) : Accuracy : 0.7344

SVM_LINEAR <- svm(Is_Diabetic~., data=training, kernel='polynomial')
SVM_LINEAR_pred <- predict(SVM_LINEAR, newdata = test)
SVM_LINEAR_pred

cm <- table(test$Is_Diabetic, SVM_LINEAR_pred)
confusionMatrix(cm)

# Decision Tree : Accuracy : 0.7135
# Random Forest : Accuracy : 0.7344
# Naive Bayes Theorem : Accuracy : 0.7135
# SVM (Linear) : Accuracy : 0.7344
# SVM (Polynomial) : Accuracy : 0.7135

