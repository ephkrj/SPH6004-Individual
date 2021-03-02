install.packages("pacman")
library(pacman)
pacman::p_load(Hmisc, data.table, tidyverse, caret, psych, caTools, randomForest, parallel, doParallel, varhandle, JOUSBoost, adabag, e1071)

registerDoParallel(detectCores()-1)
# setup some parameters during training, such as the number of cross-validations etc
fitControl<-trainControl(method = "cv", number = 5, allowParallel = T, classProbs = T, summaryFunction = twoClassSummary)

# load training data
rose.train<-fread("../data/rosetrain.csv")

########################
# logistic regression #
######################
logr<-train(factor(hospital_expire_flag)~., data = rose.train, method='glm', family="binomial", trControl=fitControl)
# saveRDS(logr, "../model/log.rds")

########
# svm #
######
svmRadial<-svm(factor(hospital_expire_flag)~., data = rose.train, probability = T, kernel = "radial")
# originally had intended to train with the below command, but caret::train is notorious for taking time to train due to its inbuild automatic tuning
# svmRadial<-train(factor(hospital_expire_flag)~., data = rose.train, method='svmRadial', trControl=fitControl, metric="ROC")
# saveRDS(svmRadial, "../model/svm.rds")

########################
# random forest model #
######################
rf<-randomForest(as.factor(hospital_expire_flag)~., data = rose.train)
# originally had intended to train with the below command, but caret::train is notorious for taking time to train due to its inbuild automatic tuning
# rf<-train(factor(hospital_expire_flag)~., data = rose.train, method='rf', trControl=fitControl, metric="ROC")
# saveRDS(rf, "../model/rf.rds")

######################
# gradient boosting #
####################
gbm<-train(factor(hospital_expire_flag)~., data = rose.train, method='gbm', trControl=fitControl, metric="ROC")
# saveRDS(gbm, "../model/gbm.rds")

#############
# adaboost #
###########
ada<-train(factor(hospital_expire_flag)~., data = rose.train, method='adaboost', trControl=fitControl, metric="ROC")
# saveRDS(ada, "../model/ada.rds")
