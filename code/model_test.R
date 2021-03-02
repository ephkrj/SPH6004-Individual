install.packages("pacman")
library(pacman)
pacman::p_load(Hmisc, data.table, tidyverse, caret, psych, caTools, randomForest, ROSE, varhandle, JOUSBoost, adabag, e1071)
synthetic.ada<-readRDS("../model/ada.rds")
synthetic.gbm<-readRDS("../model/gbm.rds")
synthetic.rf<-readRDS("../model/rf.rds")
synthetic.svm<-readRDS("../model/svm.rds")
synthetic.log<-readRDS("../model/log.rds")

# loading test set
test = fread("../data/test.csv")

model.list = list(synthetic.log, synthetic.svm, synthetic.rf, synthetic.gbm, synthetic.ada)

# print out summary (Table 1)
for(i in 1:length(model.list)){
  if(i!=2){
    # for svm, due to the use of the svm library instead of caret::train, the way to call the model differs slightly from the rest
    print(confusionMatrix(reference = factor(unlist(subset(test, select = hospital_expire_flag))), 
                predict(model.list[[i]], newdata=subset(test, select = -hospital_expire_flag)), 
                positive = "Yes"))
  }else{
    print(confusionMatrix(reference = factor(unlist(subset(test, select = hospital_expire_flag))), 
                          factor(predict(model.list[[i]], newdata=subset(test, select = -hospital_expire_flag), probability = T)), 
                          positive = "Yes"))
  }
}

colors = c("#d7191c",
  "#fdae61",
  "#018571",
  "#abd9e9",
  "#2c7bb6")

# output for figure 3
png('../ROC.png',height=8,width=12,units='cm',res=900,pointsize=8)
for(i in 1:length(model.list)){
  if(i==1){
    roc.curve(response = factor(unlist(subset(test, select = hospital_expire_flag))), 
            predicted = predict(model.list[[i]], newdata=subset(test, select = -hospital_expire_flag), type = "prob")[,2], 
            col=colors[i])
  }else if(i==2){
    # for svm, due to the use of the svm library instead of caret::train, the way to call the model differs slightly from the rest
    t = predict(model.list[[2]], newdata=subset(test, select = -hospital_expire_flag), probability =  T)
    roc.curve(response  = factor(unlist(subset(test, select = hospital_expire_flag))), 
              predicted = attr(t, "probabilities")[,2], 
              add.roc = T,
              col=colors[i], 
              lwd=2)
  }
  else{
    roc.curve(response  = factor(unlist(subset(test, select = hospital_expire_flag))), 
              predicted = predict(model.list[[i]], newdata=subset(test, select = -hospital_expire_flag), type = "prob")[,2], 
              add.roc = T,
              col=colors[i], 
              lwd=2)
  }
}

legend("bottomright", legend=c("Logistic", "SVM", "RF", "GBM", "ADA"),
       col=colors, lty=1, cex=0.8)
dev.off()
