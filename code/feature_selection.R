install.packages("pacman")
library(pacman)
pacman::p_load(Hmisc, data.table, tidyverse, caret, psych, caTools, randomForest, ROSE)
data = fread("../data/assignment1_data.csv")

# discard data columns that are not used e.g. id
id_list = names(data)[which(str_detect(names(data), pattern = "_id"))]

# variables with more than 50% null values
null_list  = names(data)[which(apply(data, MARGIN = 2, FUN=function(x) length(which(str_detect(x, "NULL")))/nrow(data))>0.5)]

# discard time
time_list = names(data)[which(str_detect(names(data), pattern = "time"))]
stay_list = names(data)[which(str_detect(names(data), pattern = "stay"))]

data = data %>% select(-id_list, -time_list, -stay_list, -null_list)
data = data %>% mutate(gender=ifelse(gender=="M", T, F)) %>% mutate(ethnicity = as.factor(ethnicity))

numeric_index = c(2,3,6,26:(ncol(data)-2))
factor_index = (1:ncol(data))[!(1:ncol(data) %in% numeric_index)]
to_numeric = names(data)[numeric_index]
to_factor = names(data)[factor_index]
data = data %>% mutate_at(to_numeric, .funs = as.numeric) %>% mutate_at(to_factor, as.factor)

# replace NA according to their class
for(index  in 26:(ncol(data)-2)){
  data[, index][data$hospital_expire_flag==1 & is.na(data[, index])]<-mean(data[, index][data$hospital_expire_flag==1], na.rm = T)
  data[, index][data$hospital_expire_flag==0 & is.na(data[, index])]<-mean(data[, index][data$hospital_expire_flag==0], na.rm = T)
}

# center and rescale all the continuous variables
data = data %>% mutate_at(to_numeric, .funs = function(x) (x-mean(x))/sd(x))

# feature selection
Xdata = model.matrix(~.-1, data = data)
Xdata = as.data.frame(Xdata)

# rename certain odd variable names
names(Xdata)[6] = "ethnicityBLACK_AFRICAN_AMERICAN"
names(Xdata)[7] = "ethnicityHISPANIC_LATINO"
names(Xdata)[9] = "ethnicityUNABLE_TO_OBTAIN"
names(Xdata)[12] = "hospital_expire_flag" 
# names(Xdata)[98] = "glucose_min_2"
# names(Xdata)[99] = "glucose_max_2"

# determining by feature importance with random forest
fit = randomForest(factor(hospital_expire_flag)~., data = temp)
ranking = varImp(fit, type = 2, sort = T, n.var = 20) # type 2 to indicate importance
selected = row.names(ranking)[order(ranking, decreasing = T)][1:20]

# using the selected variables, fit it to rf, increasing the number of variables one at a time
f.model = map(1:20, .f = function(x){
    temp = Xdata %>% select(selected[1:x], hospital_expire_flag)
    fit = randomForest(factor(hospital_expire_flag)~., data = temp)
  }
)
# compute the roc for each fit and find the number of variables where the increase in AUROC is minimal
result = pmap(list(x = f.model, col = colorRampPalette(c("#FF5733", "#C70039", "#FFC300", "#581845"))(12)), .f = function(x, col){ 
  t = predict(x, type = "prob")
  return(roc.curve(factor(Xdata$hospital_expire_flag), predicted = t[,2], plotit = T, col=col, lwd=2))
})

## final selected variables
# selected = c("los_hospital", "los_icu", "spo2_mean", "pt_max", "glucose_mean", "sbp_min", "spo2_min", "pt_min", "ptt_min", "temperature_mean")
##
cutoff=10 # determined from the result above
XXdata = Xdata %>% select(selected[1:cutoff], hospital_expire_flag)

# split data into test and train sets
sample = sample.split(XXdata$hospital_expire_flag, SplitRatio = 0.8)
train = subset(XXdata, sample==T)
test = subset(XXdata, sample==F)

# rebalance the train dataset using by synthetic data generation
rose.train<-ovun.sample(hospital_expire_flag~., data=train, seed=1)$data
# convert to class of inputs (No/Yes) for training models
rose.train$hospital_expire_flag= as.factor(ifelse(rose.train$hospital_expire_flag==0, "No", "Yes"))
test$hospital_expire_flag= as.factor(ifelse(test$hospital_expire_flag==0, "No", "Yes"))

## save the test and train files
# fwrite(rose.train, "../data/rosetrain.csv")
# fwrite(test, "../data/test.csv")