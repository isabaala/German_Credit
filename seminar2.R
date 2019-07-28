#------------------------------------------PACKAGES INSTALLATION------------------------------------------

install.packages("boot")
install.packages("knitr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("reshape2")
install.packages("RColorBrewer")
install.packages("GGally")
install.packages("ggplot2")
install.packages("caret")
install.packages("glmnet")
install.packages("nnet")
install.packages("verification")
library(knitr)
library(dplyr)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(GGally)
library(ggplot2)
library(gridExtra)
library(caret)
library(glmnet)
library(nnet)
library(boot)
library(psych)
library(verification)

#------------------------------------------END PACKAGES INSTALLATION------------------------------------------
##############################################################################################################
#------------------------------------------DATA IMPORTATION---------------------------------------------------

# data loading
credit<-read.delim("F:\\seminar2\\germancredit.txt", header = TRUE, sep=";", colClasses = "factor")

# data selection: removing the id column
credit <- credit[,-1]
str(credit)



#------------------------------------------VARIABLES MANIPULATION---------------------------------------------------

# data transformation: changing class factor to integer (4 variables are numerical only: duration, amount,install_rate,age, num_credits, num,dependents
colnames(credit)
credit$DURATION<- as.numeric(as.character(credit$DURATION))
credit$AMOUNT<-as.numeric(as.character(credit$AMOUNT))
credit$INSTALL_RATE<-as.numeric(as.character(credit$INSTALL_RATE))
credit$AGE<-as.numeric(as.character(credit$AGE))
credit$NUM_CREDITS<-as.numeric(as.character(credit$NUM_CREDITS))
credit$NUM_DEPENDENTS<-as.numeric(as.character(credit$NUM_DEPENDENTS))

# renaming levels
levels(credit$RESPONSE)<- c('Bad', 'Good')
levels(credit$HISTORY)<- c('No credits', 'All credits at this bank paid back duly', 'Existing credits paid back duly',
                           'Delay in paying off in the past', 'Critical account')
levels(credit$NEW_CAR)<- c('No', 'Yes')
levels(credit$USED_CAR)<- c('No', 'Yes')
levels(credit$FURNITURE)<- c('No', 'Yes')
levels(credit$RADIOTV)<- c('No', 'Yes')
levels(credit$EDUCATION)<- c('No', 'Yes')
levels(credit$RETRAINING)<- c('No', 'Yes')

levels(credit$CHK_ACCT)<- c('< 0DM', 'Between 0DM and <200DM',
                            'Equal or more than 200DM', 'No checking account')

levels(credit$SAV_ACCT)<- c('<100DM', 'Between 100DM and < 500DM',
                            'Between 500DM and < 1000DM', 'Equal or more than 1000DM','Unknown/No savings account')


levels(credit$EMPLOYMENT)<- c('Unemployed', '< 1yr','Between 1yr and < 4yrs',
                              'Between 4yrs and < 7yrs','Equal or more than 7yrs')

levels(credit$JOB)<- c('Unemployed/unskilled-non resident', 'Unskilled-resident',
                              'skilled employee/official','management/self-employed/highly qualified/officer')

levels(credit$MALE_DIV)<- c('No', 'Yes')
levels(credit$MALE_SINGLE)<- c('No', 'Yes')
levels(credit$MALE_MAR_or_WID)<- c('No', 'Yes')
levels(credit$CO_APPLICANT)<- c('No', 'Yes')

# results
with(credit,table(RESPONSE))

# data structure
summary(credit)

# var. Education: -1, we will change that in 1 --> there is a mistake of sign
# var. AGE: age maximum = 125yrs, change this observation to 75yrs according to the prof
indice=which(credit$EDUCATION==-1)
credit$EDUCATION[37]=1
credit$EDUCATION=droplevels(credit$EDUCATION)
indice=which.max(credit$AGE)
credit$AGE[537]=75

# var. GUARANTOR: changing ang removing the useless level "2"
credit$GUARANTOR[credit$GUARANTOR==2]<-0 
credit$GUARANTOR=credit$GUARANTOR[credit$GUARANTOR!=2]
credit$GUARANTOR=droplevels(credit$GUARANTOR)

# var. PRESENT_RESIDENT: correcting the levels from 1:4 --> 0:3
levels(credit$PRESENT_RESIDENT)=c(0,1,2,3,4)
credit$PRESENT_RESIDENT=droplevels(credit$PRESENT_RESIDENT)

#------------------------------------------DATA ANALYSIS---------------------------------------------------

attach(credit)

# response attribute
res <- rbind(table(RESPONSE), paste(round(prop.table(table(RESPONSE)) * 100, 1), "%"))
rownames(res) <- c("sample size", "proportion")
print(res)


ggplot(credit, aes(x=RESPONSE))+geom_bar(aes(fill = RESPONSE))
# unbalanced response variable.

# exploratory variables
# CREDIT_HISTORY
his <- cbind(table(HISTORY,RESPONSE), paste(round(prop.table(table(HISTORY))*100,1),"%"))
colnames(his)[3] <- "proportion"
print(his)


g.his<-ggplot(credit, aes(HISTORY, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")

# CHEcking account
t.chk<-table(CHK_ACCT, RESPONSE)
chk <- cbind(t.chk, paste(round(prop.table(table(CHK_ACCT))*100,1),"%"))
colnames(chk)[3] <- "proportion"
print(chk)

g.chk<-ggplot(credit, aes(CHK_ACCT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")

# SAvings
table(SAV_ACCT, RESPONSE)
sav <- cbind(table(SAV_ACCT, RESPONSE), paste(round(prop.table(table(SAV_ACCT))*100,1),"%"))
colnames(sav)[3] <- "proportion"
print(sav)

g.sav<-ggplot(credit, aes(SAV_ACCT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")

grid.arrange(g.chk, g.his, g.sav)

#PURPOSE OF CREDIT
purpose <- cbind(table(NEW_CAR),table(USED_CAR),table(FURNITURE),
                 table(RADIOTV),table(EDUCATION),table(RETRAINING))
colnames(purpose) <- c("New car", "Used car", "Furniture/Equipment", "Radio/TV",
                       "Education", "Retraining")
rownames(purpose) <- c("No", "Yes")
print(purpose)


#JOB
table(JOB, RESPONSE)
job <- cbind(table(JOB, RESPONSE), paste(round(prop.table(table(JOB))*100,1),"%"))
colnames(job)[3] <- "proportion"
print(job)

g.job<-ggplot(credit, aes(JOB, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")

#EMPLOYMENT
table(EMPLOYMENT, RESPONSE)
emp <- cbind(table(EMPLOYMENT, RESPONSE), paste(round(prop.table(table(EMPLOYMENT))*100,1),"%"))
colnames(emp)[3] <- "proportion"
print(emp)

g.emp<-ggplot(credit, aes(EMPLOYMENT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")

grid.arrange(g.job, g.emp)

#FOREIGN
t.for<- table(FOREIGN, RESPONSE)
job <- cbind(t.for, paste(round(prop.table(table(FOREIGN))*100,1),"%"))
colnames(job)[3] <- "proportion"
print(job)

ggplot(credit, aes(FOREIGN, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")
#MALE_DIV
ggd<-ggplot(credit, aes(MALE_DIV, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")+coord_flip()

ggs<-ggplot(credit, aes(MALE_SINGLE, ..count..)) + 
  geom_bar(aes(fill = MALE_SINGLE), position = "dodge")+coord_flip()

ggm<-ggplot(credit, aes(MALE_MAR_or_WID, ..count..)) + 
  geom_bar(aes(fill = MALE_MAR_or_WID), position = "dodge")+coord_flip()

grid.arrange(ggd,ggs,ggm)

male <- cbind(table(MALE_DIV),table(MALE_SINGLE),table(MALE_MAR_or_WID) )

colnames(male) <- c("Male and divorced", "Male and single", "Male and married or widower")
rownames(male) <- c("No", "Yes")
print(male)

#REAL_ESTATE
t.real<-table(REAL_ESTATE, RESPONSE)
real <- cbind(t.real, paste(round(prop.table(table(REAL_ESTATE))*100,1),"%"))
colnames(real)[3] <- "proportion"
print(real)

ggplot(credit, aes(REAL_ESTATE, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")

par(mfrow=c(2,3))
# AMOUNT
hist(AMOUNT, col="#6699CC")
#DURATION
hist(DURATION, col="#6699CC")
# AGE
hist(AGE, col="#6699CC")
# INSTALL_RATE
hist(INSTALL_RATE, col="#6699CC")
# NUM_CREDITS
hist(NUM_CREDITS, col="#6699CC")
# NUM_DEPENDENTS
hist(NUM_DEPENDENTS, col="#6699CC")

ggplot(credit, aes(as.factor(INSTALL_RATE), ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")

ggplot(credit, aes(as.factor(NUM_CREDITS), ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")

#correlation matrix
exp.var=c(2,10,13,22,26,28)
round(cor(credit[,exp.var]),3)
ggpairs(credit[,exp.var]) 


lblue <- "#6699CC"
par(mfrow = c(1, 3))
boxplot(DURATION ~ RESPONSE, data = credit, xlab = "duration of credit in months", notch = T, 
        varwidth = T, col = lblue)
boxplot(AMOUNT ~ RESPONSE, data = credit, xlab = "credit amount", varwidth = T, col = lblue)
boxplot(AGE ~ RESPONSE, data = credit, xlab = "age in years", varwidth = T, col = lblue)



# no need function to normalize because preprocess in our case do it ( and skip the categorical)

# ---------------------REDUCED MODEL ANALYSIS-----------------------------
credit_glm <- step(glm(RESPONSE ~ ., data = credit, family = binomial),
                   direction = 'both')
cols<-c(all.vars(formula(credit_glm[-2])), 'RESPONSE')
# remove the first response name
cols<-cols[-1]
credit_red = credit[cols]
# ---------------------Preparation for analysis-----------------------------
install.packages(c("ISLR","neuralnet","rpart.plot","DMwR","MLmetrics","pROC"))
library(pROC)
library(MLmetrics)
library(DMwR)
library(rpart.plot)
library(neuralnet)
library(ISLR)
library(caret)
# split data in training and test 
set.seed(20)
n <- dim(credit) 
mySample <- 1:n 

# Data partition: 30% for test and 70% of data for training
test <- sample(mySample, n*0.3)
train <- mySample[-test]
data.train <-credit[train,]
data.test <- credit[test,]

# Same partition for our reduced dataset
data.train.red<-credit_red[train,]
data.test.red<-credit_red[test,]

# ----------------Tuning parameters-------------------------------

# Tuning control with 10-fold (the best kfold according to our course) cv repeated 3 times 
# TwoClassSummary- to find the best parameter according to sensi, spec and ROC curve
# ClassProb = compute the class proba
# Sampling = smote because our response variable is imbalance ( there exists other possibilities but we decided to choose smote)
# preProcess = for normalized 
tcontrol <- trainControl(method="repeatedcv", number=10, repeats=3, sampling = "up",classProbs = TRUE,
                         summaryFunction = twoClassSummary)
tcrtl<-trainControl(classProbs = TRUE,
                    summaryFunction = twoClassSummary)
# --------Models with partameter by default---------------------------

# -----------------------log reg---------------------------------------
set.seed(20)
# No tuning parameters for logistic regression
# full data
lr.fit<-train(RESPONSE~., data=data.train, method="glm", family="binomial",trControl=tcrtl)
summary(lr.fit)
print(lr.fit)

# prediction with all predictors
lr.pred <- predict(lr.fit,newdata=data.test)
lr.pred 
cm.lr=confusionMatrix(lr.pred, data.test$RESPONSE )
cm.lr

# obtaining probabilities for data.test (predicted)
lr.probs <- predict(lr.fit, newdata=data.test, type="prob")
head(lr.probs)

#Calculate ROC curve

roc.lr <- roc(data.test$RESPONSE,lr.probs[,"Good"])
#plot the ROC curve
plot(roc.lr,col=c(1),main= "ROC CURVE")
auc(roc.lr)

# for the reduced data
lr.red.fit<-train(RESPONSE~., data=data.train.red, method="glm", family="binomial",trControl=tcrtl)
summary(lr.red.fit)
# prediction for reduced model
lr.red.pred <- predict(lr.red.fit,newdata=data.test.red)
lr.red.pred 
cm.red.lr=confusionMatrix(lr.red.pred, data.test.red$RESPONSE)
cm.red.lr

lr.red.probs<-predict(lr.red.fit, newdata=data.test.red, type="prob")
head(lr.red.probs)

#Calculate ROC curve
roc.red.lr <- roc(data.test.red$RESPONSE,lr.red.probs[,"Good"])
#plot the ROC curve
plot(roc.red.lr,col=c(1))
auc(roc.red.lr)

# reduced model thanks to important variable 
importance.lr<-varImp(lr.fit)
plot(importance.lr)
importance.lr

# for the reduced data
lr.imp.fit<-train(RESPONSE~CHK_ACCT+PRESENT_RESIDENT+SAV_ACCT+HISTORY+DURATION+EDUCATION+OTHER_INSTAL+INSTALL_RATE+MALE_SINGLE+NEW_CAR+GUARANTOR+AMOUNT+JOB+EMPLOYMENT, data=data.train, method="glm", family="binomial",trControl=tcrtl)
summary(lr.imp.fit)
# prediction for reduced model
lr.imp.pred <- predict(lr.imp.fit,newdata=data.test)
lr.imp.pred 
cm.imp.lr=confusionMatrix(lr.imp.pred, data.test$RESPONSE)
cm.imp.lr

lr.imp.probs<-predict(lr.imp.fit, newdata=data.test, type="prob")
head(lr.imp.probs)

#Calculate ROC curve
roc.imp.lr <- roc(data.test$RESPONSE,lr.imp.probs[,"Good"])
#plot the ROC curve
plot(roc.imp.lr,col=c(1))
auc(roc.imp.lr)

accu.lr=c(cm.lr$overall['Accuracy'],cm.red.lr$overall['Accuracy'],cm.imp.lr$overall['Accuracy'] )
kappa.lr=c(cm.lr$overall['Kappa'],cm.red.lr$overall['Kappa'],cm.imp.lr$overall['Kappa'] )
Sensi.lr=c(cm.lr$byClass['Sensitivity'],cm.red.lr$byClass['Sensitivity'],cm.imp.lr$byClass['Sensitivity'] )
Spe.lr=c(cm.lr$byClass['Specificity'],cm.red.lr$byClass['Specificity'],cm.imp.lr$byClass['Specificity'] )
auc.lr=c(auc(roc.lr),auc(roc.red.lr),auc(roc.imp.lr))

df.lr<-data.frame(round(accu.lr,4),round(kappa.lr,4),round(Sensi.lr,4),
                  round(Spe.lr,4), round(auc.lr,4))
row.names(df.lr)<-c("Full Model", "Reduced Model", "Reduced model by varImp")
colnames(df.lr)<-c("Accuracy", "Kappa", "Sensitivity","Specificty","AUC")
print(df.lr)
# here we can see that with the step function the accurancy of the model is higher as well as the AUC.
bestmodel.lr<-df.lr[2,]

# why use auc? https://pdfs.semanticscholar.org/ce8f/f40258c59ed9866bb0de7aadb79dbedb1702.pdf
# ----------------- classification tree--------------------------------
set.seed(20)
# full model
tree.fit <- train(RESPONSE~., data=data.train,method="rpart",trControl=tcrtl)
print(tree.fit)
tree.pred <- predict(tree.fit,newdata=data.test)
tree.pred
cm.tree=confusionMatrix(tree.pred, data.test$RESPONSE )
cm.tree
tree.probs<-predict(tree.fit, newdata=data.test, type="prob")
head(tree.probs)
#Calculate ROC curve
roc.tree <- roc(data.test$RESPONSE,tree.probs[,"Good"])
#plot the ROC curve
plot(roc.tree,col=c(2),main= "ROC CURVE")
auc(roc.tree)

# reduced model thanks to important variable 
importance.tree<-varImp(tree.fit)
plot(importance.tree)
importance.tree
# variables that are choosen 

tree.imp.fit <- train(RESPONSE~HISTORY+CHK_ACCT+AMOUNT+DURATION+SAV_ACCT+AGE+USED_CAR+NEW_CAR, data=data.train, method="rpart",trControl=tcrtl)
print(tree.imp.fit)
tree.imp.pred <- predict(tree.imp.fit,newdata=data.test)
tree.imp.pred
cm.imp.tree=confusionMatrix(tree.imp.pred, data.test$RESPONSE )
cm.imp.tree
tree.imp.probs<-predict(tree.imp.fit, newdata=data.test, type="prob")
head(tree.imp.probs)

#Calculate ROC curve
roc.imp.tree <- roc(data.test$RESPONSE,tree.imp.probs[,"Good"])
#plot the ROC curve
plot(roc.imp.tree,col=c(2), main= "ROC CURVE")
auc(roc.imp.tree)

# reduced model using the new dataset (step function for lr method)
tree.red.fit <- train(RESPONSE~., data=data.train.red,method="rpart",trControl=tcrtl)
print(tree.red.fit)
tree.pred.red <- predict(tree.red.fit,newdata=data.test.red)
tree.pred.red
cm.tree.red=confusionMatrix(tree.pred.red, data.test.red$RESPONSE )
cm.tree.red
tree.probs.red<-predict(tree.red.fit, newdata=data.test.red, type="prob")
head(tree.probs.red)
#Calculate ROC curve
roc.tree.red <- roc(data.test.red$RESPONSE,tree.probs.red[,"Good"])
#plot the ROC curve
plot(roc.tree.red,col=c(2),main= "ROC CURVE")
auc(roc.tree.red)
# here for the rpart (classification tree) : one tuning parameter that is cp (complexity parameter)
# complexity param=0.02392344 and 0.01196172 for full model 
# cp : 0.024 for models

grid.tree <- expand.grid(cp= seq(0.01,0.04,0.001))

# full model 
tree.fit.grid <- train(RESPONSE~., data=data.train, trControl=tcontrol, method="rpart", tuneGrid=grid.tree,preProcess = c("center", "scale"))
print(tree.fit.grid)
tree.pred.grid <- predict(tree.fit.grid,newdata=data.test)
tree.pred.grid
cm.tree.grid=confusionMatrix(tree.pred.grid, data.test$RESPONSE)
tree.probs.grid<-predict(tree.fit.grid, newdata=data.test, type="prob")
head(tree.probs.grid)

#Calculate ROC curve
roc.tree.grid <- roc(data.test$RESPONSE,tree.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.tree.grid,col=c(2))
auc(roc.tree.grid)

# Important variables with grid

tree.imp.fit.grid <- train(RESPONSE~HISTORY+CHK_ACCT+AMOUNT+DURATION+SAV_ACCT+AGE+USED_CAR+NEW_CAR, data=data.train, trControl=tcontrol, method="rpart",tuneGrid=grid.tree,preProcess = c("center", "scale") )
print(tree.imp.fit.grid)
tree.imp.pred.grid <- predict(tree.imp.fit.grid,newdata=data.test)
tree.imp.pred.grid
cm.imp.tree.grid=confusionMatrix(tree.imp.pred.grid, data.test$RESPONSE )
cm.imp.tree.grid
tree.imp.probs.grid<-predict(tree.imp.fit.grid, newdata=data.test, type="prob")
head(tree.imp.probs.grid)

#Calculate ROC curve
roc.imp.tree.grid <- roc(data.test$RESPONSE,tree.imp.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.imp.tree,col=c(2),main="ROC Curve")
auc(roc.imp.tree.grid)

# reduced model using the new dataset (step function for lr method)
tree.red.fit.grid <- train(RESPONSE~., data=data.train.red,trControl=tcontrol,method="rpart",tuneGrid=grid.tree,preProcess = c("center", "scale") )
print(tree.red.fit.grid)
tree.pred.red.grid <- predict(tree.red.fit.grid,newdata=data.test.red)
tree.pred.red.grid
cm.tree.red.grid=confusionMatrix(tree.pred.red.grid, data.test.red$RESPONSE )
cm.tree.red.grid
tree.probs.red.grid<-predict(tree.red.fit.grid, newdata=data.test.red, type="prob")
head(tree.probs.red.grid)
#Calculate ROC curve
roc.tree.red.grid <- roc(data.test.red$RESPONSE,tree.probs.red[,"Good"])
#plot the ROC curve
plot(roc.tree.red.grid,col=c(2),main= "ROC CURVE")
auc(roc.tree.red.grid)

accu.tree=c(cm.tree.grid$overall['Accuracy'],
            cm.imp.tree.grid$overall['Accuracy'],cm.tree.red.grid$overall['Accuracy'] )
kappa.tree=c(cm.tree.grid$overall['Kappa'],cm.imp.tree.grid$overall['Kappa'],
             cm.tree.red.grid$overall['Kappa'] )
Sensi.tree=c(cm.tree.grid$byClass['Sensitivity'],cm.imp.tree.grid$byClass['Sensitivity'],
             cm.tree.red.grid$byClass['Sensitivity'])
Spe.tree=c(cm.tree.grid$byClass['Specificity'],cm.imp.tree.grid$byClass['Specificity'],
           cm.tree.red.grid$byClass['Specificity'])
auc.tree=c(auc(roc.tree.grid),auc(roc.imp.tree.grid),
           auc(roc.tree.red.grid))

df.tree<-data.frame(round(accu.tree,4),round(kappa.tree,4),
                    round(Sensi.tree,4),round(Spe.tree,4), round(auc.tree,4))
row.names(df.tree)<-c("Full Model tuned", "Reduced imp Model tuned", "Reduced Model tuned")
colnames(df.tree)<- c("Accuracy", "Kappa", "Sensitivity","Specificty","AUC")
grid.table(df.tree)

bestmodel.dt<-df.tree[3,]
# --------------------SVM Kernel------------------------

set.seed(22)
# full model
svm.fit <- train(RESPONSE~., data=data.train, method="svmRadial",trControl=tcrtl)
print(svm.fit)
svm.pred <- predict(svm.fit,newdata=data.test)
svm.pred
cm.svm=confusionMatrix(svm.pred, data.test$RESPONSE )
cm.svm

svm.probs<-predict(svm.fit, newdata=data.test, type="prob")
head(svm.probs)
#Calculate ROC curve
roc.svm <- roc(data.test$RESPONSE,svm.probs[,"Good"])
#plot the ROC curve
plot(roc.svm,col=c(2))
auc(roc.svm)

# reduced model thanks to important variable 
importance.svm<-varImp(svm.fit)
plot(importance.svm)
importance.svm

# variables are choosen : 100% until 26% 

svm.imp.fit <- train(RESPONSE~CHK_ACCT+DURATION+HISTORY+SAV_ACCT+AGE+EMPLOYMENT+OWN_RES+NUM_CREDITS+USED_CAR+JOB+TELEPHONE+RADIOTV+MALE_SINGLE+NEW_CAR+AMOUNT+RENT++REAL_ESTATE+PROP_UNKN_NONE+OTHER_INSTAL+INSTALL_RATE, data=data.train, method="svmRadial",trControl=tcrtl )
print(svm.imp.fit)
svm.imp.pred <- predict(svm.imp.fit,newdata=data.test)
svm.imp.pred
cm.imp.svm=confusionMatrix(svm.imp.pred, data.test$RESPONSE )
cm.imp.svm
svm.imp.probs<-predict(svm.imp.fit, newdata=data.test, type="prob")
head(svm.imp.probs)

#Calculate ROC curve
roc.imp.svm <- roc(data.test$RESPONSE,svm.imp.probs[,"Good"])
#plot the ROC curve
plot(roc.imp.svm,col=c(2))
auc(roc.imp.svm)

# reduced model with step
svm.red.fit <- train(RESPONSE~., data=data.train.red,method="svmRadial",trControl=tcrtl)
print(svm.red.fit)
svm.pred.red <- predict(svm.red.fit,newdata=data.test.red)
svm.pred.red
cm.svm.red=confusionMatrix(svm.pred.red, data.test.red$RESPONSE )
cm.svm.red
svm.probs.red<-predict(svm.red.fit, newdata=data.test.red, type="prob")
head(svm.probs.red)
#Calculate ROC curve
roc.svm.red <- roc(data.test.red$RESPONSE,svm.probs.red[,"Good"])
#plot the ROC curve
plot(roc.svm.red,col=c(2),main= "ROC CURVE")
auc(roc.svm.red)

# tuning the parameters now: sigma and the cost: for the full model we have by default chosen sigma = 0.03204417 and C = 0.25
grid.rad <- expand.grid(sigma = c(0,0.013,0.02, 0.025, 0.04,
                                  0.05, 0.06, 0.09, 0.1, 0.25, 0.5),
                        C = c(0,0.01,0.015,0.03, 0.05, 0.1, 0.25, 0.5, 0.75,
                              1, 1.5, 2,5))

# full model
svm.fit.grid <- train(RESPONSE~., data=data.train, trControl=tcontrol, method="svmRadial",tuneGrid=grid.rad,preProcess = c("center", "scale"))
print(svm.fit.grid)
svm.pred.grid <- predict(svm.fit.grid,newdata=data.test)
svm.pred.grid
cm.svm.grid=confusionMatrix(svm.pred.grid, data.test$RESPONSE )
cm.svm.grid
svm.probs.grid<-predict(svm.fit.grid, newdata=data.test, type="prob")
head(svm.probs.grid)
#Calculate ROC curve
roc.svm.grid <- roc(data.test$RESPONSE,svm.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.svm.grid,col=c(2))
auc(roc.svm.grid)

# reduced model 

svm.imp.fit.grid <- train(RESPONSE~HISTORY+CHK_ACCT+DURATION+SAV_ACCT+AGE+EMPLOYMENT+REAL_ESTATE+OWN_RES+OTHER_INSTAL, data=data.train, trControl=tcontrol, method="svmRadial",preProcess = c("center", "scale"), tuneGrid=grid.rad)
print(svm.imp.fit.grid)
svm.imp.pred.grid <- predict(svm.imp.fit.grid,newdata=data.test)
svm.imp.pred.grid
cm.imp.svm.grid=confusionMatrix(svm.imp.pred.grid, data.test$RESPONSE )
cm.imp.svm.grid
svm.imp.probs.grid<-predict(svm.imp.fit.grid, newdata=data.test, type="prob")
head(svm.imp.probs.grid)

#Calculate ROC curve
roc.imp.svm.grid <- roc(data.test$RESPONSE,svm.imp.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.imp.svm.grid,col=c(2))
auc(roc.imp.svm.grid)

# reduced model using the new dataset (step function for lr method)
svm.red.fit.grid <- train(RESPONSE~., data=data.train.red,trControl=tcontrol,method="svmRadial",tuneGrid=grid.rad,preProcess = c("center", "scale") )
print(svm.red.fit.grid)
svm.pred.red.grid <- predict(svm.red.fit.grid,newdata=data.test.red)
svm.pred.red.grid
cm.svm.red.grid=confusionMatrix(svm.pred.red.grid, data.test.red$RESPONSE )
cm.svm.red.grid
svm.probs.red.grid<-predict(svm.red.fit.grid, newdata=data.test.red, type="prob")
head(svm.probs.red.grid)
#Calculate ROC curve
roc.svm.red.grid <- roc(data.test.red$RESPONSE,svm.probs.red[,"Good"])
#plot the ROC curve
plot(roc.svm.red.grid,col=c(2),main= "ROC CURVE")
auc(roc.svm.red.grid)

accu.svm=c(cm.svm$overall['Accuracy'],cm.imp.svm$overall['Accuracy'],cm.svm.grid$overall['Accuracy'],cm.imp.svm.grid$overall['Accuracy'],cm.svm.red$overall['Accuracy'],cm.svm.red.grid$overall['Accuracy'])
kappa.svm=c(cm.svm$overall['Kappa'],cm.imp.svm$overall['Kappa'],cm.svm.grid$overall['Kappa'],cm.imp.svm.grid$overall['Kappa'],cm.svm.red$overall['Kappa'],cm.svm.red.grid$overall['Kappa'] )
Sensi.svm=c(cm.svm$byClass['Sensitivity'],cm.imp.svm$byClass['Sensitivity'],cm.svm.grid$byClass['Sensitivity'],cm.imp.svm.grid$byClass['Sensitivity'],cm.tree.red$byClass['Sensitivity'],cm.tree.red.grid$byClass['Sensitivity'] )
Spe.svm=c(cm.svm$byClass['Specificity'],cm.imp.svm$byClass['Specificity'],cm.svm.grid$byClass['Specificity'],cm.imp.svm.grid$byClass['Specificity'],cm.svm.red$byClass['Specificity'],cm.svm.red.grid$byClass['Specificity'] )
auc.svm=c(auc(roc.svm),auc(roc.imp.svm),auc(roc.svm.grid),auc(roc.imp.svm.grid),auc(roc.svm.red),auc(roc.svm.red.grid))

df.svm<-data.frame(accu.svm,kappa.svm,Sensi.svm,Spe.svm,auc.svm)
row.names(df.svm)<-c("Full Model", "Reduced imp Model","Full Model tuned", "Reduced Model imp tuned","Reduced model", "Reduced Model tuned")
df.svm
bestmodel.svm<-df.svm[2,]
# -----------------------------Random forest-----------------------------------------
set.seed(20)
# full model
rf.fit <- train(RESPONSE~., data=data.train, method="rf",importance = TRUE,trControl=tcrtl)
print(rf.fit)
rf.pred <- predict(rf.fit,newdata=data.test)
rf.pred
cm.rf=confusionMatrix(rf.pred, data.test$RESPONSE )
cm.rf
rf.probs<-predict(rf.fit, newdata=data.test, type="prob")
head(rf.probs)
#Calculate ROC curve
roc.rf <- roc(data.test$RESPONSE,rf.probs[,"Good"])
#plot the ROC curve
plot(roc.rf,col=c(2))
auc(roc.rf)

# reduced model thanks to important variable 
importance.rf<-varImp(rf.fit)
plot(importance.rf)
importance.rf

# variables are choosen 

rf.imp.fit <- train(RESPONSE~CHK_ACCT+HISTORY+AMOUNT+FURNITURE+DURATION+SAV_ACCT+AGE+EMPLOYMENT+REAL_ESTATE+OTHER_INSTAL+RADIOTV+RENT+USED_CAR+OWN_RES+GUARANTOR+NEW_CAR+PRESENT_RESIDENT+EMPLOYMENT, data=data.train, method="rf",importance = TRUE,trControl=tcrtl)
print(rf.imp.fit)
rf.imp.pred <- predict(rf.imp.fit,newdata=data.test)
rf.imp.pred
cm.imp.rf=confusionMatrix(rf.imp.pred, data.test$RESPONSE )
cm.imp.rf
rf.imp.probs<-predict(rf.imp.fit, newdata=data.test, type="prob")
head(rf.imp.probs)

#Calculate ROC curve
roc.imp.rf <- roc(data.test$RESPONSE,rf.imp.probs[,"Good"])
#plot the ROC curve
plot(roc.imp.rf,col=c(2))
auc(roc.imp.rf)

# reduced model with step
rf.red.fit <- train(RESPONSE~., data=data.train.red,method="rf",importance = TRUE,trControl=tcrtl)
print(rf.red.fit)
rf.pred.red <- predict(rf.red.fit,newdata=data.test.red)
rf.pred.red
cm.rf.red=confusionMatrix(rf.pred.red, data.test.red$RESPONSE )
cm.rf.red
rf.probs.red<-predict(rf.red.fit, newdata=data.test.red, type="prob")
head(rf.probs.red)
#Calculate ROC curve
roc.rf.red <- roc(data.test.red$RESPONSE,rf.probs.red[,"Good"])
#plot the ROC curve
plot(roc.rf.red,col=c(2),main= "ROC CURVE")
auc(roc.rf.red)

# tuning the parameters now: mtry: the mtry chosen by default is 2 so we will tune it
tunegrid.rf= expand.grid(.mtry=(1:24)) 

# full model
rf.fit.grid <- train(RESPONSE~., data=data.train, trControl=tcontrol, method="rf",preProcess = c("center", "scale"),importance = TRUE,tuneGrid=tunegrid.rf)
print(rf.fit.grid)
rf.pred.grid <- predict(rf.fit.grid,newdata=data.test)
rf.pred.grid
cm.rf.grid=confusionMatrix(rf.pred.grid, data.test$RESPONSE )
cm.rf.grid
rf.probs.grid<-predict(rf.fit.grid, newdata=data.test, type="prob")
head(rf.probs.grid)
#Calculate ROC curve
roc.rf.grid <- roc(data.test$RESPONSE,rf.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.rf.grid,col=c(2))
auc(roc.rf.grid)

# reduced model and tuning ICI

rf.imp.fit.grid <- train(RESPONSE~CHK_ACCT+HISTORY+AMOUNT+FURNITURE+DURATION+SAV_ACCT+AGE+EMPLOYMENT+REAL_ESTATE+OTHER_INSTAL+RADIOTV+RENT+USED_CAR+OWN_RES+GUARANTOR+NEW_CAR+PRESENT_RESIDENT+EMPLOYMENT, data=data.train, trControl=tcontrol, method="rf",preProcess = c("center", "scale"),importance = TRUE,tuneGrid=tunegrid.rf)
print(rf.imp.fit.grid)
rf.imp.pred.grid <- predict(rf.imp.fit.grid,newdata=data.test)
rf.imp.pred.grid
cm.imp.rf.grid=confusionMatrix(rf.imp.pred.grid, data.test$RESPONSE )
cm.imp.rf.grid
rf.imp.probs.grid<-predict(rf.imp.fit.grid, newdata=data.test, type="prob")
head(rf.imp.probs.grid)

#Calculate ROC curve
roc.imp.rf.grid <- roc(data.test$RESPONSE,rf.imp.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.imp.rf.grid,col=c(2))
auc(roc.imp.rf.grid)

# tuning reduced model with step 
# reduced model using the new dataset (step function for lr method)
rf.red.fit.grid <- train(RESPONSE~., data=data.train.red,trControl=tcontrol,method="rf",importance = TRUE,tuneGrid=tunegrid.rf,preProcess = c("center", "scale") )
print(rf.red.fit.grid)
rf.pred.red.grid <- predict(rf.red.fit.grid,newdata=data.test.red)
rf.pred.red.grid
cm.rf.red.grid=confusionMatrix(rf.pred.red.grid, data.test.red$RESPONSE )
cm.rf.red.grid
rf.probs.red.grid<-predict(rf.red.fit.grid, newdata=data.test.red, type="prob")
head(rf.probs.red.grid)
#Calculate ROC curve
roc.rf.red.grid <- roc(data.test.red$RESPONSE,rf.probs.red[,"Good"])
#plot the ROC curve
plot(roc.rf.red.grid,col=c(2),main= "ROC CURVE")
auc(roc.rf.red.grid)


# summary
accu.rf=c(cm.rf$overall['Accuracy'],cm.imp.rf$overall['Accuracy'],cm.rf.grid$overall['Accuracy'],cm.imp.rf.grid$overall['Accuracy'],cm.rf.red$overall['Accuracy'],cm.rf.red.grid$overall['Accuracy'])
kappa.rf=c(cm.rf$overall['Kappa'],cm.imp.rf$overall['Kappa'],cm.rf.grid$overall['Kappa'],cm.imp.rf.grid$overall['Kappa'],cm.rf.red$overall['Kappa'],cm.rf.red.grid$overall['Kappa'] )
Sensi.rf=c(cm.rf$byClass['Sensitivity'],cm.imp.rf$byClass['Sensitivity'],cm.rf.grid$byClass['Sensitivity'],cm.imp.rf.grid$byClass['Sensitivity'],cm.rf.red$byClass['Sensitivity'],cm.rf.red.grid$byClass['Sensitivity'] )
Spe.rf=c(cm.rf$byClass['Specificity'],cm.imp.rf$byClass['Specificity'],cm.rf.grid$byClass['Specificity'],cm.imp.rf.grid$byClass['Specificity'],cm.rf.red$byClass['Specificity'],cm.rf.red.grid$byClass['Specificity'] )
auc.rf=c(auc(roc.rf),auc(roc.imp.rf),auc(roc.rf.grid),auc(roc.imp.rf.grid),auc(roc.rf.red),auc(roc.rf.red.grid))

df.rf<-data.frame(accu.rf,kappa.rf,Sensi.rf,Spe.rf,auc.rf)
row.names(df.rf)<-c("Full Model", "Reduced imp Model","Full Model tuned", "Reduced imp Model tuned","Reduced model", "Reduced Model tuned")
df.rf

bestmodel.rf<-df.rf[3,]
# ----------------------------KNN----------------------------------------------------------
# full model
set.seed(20)
knn.fit <- train(RESPONSE~., data=data.train, method="knn",trControl=tcrtl)
print(knn.fit)
knn.pred <- predict(knn.fit,newdata=data.test)
knn.pred
cm.knn=confusionMatrix(knn.pred, data.test$RESPONSE )
draw_confusion_matrix(cm.knn)

knn.probs<-predict(knn.fit, newdata=data.test, type="prob")
head(knn.probs)
#Calculate ROC curve
roc.knn <- roc(data.test$RESPONSE,knn.probs[,"Good"])
#plot the ROC curve
plot(roc.knn,col=c(2))
auc(roc.knn)

# reduced model thanks to important variable 
importance.knn<-varImp(knn.fit)
plot(importance.knn)
importance.knn

# variables are choosen : 100% until 25% 

knn.imp.fit <- train(RESPONSE~HISTORY+CHK_ACCT+DURATION+SAV_ACCT+AGE+EMPLOYMENT+REAL_ESTATE+OWN_RES+RENT+PROP_UNKN_NONE+INSTALL_RATE+OTHER_INSTAL, data=data.train, method="knn",trControl=tcrtl)
print(knn.imp.fit)
knn.imp.pred <- predict(knn.imp.fit,newdata=data.test)
knn.imp.pred
cm.imp.knn=confusionMatrix(knn.imp.pred, data.test$RESPONSE )
cm.imp.knn
knn.imp.probs<-predict(knn.imp.fit, newdata=data.test, type="prob")
head(knn.imp.probs)

#Calculate ROC curve
roc.imp.knn <- roc(data.test$RESPONSE,knn.imp.probs[,"Good"])
#plot the ROC curve
plot(roc.imp.knn,col=c(2))
auc(roc.imp.knn)

# reduced model with step
knn.red.fit <- train(RESPONSE~., data=data.train.red,method="knn",trControl=tcrtl)
print(knn.red.fit)
knn.pred.red <- predict(knn.red.fit,newdata=data.test.red)
knn.pred.red
cm.knn.red=confusionMatrix(knn.pred.red, data.test.red$RESPONSE )
cm.knn.red
knn.probs.red<-predict(knn.red.fit, newdata=data.test.red, type="prob")
head(knn.probs.red)
#Calculate ROC curve
roc.knn.red <- roc(data.test.red$RESPONSE,knn.probs.red[,"Good"])
#plot the ROC curve
plot(roc.knn.red,col=c(2),main= "ROC CURVE")
auc(roc.knn.red)
# Now we will tune the model : change the parameter k

tunegrid.knn= expand.grid(k=(1:15)) 

knn.fit.grid <- train(RESPONSE~., data=data.train, trControl=tcontrol, method="knn",preProcess = c("center", "scale"),tuneGrid=tunegrid.knn )
print(knn.fit.grid)
knn.pred.grid <- predict(knn.fit.grid,newdata=data.test)
knn.pred.grid
cm.knn.grid=confusionMatrix(knn.pred.grid, data.test$RESPONSE )
cm.knn.grid
knn.probs.grid<-predict(knn.fit.grid, newdata=data.test, type="prob")
head(knn.probs.grid)
#Calculate ROC curve
roc.knn.grid <- roc(data.test$RESPONSE,knn.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.knn.grid,col=c(2))
auc(roc.knn.grid)

# reduced model with tuning
knn.imp.fit.grid <- train(RESPONSE~HISTORY+CHK_ACCT+DURATION+SAV_ACCT+AGE+EMPLOYMENT+REAL_ESTATE+OWN_RES+RENT+PROP_UNKN_NONE+INSTALL_RATE+OTHER_INSTAL, data=data.train, trControl=tcontrol, method="knn",preProcess = c("center", "scale"),tuneGrid=tunegrid.knn)
print(knn.imp.fit.grid)
knn.imp.pred.grid <- predict(knn.imp.fit.grid,newdata=data.test)
knn.imp.pred.grid
cm.imp.knn.grid=confusionMatrix(knn.imp.pred.grid, data.test$RESPONSE )
cm.imp.knn.grid
knn.imp.probs.grid<-predict(knn.imp.fit.grid, newdata=data.test, type="prob")
head(knn.imp.probs.grid)

#Calculate ROC curve
roc.imp.knn.grid <- roc(data.test$RESPONSE,knn.imp.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.imp.knn.grid,col=c(2))
auc(roc.imp.knn.grid)

# reduced model using the new dataset (step function for lr method)
knn.red.fit.grid <- train(RESPONSE~., data=data.train.red,trControl=tcontrol,method="knn",tuneGrid=tunegrid.knn,preProcess = c("center", "scale") )
print(knn.red.fit.grid)
knn.pred.red.grid <- predict(knn.red.fit.grid,newdata=data.test.red)
knn.pred.red.grid
cm.knn.red.grid=confusionMatrix(knn.pred.red.grid, data.test.red$RESPONSE )
cm.knn.red.grid
knn.probs.red.grid<-predict(knn.red.fit.grid, newdata=data.test.red, type="prob")
head(knn.probs.red.grid)
#Calculate ROC curve
roc.knn.red.grid <- roc(data.test.red$RESPONSE,knn.probs.red[,"Good"])
#plot the ROC curve
plot(roc.knn.red.grid,col=c(2),main= "ROC CURVE")
auc(roc.knn.red.grid)
# summary
accu.knn=c(cm.knn$overall['Accuracy'],cm.imp.knn$overall['Accuracy'],cm.knn.grid$overall['Accuracy'],cm.imp.knn.grid$overall['Accuracy'],cm.knn.red$overall['Accuracy'],cm.knn.red.grid$overall['Accuracy'])
kappa.knn=c(cm.knn$overall['Kappa'],cm.imp.knn$overall['Kappa'],cm.knn.grid$overall['Kappa'],cm.imp.knn.grid$overall['Kappa'],cm.knn.red$overall['Kappa'],cm.knn.red.grid$overall['Kappa'])
Sensi.knn=c(cm.knn$byClass['Sensitivity'],cm.imp.knn$byClass['Sensitivity'],cm.knn.grid$byClass['Sensitivity'],cm.imp.knn.grid$byClass['Sensitivity'],cm.knn.red$byClass['Sensitivity'],cm.knn.red.grid$byClass['Sensitivity'] )
Spe.knn=c(cm.knn$byClass['Specificity'],cm.imp.knn$byClass['Specificity'],cm.knn.grid$byClass['Specificity'],cm.imp.knn.grid$byClass['Specificity'], cm.knn.red$byClass['Specificity'],cm.knn.red.grid$byClass['Specificity'])
auc.knn=c(auc(roc.knn),auc(roc.imp.knn),auc(roc.knn.grid),auc(roc.imp.knn.grid),auc(roc.knn.red),auc(roc.knn.red.grid))

df.knn<-data.frame(accu.knn,kappa.knn,Sensi.knn,Spe.knn,auc.knn)
row.names(df.knn)<-c("Full Model", "Reduced imp Model","Full Model tuned", "Reduced Model imp tuned", "Red model", "red model tuned")
df.knn

bestmodel.knn<-df.knn[3,]
# -----------------------------Neutral Network----------------------------------
# full model
set.seed(20)
nn.fit <- train(RESPONSE~., data=data.train, method="nnet",trControl=tcrtl)
print(nn.fit)
nn.pred <- predict(nn.fit,newdata=data.test)
nn.pred
cm.nn=confusionMatrix(nn.pred, data.test$RESPONSE )
cm.nn
draw_confusion_matrix(cm.nn)
nn.probs<-predict(nn.fit, newdata=data.test, type="prob")
head(nn.probs)
#Calculate ROC curve
roc.nn <- roc(data.test$RESPONSE,nn.probs[,"Good"])
#plot the ROC curve
plot(roc.nn,col=c(2))
auc(roc.nn)

# reduced model thanks to important variable 
importance.nn<-varImp(nn.fit)
plot(importance.nn)
importance.nn

# reduced model with step
nn.red.fit <- train(RESPONSE~., data=data.train.red,method="nnet",trControl=tcrtl)
print(nn.red.fit)
nn.pred.red <- predict(nn.red.fit,newdata=data.test.red)
nn.pred.red
cm.nn.red=confusionMatrix(nn.pred.red, data.test.red$RESPONSE )
cm.nn.red
nn.probs.red<-predict(nn.red.fit, newdata=data.test.red, type="prob")
head(nn.probs.red)
#Calculate ROC curve
roc.nn.red <- roc(data.test.red$RESPONSE,nn.probs.red[,"Good"])
#plot the ROC curve
plot(roc.nn.red,col=c(2),main= "ROC CURVE")
auc(roc.nn.red)

# variables are choosen : 100% until 50% 

nn.imp.fit <- train(RESPONSE~AMOUNT+HISTORY+NEW_CAR+RENT+PRESENT_RESIDENT+CHK_ACCT+RADIOTV+JOB+REAL_ESTATE+EDUCATION+SAV_ACCT+PROP_UNKN_NONE+RENT, data=data.train,method="nnet",trControl=tcrtl)
print(nn.imp.fit)
nn.imp.pred <- predict(nn.imp.fit,newdata=data.test)
nn.imp.pred
cm.imp.nn=confusionMatrix(nn.imp.pred, data.test$RESPONSE )
cm.imp.nn
nn.imp.probs<-predict(nn.imp.fit, newdata=data.test, type="prob")
head(nn.imp.probs)

#Calculate ROC curve
roc.imp.nn <- roc(data.test$RESPONSE,nn.imp.probs[,"Good"])
#plot the ROC curve
plot(roc.imp.nn,col=c(2))
auc(roc.imp.nn)

# Now we will tune the model : change the parameter size: 1,decay:0,1

tunegrid.nn= expand.grid(size=c(0,1,2,3,4,5,6,7,8),decay=c(0.01,0.05,0.1,0.15,0.2,0.25,0.3))

nn.fit.grid <- train(RESPONSE~., data=data.train, trControl=tcontrol, method="nnet",preProcess = c("center", "scale"),tuneGrid=tunegrid.nn )
print(nn.fit.grid)
nn.pred.grid <- predict(nn.fit.grid,newdata=data.test)
nn.pred.grid
cm.nn.grid=confusionMatrix(nn.pred.grid, data.test$RESPONSE )
cm.nn.grid
nn.probs.grid<-predict(nn.fit.grid, newdata=data.test, type="prob")
head(nn.probs.grid)
#Calculate ROC curve
roc.nn.grid <- roc(data.test$RESPONSE,nn.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.nn.grid,col=c(2))
auc(roc.nn.grid)

# reduced model with tuning
nn.imp.fit.grid <- train(RESPONSE~HISTORY+CHK_ACCT+DURATION+SAV_ACCT+AGE+EMPLOYMENT+REAL_ESTATE+OWN_RES+OTHER_INSTAL, data=data.train, trControl=tcontrol, method="nnet",preProcess = c("center", "scale"),tuneGrid=tunegrid.nn)
print(nn.imp.fit.grid)
nn.imp.pred.grid <- predict(nn.imp.fit.grid,newdata=data.test)
nn.imp.pred.grid
cm.imp.nn.grid=confusionMatrix(nn.imp.pred.grid, data.test$RESPONSE )
cm.imp.nn.grid
nn.imp.probs.grid<-predict(nn.imp.fit.grid, newdata=data.test, type="prob")
head(nn.imp.probs.grid)

#Calculate ROC curve
roc.imp.nn.grid <- roc(data.test$RESPONSE,nn.imp.probs.grid[,"Good"])
#plot the ROC curve
plot(roc.imp.nn.grid,col=c(2))
auc(roc.imp.nn.grid)

#  reduced model using the new dataset (step function for lr method)
nn.red.fit.grid <- train(RESPONSE~., data=data.train.red,trControl=tcontrol,method="nnet",tuneGrid=tunegrid.nn,preProcess = c("center", "scale") )
print(nn.red.fit.grid)
nn.pred.red.grid <- predict(nn.red.fit.grid,newdata=data.test.red)
nn.pred.red.grid
cm.nn.red.grid=confusionMatrix(nn.pred.red.grid, data.test.red$RESPONSE )
cm.nn.red.grid
nn.probs.red.grid<-predict(nn.red.fit.grid, newdata=data.test.red, type="prob")
head(nn.probs.red.grid)
#Calculate ROC curve
roc.nn.red.grid <- roc(data.test.red$RESPONSE,nn.probs.red[,"Good"])
#plot the ROC curve
plot(roc.nn.red.grid,col=c(2),main= "ROC CURVE")
auc(roc.nn.red.grid)
# summary
accu.nn=c(cm.nn$overall['Accuracy'],cm.imp.nn$overall['Accuracy'],cm.nn.grid$overall['Accuracy'],cm.imp.nn.grid$overall['Accuracy'],cm.nn.red$overall['Accuracy'],cm.nn.red.grid$overall['Accuracy'])
kappa.nn=c(cm.nn$overall['Kappa'],cm.imp.nn$overall['Kappa'],cm.nn.grid$overall['Kappa'],cm.imp.nn.grid$overall['Kappa'],cm.nn.red$overall['Kappa'],cm.nn.red.grid$overall['Kappa'] )
Sensi.nn=c(cm.nn$byClass['Sensitivity'],cm.imp.nn$byClass['Sensitivity'],cm.nn.grid$byClass['Sensitivity'],cm.imp.nn.grid$byClass['Sensitivity'],cm.nn.red$byClass['Sensitivity'],cm.nn.red.grid$byClass['Sensitivity'] )
Spe.nn=c(cm.nn$byClass['Specificity'],cm.imp.nn$byClass['Specificity'],cm.nn.grid$byClass['Specificity'],cm.imp.nn.grid$byClass['Specificity'],cm.nn.red$byClass['Specificity'],cm.nn.red.grid$byClass['Specificity'] )
auc.nn=c(auc(roc.nn),auc(roc.imp.nn),auc(roc.nn.grid),auc(roc.imp.nn.grid),auc(roc.nn.red),auc(roc.nn.red.grid))

df.nn<-data.frame(accu.nn,kappa.nn,Sensi.nn,Spe.nn,auc.nn)
row.names(df.nn)<-c("Full Model", "Reduced imp Model","Full Model tuned", "Reduced Model imp tuned", "Reduced model", "reduced model tuned")
df.nn

# https://quantdev.ssri.psu.edu/sites/qdev/files/09_EnsembleMethods_2017_1127.html
# Plot the best model for each method:

bestmodel.nn<-df.nn[6,]


plot(roc.red.lr, col = "cadetblue1",  main = "ROC")
plot(roc.tree.red.grid, col = "red",  add = TRUE)
plot(roc.svm.red.grid, col = "blueviolet",  add = TRUE)
plot(roc.knn.grid, col = "green",  add = TRUE)
plot(roc.rf.grid, col = "yellow",  add = TRUE)
plot(roc.imp.nn.grid, col="pink", add=TRUE)
legend("bottomright", 
       legend = c("Logistic Regression", "Decision Tree","SVM Radial","KNN","Random Forest","Neutral network"), 
       col =c("cadetblue1","red","blueviolet","green","yellow","pink"),
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.20, 0.10))