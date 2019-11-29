library(ggplot2) 
library(GGally) 
library(gmodels)
library(forecast)
library(lattice)
library(plotROC)
library(prob)
library(caret)


Bank = read.csv(file="/Users/jared/PredicBankruptcy/Bankruptcy.csv", header= TRUE, sep = ",", na.strings=c("","NA") )

#Summary of our data
summary(Bank)
head(Bank)
tail(Bank)

#Visualizing the Data
#Density Charts

BankDen = Bank
BankDen$D <- factor(Bank$D, levels = c(0, 1),
                            labels = c("Bankrupt", "Healthy"))

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R1, fill=as.factor(BankDen$D))) +
  labs(x= "CASH/CURDEBT", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R2, fill=as.factor(BankDen$D))) +
  labs(x= "CASH/SALES", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R3, fill=as.factor(BankDen$D))) +
  labs(x= "CASH/ASSETS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R4, fill=as.factor(BankDen$D))) +
  labs(x= "CASH/DEBTS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R5, fill=as.factor(BankDen$D))) +
  labs(x= "CFFO/SALES", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R6, fill=as.factor(BankDen$D))) +
  labs(x= "CFFO/ASSETS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R7, fill=as.factor(BankDen$D))) +
  labs(x= "CFFO/DEBTS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R8, fill=as.factor(BankDen$D))) +
  labs(x= "COGS/INV", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R9, fill=as.factor(BankDen$D))) +
  labs(x= "CURASS/CURDEBT", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R10, fill=as.factor(BankDen$D))) +
  labs(x= "CURASS/SALES", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R11, fill=as.factor(BankDen$D))) +
  labs(x= "CURASS/ASSETS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R12, fill=as.factor(BankDen$D))) +
  labs(x= "CURDEBT/DEBTS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R13, fill=as.factor(BankDen$D))) +
  labs(x= "INC/SALES", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R14, fill=as.factor(BankDen$D))) +
  labs(x= "INC/ASSETS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R15, fill=as.factor(BankDen$D))) +
  labs(x= "INC/DEBTS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R16, fill=as.factor(BankDen$D))) +
  labs(x= "INCDEP/SALES", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R17, fill=as.factor(BankDen$D))) +
  labs(x= "INCDEP/ASSETS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R18, fill=as.factor(BankDen$D))) +
  labs(x= "INCDEP/DEBTS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R19, fill=as.factor(BankDen$D))) +
  labs(x= "SALES/REC", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R20, fill=as.factor(BankDen$D))) +
  labs(x= "SALES/ASSETS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R21, fill=as.factor(BankDen$D))) +
  labs(x= "ASSETS/DEBTS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R22, fill=as.factor(BankDen$D))) +
  labs(x= "WCFO/SALES", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R23, fill=as.factor(BankDen$D))) +
  labs(x= "WCFO/ASSETS", y= "density", fill="Bankruptcy") 

ggplot(BankDen) +
  geom_density(alpha = .5, aes(x=BankDen$R24, fill=as.factor(BankDen$D))) +
  labs(x= "WCFO/DEBTS", y= "density", fill="Bankruptcy") 


#Boxplots
ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R1, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CASH/CURDEBT")


ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R2, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CASH/SALES")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R3, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CASH/ASSETS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R4, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CASH/DEBTS")


ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R5, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CFFO/SALES")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R6, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CFFO/ASSETS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R7, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CFFO/DEBTS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R8, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "COGS/INV")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R9, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CURASS/CURDEBT")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R10, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CURASS/SALES")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R11, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CURASS/ASSETS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R12, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "CURASS/DEBTS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R13, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "INC/SALES")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R14, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "INC/ASSETS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R15, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "INC/DEBTS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R16, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "INCDEP/SALES")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R17, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "INCDEP/ASSETS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R18, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "INCDEP/DEBTS")

ggplot(Bank) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "SALES/REC")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R20, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "SALES/ASSETS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R21, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "ASSETS/DEBTS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R22, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "WCFO/SALES")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R23, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "WCFO/ASSETS")

ggplot(Bank) +
  geom_boxplot(aes(x=Bank$D, y=Bank$R24, group= Bank$D)) +
  scale_y_log10(breaks=c(0.1,0.5,1.5,2.5)) +
  labs(x= "Bankrupt                                                              Healthy", y= "WCFO/DEBTS")

#Logistic Regression Models

Bank.df = Bank[,-c(1,3)]

#Cash To Pay Debts
train.id <- sample(c(1:dim(Bank.df)[1]), dim(Bank.df)[1]*0.6)
train.df <- Bank.df[train.id, ]
valid.df <- Bank.df[-train.id, ]

predictBank = glm(D~R1+R2+R3+R4, data=train.df, family = "binomial")
summary(predictBank)

predict.value = predict(predictBank, valid.df, type= "response")

confusionMatrix(as.factor(ifelse(predict.value > 0.5, 1, 0)),
                as.factor(valid.df$D), positive = "1")

ggplot(valid.df)+
  geom_roc(aes(d = valid.df$D, m = predict.value))+
  labs(title= "ROC Curve", x="1-Specificty", y=" Sensitivity")

#Debt Structure
train2.id <- sample(c(1:dim(Bank.df)[1]), dim(Bank.df)[1]*0.6)
train2.df <- Bank.df[train2.id, ]
valid2.df <- Bank.df[-train2.id, ]

predictBank2 = glm(D~R12+R21, data=train2.df, family = "binomial")

summary(predictBank2)

predict.value2 = predict(predictBank2, valid2.df, type= "response")

confusionMatrix(as.factor(ifelse(predict.value2 > 0.5, 1, 0)),
                as.factor(valid2.df$D), positive = "1")

ggplot(valid2.df)+
  geom_roc(aes(d = valid2.df$D, m = predict.value2))+
  labs(title= "ROC Curve", x="1-Specificty", y=" Sensitivity")

#Generation of Current Assets
train3.id <- sample(c(1:dim(Bank.df)[1]), dim(Bank.df)[1]*0.6)
train3.df <- Bank.df[train3.id, ]
valid3.df <- Bank.df[-train3.id, ]

predictBank3 = glm(D~R9+R10+R11, data=train3.df, family = "binomial")

summary(predictBank3)

predict.value3 = predict(predictBank3, valid3.df, type= "response")

confusionMatrix(as.factor(ifelse(predict.value3 > 0.5, 1, 0)),
                as.factor(valid3.df$D), positive = "1")

ggplot(valid3.df)+
  geom_roc(aes(d = valid3.df$D, m = predict.value3))+
  labs(title= "ROC Curve", x="1-Specificty", y=" Sensitivity")

#Inventory and Receivables Turnover
train4.id <- sample(c(1:dim(Bank.df)[1]), dim(Bank.df)[1]*0.6)
train4.df <- Bank.df[train4.id, ]
valid4.df <- Bank.df[-train4.id, ]

predictBank4 = glm(D~R8+R19, data=train4.df, family = "binomial")

summary(predictBank4)

predict.value4 = predict(predictBank4, valid4.df, type= "response")

confusionMatrix(as.factor(ifelse(predict.value4 > 0.5, 1, 0)),
                as.factor(valid4.df$D), positive = "1")

ggplot(valid4.df) +
  geom_roc(aes(d = valid4.df$D, m = predict.value4))+
  labs(title= "ROC Curve", x="1-Specificty", y=" Sensitivity")

#Sales Generation
train5.id <- sample(c(1:dim(Bank.df)[1]), dim(Bank.df)[1]*0.6)
train5.df <- Bank.df[train5.id, ]
valid5.df <- Bank.df[-train5.id, ]

predictBank5 = glm(D~R21, data=train5.df, family = "binomial")

summary(predictBank5)

predict.value5 = predict(predictBank5, valid5.df, type= "response")

confusionMatrix(as.factor(ifelse(predict.value5 > 0.5, 1, 0)),
                as.factor(valid5.df$D), positive = "1")

ggplot(valid5.df) +
  geom_roc(aes(d = valid5.df$D, m = predict.value5))+
  labs(title= "ROC Curve", x="1-Specificty", y=" Sensitivity")

#Asset Flow Measures
train6.id <- sample(c(1:dim(Bank.df)[1]), dim(Bank.df)[1]*0.6)
train6.df <- Bank.df[train6.id, ]
valid6.df <- Bank.df[-train6.id, ]

predictBank6 = glm(D~R5+R6+R7+R13+R14+R15+R16+R17+R18+R22+R23+R24, data=train6.df, family = "binomial")

summary(predictBank6)

predict.value6 = predict(predictBank6, valid6.df, type= "response")

confusionMatrix(as.factor(ifelse(predict.value6 > 0.5, 1, 0)),
                as.factor(valid6.df$D), positive = "1")

ggplot(valid6.df) +
  geom_roc(aes(d = valid6.df$D, m = predict.value6))+
  labs(title= "ROC Curve", x="1-Specificty", y=" Sensitivity")


