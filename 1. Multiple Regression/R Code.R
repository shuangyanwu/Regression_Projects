library(tidyverse)
library(readxl)
library(car)
library(mice)
library(VIM)

df0<-read_excel("BoneDensity.xls", col_names = TRUE, na=c("","NA"))

# slicing age
df<-df0[df0$Age>=12 & df0$Age<=20, ]

#data summary for interested variables
# Continuous:
summary(df[, c(2,6, 7, 15, 9, 10, 13,14)])
sd(df$Age,na.rm=TRUE)
sd(df$Height,na.rm=TRUE)
sd(df$Weight,na.rm=TRUE)
sd(df$LBMI,na.rm=TRUE)
sd(df$TotalBMD,na.rm=TRUE)
sd(df$TotalBMC,na.rm=TRUE)
sd(df$SpineBMD,na.rm=TRUE)
sd(df$SpineBMC,na.rm=TRUE)

#Categorical
table(df$Race)
table(df$Gender)
table(df$FastGluType)
table(df$Glu2HourType)
table(df$HbA1cType)

# missing data and data cleaning
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(df,2,pMiss)  # missing %
md.pattern(df[,-1],rotate.names=TRUE) 
aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis=0.7, gap=0.5, ylab=c("Histogram of missing data","Pattern"))

# Add pre_diabetes
dim (df[is.na(df$FastGluType)&is.na(df$Glu2HourType)&is.na(df$HbA1cType), ])[1] 
# no rows missing all three tests

df$FastGluType[is.na(df$FastGluType)]<-"miss"
df$Glu2HourType[is.na(df$Glu2HourType)]<-"miss"
df$HbA1cType[is.na(df$HbA1cType)]<-"miss"

test<-df$FastGluType=="Pre-diabetes"|df$Glu2HourType=="Pre-diabetes"|df$HbA1cType=="Pre-diabetes"
Prediabetes<-replace(test, test==TRUE,"Pre-diabetes")
Prediabetes2<-replace(Prediabetes, Prediabetes==FALSE,"Healthy")
df2<-mutate(df, Pre_diabetes=Prediabetes2)
# Count created Pre_diabetes
table(df2[,19])

# Exclude ID and response variables
df2_t<-df2[, - c(1, 9, 10,13,14)]
# impute data using MICE pmm
tempData <- mice(df2_t,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData) 
completedData <- complete(tempData,1)
# get complete data
df3<-as_tibble(cbind(completedData,df2[, c(1, 9, 10,13,14)]))

#Data visualization
TotalBMD<-df3$TotalBMD
TotalBMC<-df3$TotalBMC
SpineBMD<-df3$SpineBMD
SpineBMC<-df3$SpineBMC

Age<-df3$Age;
Height<-df3$Height
Weight<-df3$Weight
LBMI<-df3$LBMI
Race<-df3$Race
Gender<-df3$Gender
Pre_diabetes<-df3$Pre_diabetes

# Matrix plots
x<-cbind(TotalBMD,Age,Height,Weight,LBMI) 
pairs(x,cex = 0.8, col = "darkgreen",pch = 18, 
      main = " Pairs plot for continous variables vs TotalBMD")
x2<-cbind(TotalBMC,Age,Height,Weight,LBMI) 
pairs(x2,cex = 0.8, col = "darkgreen",pch = 18, 
      main = " Pairs plot for continous variables vs TotalBMC")
x3<-cbind(SpineBMD,Age,Height,Weight,LBMI) 
pairs(x3,cex = 0.8, col = "darkgreen",pch = 18, 
      main = " Pairs plot for continous variables vs SpineBMD")
x4<-cbind(SpineBMC,Age,Height,Weight,LBMI) 
pairs(x4,cex = 0.8, col = "darkgreen",pch = 18, 
      main = " Pairs plot for continous variables vs SpineBMC")

# Plot with TotalBMD, Height and Gender
df3$Gender<-factor(df3$Gender)   
sp<-ggplot(data=df3) +
  geom_point(aes(x=Height, y=TotalBMD, group=1,color=Gender))+
  ylab("TotalBMD")+xlab("Height")+ggtitle("TotalBMD vs Height and Gender")
sp+scale_color_manual(values=c("red", "blue"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), 
        legend.position="bottom") 
# Boxplots for TotalBMD
plot(TotalBMD~factor(Race),ylab="TotalBMD",xlab="Race",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,main="TotalBMD vs Race")
plot(TotalBMD~factor(Gender),ylab="TotalBMD",xlab="Gender",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,main="TotalBMD vs Gender")
plot(TotalBMD~factor(Pre_diabetes),ylab="TotalBMD",xlab="Pre_diabetes",
     cex.axis=1.5,cex.lab=1.5,cex.main=1.5,main="TotalBMD vs Pre_diabetes")

# Build model for TotalBMD
# initial model with demographic variables
g_0<-lm (df3$TotalBMD~ df3$Age + factor(df3$Race)+ df3$Gender)
summary(g_0)
anova(g_0)
vif(g_0)
g_1<-lm (df3$TotalBMD~ df3$Age + factor(df3$Race)+ df3$Gender +df3$Height)
summary(g_1)
anova(g_1)
vif(g_1)
g_2<-lm (df3$TotalBMD~ df3$Age + factor(df3$Race)+ df3$Gender +df3$Height+df3$LBMI)
summary(g_2)
anova(g_2)
vif(g_2)
g_3<-lm ((df3$TotalBMD)~ df3$Age + factor(df3$Race) + df3$Gender +
           df3$Height+ df3$LBMI+ df3$Pre_diabetes)

summary(g_3)  # Final model
anova(g_3)
vif(g_3) 
confint(g2)
plot(g_3, 1:2)

g_4<-lm (df3$TotalBMD~ df3$Age + factor(df3$Race)+ df3$Gender +df3$Height+
  df3$LBMI+ df3$Pre_diabetes+df3$Weight) # LBMI and Weight highly correlated
summary(g_4)
anova(g_4)
vif(g_4) 

g_5<-lm (df3$TotalBMD~ df3$Age + factor(df3$Race)+ df3$Gender +df3$Height+
           df3$Weight+ df3$Pre_diabetes)
summary(g_5)
anova(g_5)
vif(g_5) 

############################################################################
# For second model, TotalBMC
g11<-lm (df3$TotalBMC~ df3$Age + factor(df3$Race)+ df3$Gender) 

summary(g11)
vif(g11) 
anova(g11)

g12<-lm (df3$TotalBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height)

summary(g12)
vif(g12) 
anova(g12)

g13<-lm (df3$TotalBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height+
           df3$LBMI) 

summary(g13)
vif(g13) 
anova(g13)

g14<-lm (df3$TotalBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height + 
           df3$LBMI + df3$Pre_diabetes )  # Final model
summary(g14)
vif(g14) 
anova(g14)
plot(g14, 1:2)


g15<-lm (df3$TotalBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height+
  df3$LBMI+ df3$Pre_diabetes+df3$Weight)  # LBMI and Weight highly correlated
summary(g15)
vif(g15) 
anova(g15)

g16<-lm (df3$TotalBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height + 
           df3$Weight + df3$Pre_diabetes)  
summary(g16)
vif(g16) 
anova(g16)

############################################################################
#For third model SpineBMD

g21<-lm (df3$SpineBMD~ df3$Age + factor(df3$Race)+ df3$Gender) 
summary(g21)
vif(g21) 
anova(g21)

g22<-lm (df3$SpineBMD~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height)
summary(g22)
vif(g22) 
anova(g22)

g23<-lm (df3$SpineBMD~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height+
           df3$LBMI) 
summary(g23)
vif(g23) 
anova(g23)

g24<-lm (df3$SpineBMD~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height+
           df3$LBMI+ df3$Pre_diabetes) # Final model
summary(g24)
vif(g24) 
anova(g24)
plot(g24, 1:2)
confint(g24)

g25<-lm (df3$SpineBMD~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height +
           df3$LBMI+ df3$Pre_diabetes +df3$Weight )  
summary(g25)
vif(g25)  # LBMI and Weight highly correlated
anova(g25)


g26<-lm (df3$SpineBMD~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height + 
           df3$Weight + df3$Pre_diabetes)  
summary(g26)
vif(g26) 
anova(g26)

############################################################################
#For fourth model SpineBMC
g31<-lm (df3$SpineBMC~ df3$Age + factor(df3$Race)+ df3$Gender) 
summary(g31)
vif(g31) 
anova(g31)

g32<-lm (df3$SpineBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height)
summary(g32)
vif(g32) 
anova(g32)

g33<-lm (df3$SpineBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height+
           df3$LBMI) 
summary(g33)
vif(g33) 
anova(g33)

g34<-lm (df3$SpineBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height + 
           df3$LBMI + df3$Pre_diabetes)
summary(g34)
vif(g34) 
Anova(g34,type=3)

g35<-lm (df3$SpineBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height + 
  df3$LBMI +df3$Pre_diabetes + df3$Weight)  # LBMI and Weight highly correlated
summary(g35)    
vif(g35) 
anova(g35)

g36<-lm (df3$SpineBMC~ df3$Age + factor(df3$Race)+ df3$Gender + df3$Height + 
           df3$Weight + df3$Pre_diabetes)  
summary(g36) # Race not significant
vif(g36) 
anova(g36)

# Remove Race
g37<-lm (df3$SpineBMC~ df3$Age + df3$Gender + df3$Height + 
           df3$LBMI + df3$Pre_diabetes) # Final model
summary(g37)
vif(g37) 
anova(g37)
plot(g37, 1:2)
confint(g37)
