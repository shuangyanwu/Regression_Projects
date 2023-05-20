library(tidyverse)
library(readxl)
library(car)
library(mice)
library(VIM)

#######################################################################
#                                                                     #
#                              Load data                              #
#                                                                     #
#######################################################################
data <- read_excel("BoneDensity.xls", col_names = TRUE, na = c("","NA"))

# slicing age
df <- data [data$Age >= 12 & data$Age <= 20, ]
df

#######################################################################
#                                                                     #
#                        Exploratory Data Analysis                    #
#                                                                     #
#######################################################################

#############################################
#                                           #
#                Data Summary               #
#                                           #
#############################################

# Continuous:
summary(df[, c(2,6, 7, 15, 9, 10, 13,14)])
sd(df$Age,na.rm = TRUE)
sd(df$Height,na.rm = TRUE)
sd(df$Weight,na.rm = TRUE)
sd(df$LBMI,na.rm = TRUE)
sd(df$TotalBMD,na.rm = TRUE)
sd(df$TotalBMC,na.rm = TRUE)
sd(df$SpineBMD,na.rm = TRUE)
sd(df$SpineBMC,na.rm = TRUE)

#Categorical
table(df$Race)
table(df$Gender)
table(df$FastGluType)
table(df$Glu2HourType)
table(df$HbA1cType)

#############################################
#                                           #
#     Data Cleaning and Preprocessing       #
#                                           #
#############################################

# Missing data 
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(df,2,pMiss)  # missing %
md.pattern(df[,-1],rotate.names=TRUE) 
aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
     labels=names(df), cex.axis = 0.7, gap = 0.5, ylab = c("Histogram of missing data","Pattern"))


# Combining Three Pre-diabetes Test Results into One
dim (df[is.na(df$FastGluType)&is.na(df$Glu2HourType)&is.na(df$HbA1cType), ])[1] 
# no rows missing all three tests

df$FastGluType[is.na(df$FastGluType)] <- "miss"
df$Glu2HourType[is.na(df$Glu2HourType)] <- "miss"
df$HbA1cType[is.na(df$HbA1cType)] <- "miss"

test <- df$FastGluType == "Pre-diabetes"|df$Glu2HourType == "Pre-diabetes"|df$HbA1cType == "Pre-diabetes"
Prediabetes <- replace(test, test == TRUE,"Pre-diabetes")
Prediabetes2 <- replace(Prediabetes, Prediabetes == FALSE,"Healthy")

# Created Pre_diabetes column
df2 <- mutate(df, Pre_diabetes = Prediabetes2)
table(df2[, 19])
df2[,c(1, 9, 10,13,14)]

# X data
df2_x <- df2[, - c(1, 9, 10,13,14)]
df2_x

# Impute missing data using MICE pmm
tempData <- mice(df2_x, m=5, maxit=50, meth='pmm', seed=500)
summary(tempData) 
completedData <- complete(tempData, 1)
df3 <- as_tibble(cbind(completedData, df2[, c(1, 9, 10,13,14)]))

#############################################
#                                           #
#              Data Visualization           #
#                                           #
#############################################
TotalBMD <- df3$TotalBMD
TotalBMC <- df3$TotalBMC
SpineBMD <- df3$SpineBMD
SpineBMC <- df3$SpineBMC

Age <- df3$Age;
Height <- df3$Height
Weight <- df3$Weight
LBMI <- df3$LBMI
Race <- df3$Race
Gender <- df3$Gender
Pre_diabetes<-df3$Pre_diabetes

# Matrix Plots
x <- cbind(TotalBMD, Age, Height, Weight, LBMI) 
pairs(x, cex = 0.8, col = "darkgreen", pch = 18, 
      main = " Pairs plot for continous variables vs TotalBMD")

x2 <- cbind(TotalBMC, Age, Height, Weight, LBMI) 
pairs(x2, cex = 0.8, col = "darkgreen", pch = 18, 
      main = " Pairs plot for continous variables vs TotalBMC")

x3 <- cbind(SpineBMD, Age, Height, Weight, LBMI) 
pairs(x3, cex = 0.8, col = "darkgreen", pch = 18, 
      main = " Pairs plot for continous variables vs SpineBMD")

x4 <- cbind(SpineBMC, Age, Height, Weight, LBMI) 
pairs(x4, cex = 0.8, col = "darkgreen", pch = 18, 
      main = " Pairs plot for continous variables vs SpineBMC")

# Plot with TotalBMD, Height and Gender
df3$Gender <- factor(df3$Gender)   

sp <- ggplot(data=df3) +
  geom_point(aes(x = Height, y = TotalBMD, group = 1,color = Gender)) +
  ylab("TotalBMD") + xlab("Height") + ggtitle("TotalBMD vs Height and Gender")

sp + scale_color_manual(values = c("red", "blue")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14,face = "bold"), 
        legend.position = "bottom") 

# Boxplots for TotalBMD vs Race, Gender, and Pre_diabetes
plot(TotalBMD ~ factor(Race), ylab="TotalBMD", xlab="Race",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5, main="TotalBMD vs Race")
plot(TotalBMD ~ factor(Gender), ylab="TotalBMD", xlab="Gender",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5, main="TotalBMD vs Gender")
plot(TotalBMD ~ factor(Pre_diabetes), ylab="TotalBMD", xlab="Pre_diabetes",
     cex.axis=1.5, cex.lab=1.5, cex.main=1.5, main="TotalBMD vs Pre_diabetes")

#######################################################################
#                                                                     #
#                       Model Building - TotalBMD                     #
#                                                                     #
#######################################################################

# initial model with demographic variables
df3$Race <- factor(df3$Race)
g11 <- lm (TotalBMD ~ Age + Race + Gender, data = df3)
summary(g11)
anova(g11)
vif(g11)

# Add more predictors
g12 <- lm (TotalBMD ~ Age + Race + Gender + Height, data = df3)
summary(g12)
anova(g12)
vif(g12)

g13 <- lm (TotalBMD ~ Age + Race + Gender + Height + LBMI, data = df3)
summary(g13)
anova(g13)
vif(g13)

# Final model
g14 <- lm (TotalBMD ~ Age + Race + Gender + Height + LBMI + Pre_diabetes, data = df3)
summary(g14)  
anova(g14)
vif(g14) 

# 95% Confidence Interval for Estimates
confint(g14)

# Residual Plots
plot(g14, 1:2)

g15 <- lm (TotalBMD ~ Age + Race + Gender + Height + LBMI + Pre_diabetes + Weight, data = df3)
summary(g15)
anova(g15)
# LBMI and Weight highly correlated
vif(g15) 

g16 <- lm (TotalBMD ~ Age + Race + Gender + Height + Pre_diabetes + Weight, data = df3)
summary(g16)
anova(g16)
vif(g16) 

#######################################################################
#                                                                     #
#                       Model Building - TotalBMC                     #
#                                                                     #
#######################################################################

# initial model with demographic variables
g21 <- lm (TotalBMC ~ Age + Race + Gender, data = df3)
summary(g21)
anova(g21)
vif(g21)

# Add more predictors
g22 <- lm (TotalBMC ~ Age + Race + Gender + Height, data = df3)
summary(g22)
anova(g22)
vif(g22)

g23 <- lm (TotalBMC ~ Age + Race + Gender + Height + LBMI, data = df3)
summary(g23)
anova(g23)
vif(g23)

# Final model
g24 <- lm (TotalBMC ~ Age + Race + Gender + Height + LBMI + Pre_diabetes, data = df3)
summary(g24)
anova(g24)
vif(g24)
# CI for Estimates
confint(g24)

# Residual Plots
plot(g24, 1:2)

g25 <- lm (TotalBMC ~ Age + Race + Gender + Height + LBMI + Pre_diabetes + Weight, data = df3)
summary(g25)
anova(g25)
vif(g25)

g26 <- lm (TotalBMC ~ Age + Race + Gender + Height + Pre_diabetes + Weight, data = df3)
summary(g26)
anova(g26)
vif(g26)

#######################################################################
#                                                                     #
#                       Model Building - SpineBMD                     #
#                                                                     #
#######################################################################

# initial model with demographic variables
g31 <- lm (SpineBMD ~ Age + Race + Gender, data = df3)
summary(g31)
anova(g31)
vif(g31)

g32 <- lm (SpineBMD ~ Age + Race + Gender + Height, data = df3)
summary(g32)
anova(g32)
vif(g32)

g33 <- lm (SpineBMD ~ Age + Race + Gender + Height + LBMI, data = df3)
summary(g33)
anova(g33)
vif(g33)

# Final model
g34 <- lm (SpineBMD ~ Age + Race + Gender + Height + LBMI + Pre_diabetes, data = df3)
summary(g34)
anova(g34)
vif(g34)

# Confidence Interval for Estimates
confint(g34)

# Residual Plots
plot(g34, 1:2)

g35 <- lm (SpineBMD ~ Age + Race + Gender + Height + LBMI + Pre_diabetes + Weight, data = df3)
summary(g35)
anova(g35)
# LBMI and Weight highly correlated
vif(g35)

g36 <- lm (SpineBMD ~ Age + Race + Gender + Height + Pre_diabetes + Weight, data = df3)
summary(g36)
anova(g36)
vif(g36)

#######################################################################
#                                                                     #
#                       Model Building - SpineBMC                     #
#                                                                     #
#######################################################################

# initial model with demographic variables
g41 <- lm (SpineBMC ~ Age + Race + Gender, data = df3)
summary(g41)
anova(g41)
vif(g41)

g42 <- lm (SpineBMC ~ Age + Race + Gender + Height, data = df3)
summary(g42)
anova(g42)
vif(g42)

g43 <- lm (SpineBMC ~ Age + Race + Gender + Height + LBMI, data = df3)
summary(g43)
anova(g43)
vif(g43)

g44 <- lm (SpineBMC ~ Age + Race + Gender + Height + LBMI + Pre_diabetes, data = df3)
summary(g44)
Anova(g44, type = 3)
vif(g44)

# LBMI and Weight highly correlated
g45 <- lm (SpineBMC ~ Age + Race + Gender + Height + LBMI + Pre_diabetes + Weight, data = df3)
summary(g45)
anova(g45)
vif(g45)

g46 <- lm (SpineBMC ~ Age + Race + Gender + Height + Pre_diabetes + Weight, data = df3)
summary(g46)
anova(g46)
vif(g46)

# From g44: Race not significant, remove Race, final model
g47 <- lm (SpineBMC ~ Age + Gender + Height + LBMI + Pre_diabetes, data = df3)
summary(g47)
Anova(g47, type = 3)
vif(g47)

# CI for Estimates
confint(g47)

# Residual Plots
plot(g47, 1:2)


