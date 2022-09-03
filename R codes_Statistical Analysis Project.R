#Statistical Analysis and Prediction of Chances of Graduate Admission


install.packages("colorspace")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("leaps")

library(tidyverse)
library(corrplot)
library(leaps)
library("ggplot2")

my_data = read.csv(file.choose(),header = TRUE)
my_data

#To check null values
any(is.na(my_data))



#Hypothesis testing 1-sample

#We have selected the variable GRE Score to do this analysis. The Test Statistic 
#that we are going to use here is z-test because the variance of the dataset is
#unknown and the sample is more than 30.

#We are going to determine if there is evidence with significance level of 0.05 
#to support our claim that the mean GRE Score is different from 310.

#Hypothesis statement

#Null hypothesis = population mean of GRE Score is equal to 310
#Alternate hypothesis = population mean of GRE Score is not equal to/different 
#from 310

#Level of significance = 0.05

n <- length(my_data$GRE.Score)
n

s <- var(my_data$GRE.Score)
s

z_stat <- (mean(my_data$GRE.Score)-310)/(s/sqrt(n))
z_stat

#Test statistic = 1.034 is greater than -1.96 and less than +1.96
#Therefore we accept the null hypothesis

p_value <- 2*pnorm(-abs(z_stat))
p_value

#p-value = 0.301 is greater than significance level of 0.05
#Therefore we accept the null hypothesis



#Hypothesis testing 2-sample

#Here we've to determine if there is any difference  between the mean  
#of chance of admission of students with research experience and 
#mean of chance of admission of students without research experience using the 
#t-test with 5% significance level. The dataset will be split into 2 groups based 
#on the value of the Research column which is students with research experience 
#and students without research experience. The variances of both groups are 
#assumed to be not equal.

#Hypothesis statement

#Null Hypothesis = sample mean of chance of admission of students with research 
#experience is equal to sample mean of chance of admission of students without 
#research experience
#Alternate Hypothesis = sample mean of chance of admission of students with 
#research experience differs from sample mean of chance of admission of students 
#without research experience

StudentWResearch <- my_data[which(my_data$Research=="1"),]
StudentWOResearch <- my_data[which(my_data$Research=="0"),]
StudentWResearch
StudentWOResearch

n1 = 219
n2 = 181

x1 <- mean(StudentWResearch$Chance.of.Admit)
x1
x2 <- mean(StudentWOResearch$Chance.of.Admit)
x2

v1 <- var(StudentWResearch$Chance.of.Admit)
v1
v2 <- var(StudentWOResearch$Chance.of.Admit)
v2

t = (x1-x2-0)/(sqrt((v1/n1)+(v2/n2)))
t

dof = ((v1/n1)+(v2/n2))^2/{(((v1/n1)^2)/(n1-1))+(((v2/n2)^2)/(n2-1))}
dof

alpha = 0.05
t.alpha = qt(alpha/2,floor(dof))
t.alpha

#Since t = 13.35 is greater than t.alpha = 1.96 we reject the null hypothesis.



#Correlation

#The type of correlation coefficients that we've used here is Pearson's 
#product moment correlation coefficient(Because we're considering two 
#quantitative variables to see if there's a linear relationship between them).
#we've selected few variables to perform this correlation analysis to determine 
#the strength  of association(linear  relationship) between two variables.

#Relationship between TOEFL Score and GRE Score

ggplot(data = my_data) +
  geom_point(mapping = aes(x = TOEFL.Score, y = GRE.Score)) +
  geom_smooth(mapping = aes(x = TOEFL.Score, y = GRE.Score))

Corr <- cor.test(my_data$GRE.Score, my_data$TOEFL.Score, 
                 method = "pearson")
Corr

#It can be seen that TOEFL.Score increases as the GRE.Score increases. 
#Since r = 0.836, it shows a strong positive linear relationship. A 
#scatter plot and correlation analysis of the data indicates that there is 
#relatively strong positive linear association between x, TOEFL.Score and 
#y, GRE.Score.

#Significance Test for Correlation.

#To provide more evidence in supporting the fact that there is a linear 
#relationship between TOEFL.Score and GRE.Score, a significance test is 
#conducted at significance level of 0.05.

#Hypothesis statement

#Null hypothesis = There is no linear correlation between TOEFL.Score and 
#GRE.Score
#Alternate hypothesis = There exists linear correlation between TOEFL.Score 
#and GRE.Score

#test statistic, t = 30.391
#p-value = 2.2*10^-16
#the p-value is less than significance level of 0.05. therefore the null 
#hypothesis is rejected. Therefore there is sufficient evidence of linear 
#relationship between TOEFL Score and GRE Score of students at the 5% level 
#of significance. 

#Relationship between CGPA and TOEFL.Score 

ggplot(data = my_data) +
  geom_point(mapping = aes(x = CGPA, y = TOEFL.Score)) +
  geom_smooth(mapping = aes(x = CGPA, y = TOEFL.Score))

Corr1 <- cor.test(my_data$CGPA, my_data$TOEFL.Score, 
                  method = "pearson")
Corr1

#It can be seen that CGPA increases as the TOEFL.Score increases. 
#Since r = 0.828, it shows a strong positive linear relationship. A 
#scatter plot and correlation analysis of the data indicates that there is 
#relatively strong positive linear association between x, CGPA and 
#y, TOEFL.Score.

#Significance Test for Correlation.

#In  providing  evidence  of  a  linear  relationship  between  CGPA  and  
#TOEFL.Score  at  0.05  level  of significance, significance test has been 
#conducted.

#Hypothesis statement

#Null hypothesis = There is no linear correlation between CGPA and TOEFL.Score
#Alternate hypothesis = There exists linear correlation between CGPA and 
#TOEFL.Score

#test statistic, t = 29.506
#p-value = 2.2*10^-16
#since the p-value is less than significance level of 0.05, the null hypothesis 
#is rejected. Therefore, there is sufficient evidence of linear relationship 
#between CGPA and TOEFL.Score.

#from the correlation analyses that we've done, a strong positive correlation  
#between student's CGPA and student's GRE Score are found with variable TOEFL 
#Score.



#Regression

#It is important to describe the impact of one independent variable on the 
#other dependent variable, therefore, regression analysis has also been conducted. 
#Let us consider the dependent variable(y) in this case is the collected variable 
#Chance.of.Admit and the independent variable(x) is the variable CGPA. 
#This analysis aims to test the existence of a linear relationship between the 
#variable x and y. 

linearMod <- lm(my_data$Chance.of.Admit ~ my_data$CGPA)
print(linearMod)
summary(linearMod)

ggplot(data = my_data) +
  geom_point(mapping = aes(x = CGPA, y = Chance.of.Admit)) +
  geom_smooth(mapping = aes(x = CGPA, y = Chance.of.Admit))

#Based on the graph generated, the regression model involves a single 
#independent variable and it is called simple regression. The regression 
#model is a positive linear relationship with an equation of 
#y = -1.0715 + 0.2088x 

#Interpretation of Intersection Coefficient(b0) and Slope Coefficient(b1)

#The value of intersection coefficient(b0) = -1.0715 indicates that the 
#estimated average value of Chance.of.Admit(y) when the value of CGPA(x) is 0. 
#Here, as no students would get 0 for CGPA unless they did not participate in 
#any compulsory academic activities, it is safe to say that b0 is just a value 
#of the chance of admit that is not explained by CGPA.

#The value of slope coefficient(b1) = +0.2088 indicates the average value of 
#Chance.of.Admit as a result of a one-unit change in CGPA. In this case, 
#Chance.of.Admit will increase by 0.2088 on average for each additional 
#one-unit change of CGPA. 

#Therefore, we can say that there is a relationship between the x and y 
#variables based on the calculated value of b1.

eruption.lm = lm(my_data$Chance.of.Admit ~ my_data$CGPA)
summary(eruption.lm)$r.squared

#Coefficient of determination, R squared = 0.7626339

#Since the R square = 0.7626339 which is between 0 and 1, it can be concluded 
#that there exists a weaker linear relationship between CGPA(x) and 
#Chance.of.Admit(y). Some but not all of the variation in Chance.of.Admit is 
#explained by variation in CGPA. In this case, around 76% of the variation in 
#Chance.of.Admit is explained by variation in CGPA.



#Goodness-of-fit Test 

#Goodness-of-fit test is used to test the hypothesis that an observed frequency 
#distribution, in this case the students with and without research experience 
#fits some claimed distribution. A claim that the students with and without 
#research experience has equal proportions is tested at 5% level of significance. 

#Let P1 = Students with research experience 
#Let P2 = Students without research experience 

#Test hypothesis: 
#H0:P1 = P2 = 0.5(Frequency of students with and without research experience has 
#the same proportion which is equal to probability of 0.5)
#H1:At least one of the proportions is different from the others 

ct <- table(my_data$Research)
ct
chisq.test(ct)

#Significance level = 0.05 
#Test statistic = 3.61.
#p-value = 0.05743
#Critical value(0.05,1) = 3.8415 

#Since test statistic = 3.61 < critical value = 3.8415, we accept
#the null hypothesis. 

#Another way to decide whether to reject or fail to reject null hypothesis is 
#by using p-value method. In this case, since the p-value = 0.05743 > 
#significance level = 0.05, Therefore we fail to  reject null hypothesis. 
#There is no significant evidence of a difference in proportions between 
#students with research experience and students without research experience. 



#Chi-Square Test for Independence 

#This analysis is used to find out if there is any relationship between variable 
#Research and variable University Rating. 

#Test hypothesis: 
#Null hypothesis = Variables Research and University Rating are independent. 
#Alternate hypothesis = Variables Research and University Rating are related 
#to each other. 

table(my_data$Research,my_data$University.Rating)
chisq.test(table(my_data$Research,my_data$University.Rating))
alpha <- 0.05
cv.alpha <- qchisq(alpha,df=4,lower.tail = FALSE)
cv.alpha

#Significance level = 0.05 
#Test statistic = 83.3
#Critical value = 9.4877

#Since the test statistic value is greater than critical value, null hypothesis
#is rejected at 0.05 significance level. 
#There is significant evidence to conclude that there exists a relationship 
#between the variables Research and University Rating. Thus, we can conclude 
#that student's research experience and the university rating of student's 
#choice do related to each other. 



#Modeling

library(caret)
grad1<-df[complete.cases(df),]
grad1<-grad1[,-1]
#get 80% index numbers of original dataset
validation.index <- createDataPartition(grad1$admit, p=0.80, list=FALSE)
#choose 20% data as test dataset
validation.data <- grad1[-validation.index,]
#choose 80% data as train dataset
train.data <- grad1[validation.index,]

#Random Forest model
library(randomForest)
rf<-randomForest(admit~.,data=train.data,importance=TRUE)
print(rf)
importance(rf)
varImpPlot(rf)
#We can see that GPA, GRE and TOEFL are most important factors in grad school application.
pred_rf <- predict(rf, validation.data)
pred_rf
rmse=RMSE(pred_rf,validation.data$admit)
r2=R2(pred_rf, validation.data$admit, form = "traditional")
cat("RMSE:", rmse,"\nR^2:", r2)
#The random forest model got a 0.077 RMSE and a 0.738 R^2 which is pretty good.

#Multiple Linear Regression Model
lm_model<-lm(train.data$admit~.,train.data)
summary(lm_model)
lm_model1<-lm(train.data$admit~gre+toefl+lor+gpa+research,train.data)
summary(lm_model1)
pred_lm=predict(lm_model1,validation.data)
library(varhandle)
validation.data$admit=as.factor(validation.data$admit)
rmse=RMSE(pred_lm,validation.data$admit)
r2=R2(pred_lm, validation.data$admit, form = "traditional")
cat("RMSE:", rmse,"\nR^2:", r2)
#The multiple linear regression model got a 0.070 RMSE and a 0.781 R^2 which is even better.

#XGBoost model
library(xgboost)
xgb <- xgboost(data=as.matrix(train.data),label=train.data$admit,
               objective='reg:linear',nrounds=50)

summary(xgb)
rmse=RMSE(xgb,validation.data$admit)
r2=R2(xgb, validation.data$admit, form = "traditional")
cat("RMSE:", rmse,"\nR^2:", r2)



#EDA

attach(df)
names(df) <- c("serial", "gre", "toefl","uni_rating","sop","lor","gpa","research","admit")
head(df, 5)
# Save the default personal theme
my_theme <- theme(
  text = element_text(color = "grey35"),
  plot.title = element_text(size = 20, face = "bold"),
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 11),
  axis.line = element_line(size = 1.2, color = "grey35"),
  legend.box.background = element_rect(color = "grey35", size = 1),
  legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
  legend.title = element_text(face = "bold"))

library("ggplot2")

library(scales) # show the colors
colorsEarth <- c("#DA4511", "#FFBD00", "#6A953F", "#9A6233", "#D3AE7C", "#307CA1")
show_col(colorsEarth, labels = F, borders = NA)

cormat <- round(cor(df),2)
get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)]<- NA
  return(cormat)
}
lower_tri <- get_lower_tri(cormat)
library(reshape2)
melted_cormat <- melt(lower_tri)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value))+geom_tile(color="white")+
  
  scale_fill_gradient2(low = "#FFBD00", high = "#DA4511", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +theme_light()+theme_minimal()+ 
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.text=element_text(angle=20,size=12,face = "bold"),axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))
#We can see the correlations between different elements from above heat map.Basically, GPA plays most important part in admission. 
#It's interesting to see that students with higher GPA usually have higher GRE and TOEFL scores as well.
ggplot(df,aes(gre,color=factor(research)))+geom_density(size=2)+ggtitle("GRE vs Research Distribution")
#We can see from above density plot that students with research experience are more likely to have a higher GRE score.
ggplot(df,aes(gre,toefl))+geom_point()+geom_smooth()+ggtitle("GRE vs TOEFL")
ggplot(df,aes(gre,admit))+geom_point()+geom_smooth()+ggtitle("GRE vs Admit Chance")
ggplot(df,aes(toefl,admit))+geom_point()+geom_smooth()+ggtitle("TOEFL vs Admit Chance")
boxplot(df$gre,col="#0099FF",
        horizontal=TRUE,xlab="GRE",main="Boxplot for GRE")

hist(df$gre,col="#0099FF",
     xlab="GRE",
     ylab="Frequency",
     main="Histogram for GRE",
     labels=TRUE)
#We can see from above plots that the median of GRE is around 318. 
#Also, one should reach 325 if he/she wants to be top 25%.
boxplot(df$toefl,col="#FFCC66",
        horizontal=TRUE,xlab="TOFEL",main="Boxplot for TOEFL")

hist(df$toefl,col="#FFCC66",
     xlab="TOEFL",
     ylab="Frequency",
     main="Histogram for TOEFL",
     labels=TRUE)
#We can see from above plots that the median of TOEFL is around 107. 
#Also, one should reach 112 if he/she wants to be top 25%. 
#However, I highly doubt the reliability of these data on this point.
#I went to graduate school Northeastern University while I barely see people around me got a 100+ Toefl score or a 320+ GRE score.
ggplot(df,aes(gpa,admit))+geom_point(color="#339966")+facet_grid(research~.)
#I want to research the correlation between GPA and admission rate. 
#I find useful to divide it into two groups- students with research experience and students who do not. 
#When compared to students wit research expience, the other group of students barely get 9+ gpa. 
#If you did research before and you gpa is higher than 9, we can be pretty sure you have a BIG chance to be admitted.
library(hexbin)
ggplot(df,aes(x = gpa, y = admit)) +
  
  geom_hex(bins = 26, color = "grey35") +
  theme_light() +
  labs(x = "GPA", y = "Admit Prob", title = "Hive Plot",
       subtitle = "Scatter Plot", caption = "Kaggle:Pokemon Dataset") +
  my_theme +
  scale_fill_gradient2(low = colorsEarth[5], mid = colorsEarth[2], high = colorsEarth[1],
                       midpoint = 8)
