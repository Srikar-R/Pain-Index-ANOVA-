#Objective: To check whether the different genders feel the same relief of pain and to check whether each drug gives 
# same amount of relief from pain

#Data Description:

#The dataset contains mainly 4 variables, they are Gender, Drug type Interaction and Pain Index. Gender, Interaction and Drug type are independant variables (X)
# and dependent variable is the Pain Index (Y). The two genders being Male and Female. The different type of drugs are A, B and C which are pain 
#relieving drugs. The pain index is on a scale from 1-20 with 1 being the least amount of pain and 20 being the most amount of pain.
#The interaction category contains the interaction between each gender with respect to the 3 drug types, 

#Data Summary

library(readxl)
dat <- read_excel("C:/Users/Srikar/Desktop/SS/R/Sem 5/Design of Exp/Practical 4/data.xlsx")
head(dat,10)

str(dat)

#Gender,Drug and INteraction are character types. Pain Index is of numeric type

dim(dat)

#There are 33 observations and 4 variables

#Important terminologies:

#1)Two way Analysis- A two-way ANOVA test is a statistical test used to determine the effect of two nominal predictor variables on a continuous outcome variable. 
#A two-way ANOVA tests the effect of two independent variables on a dependent variable

#2)Interaction-Interaction effects represent the combined effects of factors on the dependent measure. When an interaction effect is present, 
#the impact of one factor depends on the level of the other factor


#Hypothesis Testing


#1)Hypothesis with respect to gender

#Null Hypothesis (Ho) : µ 1 =µ 2 (Each Gender has eqaul pain index)
#Alternative Hypothesis (H1):  µ 1 =/ µ 2 (Genders have different pain index)

#2) Hypothesis with respect to drug

#Null Hypothesis (Ho): µ 1 =µ 2 =µ 3 (Each drug gives the same amount of pain relief) 
#Alternative Hypothesis (H1):  (Each drug gives different amount of pain relief) 


#3)Hypothesis with respect to interaction between factors

#Null Hypothesis (Ho): There is no interaction between gender and drug type
#Alternate Hypothesis (H1): There is interaction between gender and drug type

#Procedure

#1) Converting the factor variables as factors

gen=as.factor(dat$Gender)
drg=as.factor(dat$Drug)
int=as.factor(dat$Interaction)

#The function factor has helped us encode the vector as a factor for performing the two-way analysis

#2) Visualizing the data

par(mfrow=c(1,2))
A.box=boxplot(dat$`Pain Index`~dat$Gender,xlab = 'Gender',ylab = 'Pain Index',col=c('red','blue'),main="Gender X Pain Index")
B.box=boxplot(dat$`Pain Index`~dat$Drug,xlab = 'Drug',ylab = 'Pain Index',col=c('purple','gold'),main="Drug Type X Pain Index")

Table.A <- A.box$stats
colnames(Table.A)<-A.box$names
rownames(Table.A)<-c('min','lower quartile','median','upper quartile','max')
Table.A

#We see that the minimum level of pain level of female and male are 2.4 and 7 respectively.The median pain levels of female
#and male are 7.025 and 9.81. The maximum pain levels of female and male are 9.69 and 16.60. We observe 2 outliers in female

Table.B <- B.box$stats
colnames(Table.B)<-B.box$names
rownames(Table.B)<-c('min','lower quartile','median','upper quartile','max')
Table.B

#The minimum pain levels relieved by Drug A,B and C are 5.87,3.84 and 9.410. The median levels of each drug are 7.035 , 9.420, 11.900
#The mximum pain levels of each drug are 8.890, 12.900, 14.000.We observe 2 outliers in Drug A. One outlier above the maximum value
#in Drug B and one outlier below the the minimum value of Drug C

C.box=boxplot(dat$`Pain Index`~dat$Interaction,xlab = 'Interaction',ylab = 'Pain Index',col=c('blue','green'),main="Interaction X Pain Index",horizontal=TRUE)

Table.C <- C.box$stats
colnames(Table.C)<-C.box$names
rownames(Table.C)<-c('min','lower quartile','median','upper quartile','max')
Table.C

#The table provides the minimum, first quarter,median,third quarter and maximum values of the interaction which can be grahically represented
#for better understanding.

#Since th outliers are naturally part of the population and are not any sampled values, we will consider the outliers for both 
#the factors.

#3)Creating the anova mod

model=aov(dat$`Pain Index`~dat$Gender+dat$Drug+dat$Interaction)
summary(model)

#As The p-value of of gender is below the significance level of 5% (0.000397),we will reject the Null-Hypothesis. Similarly
#the p-value of drug (0.041804) is below the 5% significance value, we will reject the Null-Hypothesis here as well. Sinnce we rejected 
#the null hypothesis in both the cases, we wil say that the factors are significant from each other. Since the p-value of Interaction
#is above 5% (0.05), we say that there is no interaction between gender and drug.

#4) Considering the factor,'gender'  and 'drug' as they show significance

library(lsmeans)

PI=dat$`Pain Index`

lm1=lm(PI~dat$Gender+dat$Drug+dat$Interaction)
lsm1=lsmeans(lm1,"Gender")
lsm1

#This shows the average predictions of the pain levels along with their confidence level.
#Female pain levels has a 95 % confidence level of (5.39, 8.54)
#Male pain levels has a 95 % confidence level of (9.19,11.75)

pairs(lsm1)


#We find an average pain level difference of  3.51 between female and male.

lsm2=lsmeans(lm1,"Drug")
lsm2
#This shows the average predictions of the pain levels along with their confidence level.
#Drug A pain levels has a 95 % confidence interval of (5.39, 8.54)
#Drug B pain levels has a 95 % confidence interval of (6.63,10.78)
#Drug C pain levels has a 95 % confidence interval of (8.50,12.12)

pairs(lsm2)

#We observe that only A and C are statistically significant as their p-value is lower than the significance value (0.05),
#Drug A and Drug B are not significant with respect to their mean pain index levels and Drug B and Drug C are not significant either
#with respect to thier mean levels of pain index.

#Analysis

#1) We observe from the ANOVA table constructed above that, the pain levels of male and female are different. It can be observed that
#on an average female have lower pain levels or high pain tolerance levels. Men have lower pain tolerance levels as their pain index is high
#On an average, women have lower pain levels than men with an estimate of 3.51.

#2)From the above ANOVA table, we can observe that the different pain levels of people after taking the different types of drugs are different.
#It can be observed that Drug A and Drug C are significantly different from each other. On an average, Drug A gives more relief from pain than
#drug C by providing lesser pain level of 3.19 than Drug C. Drug A and Drug B provide similar type of pain relief, Drug C and Drug B provide
#similar type of pain relief

#3) There is no interaction between the the two factors Drug and Gender.This means that by increasing or decreasing the levels of any of the
#factors will not have an impact on the other factor.

#Conclusion

#1) Female have more pain tolerance than males

#2) Drug A provides the most pain relief from the rest




