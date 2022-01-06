#getwd()
#setwd("D:/Desktop/final_analysis")
#install.packages("ggpubr")
#install.packages("moments")
#install.packages("faraway")
library(readxl)
library(ggpubr)
library(moments)
library(MASS)
library(faraway)
M <- read_excel("MANOVA Data.xlsx")
M=M[,c(4,6,7,8,9,10,12,13,14,15)]
str(M)# structure of data we have
summary(M)
# from summary we can see the mean and median of Returns column are not same a identifier that these are not normal
# max values of Returns are very far from the mean which indicate that there are outliers as well for Returns
#will check quantitatively the normality and the outliers


#Box plot for outliers if any:
# wrt to Market Caps factor 1
ggboxplot(
  M, x = c("Market Cap"), y = c("1 year Return", "3 Years Return","5 Years Return","10 Years Return"), 
  merge = TRUE, palette = "jco"
)
# from box plot you can see alot of outliers which are extreme for all the Market caps, we have to remove it

#wrt to Price to Book Ratio (High/Low)
ggboxplot(
  M, x = c("Price to Book Ratio (High/Low)"), y = c("1 year Return", "3 Years Return","5 Years Return","10 Years Return"), 
  merge = TRUE, palette = "jco"
)
# from the box plot you can see most of the outliers are for high PB ratio (just a observation) we have to remove all this

# Removing outliers using mahalanobis function:
cutoff=qchisq(1-0.05,4)
cutoff
mahal=mahalanobis(M[,-c(1,2)],colMeans(M[,-c(1,2)]),cov(M[,-c(1,2)]))
summary(mahal<cutoff)


# 21 outliers 
noout=subset(M,mahal<cutoff)
Mo=noout
Mo
#analysis for Returns:
Mrto=Mo[,c(1,2,3,4,5,6)]#return table seperate
Mrso=Mo[,c(1,2,7,8,9,10)]# risk table separate


#analysis for Returns:
# box plot to verify if the outliers are removed

ggboxplot(
  Mrto, x = c("Market Cap"), y = c("1 year Return", "3 Years Return","5 Years Return","10 Years Return"), 
  merge = TRUE, palette = "jco"
)

ggboxplot(
  Mrto, x = c("Price to Book Ratio (High/Low)"), y = c("1 year Return", "3 Years Return","5 Years Return","10 Years Return"), 
  merge = TRUE, palette = "jco"
)

#now the data looks quite good as compared to last box plot
## since the data is not yet normal we will normalize the data and plot the box plot again 
# Now doing the normality checkup
# Normality
R=Mrto[,-c(1,2)]
R=as.matrix(R)

shapiro.test(R[,1])
ggqqplot(R[,1])

shapiro.test(R[,2])
ggqqplot(R[,2])

shapiro.test(R[,3])
ggqqplot(R[,3])

shapiro.test(R[,4])
ggqqplot(R[,4])

# you can see that the data is not normal 
# we have to perform some transfromation to make it Normal which is one of the key assumptions for doing Manova

#power Transformation:
# and replacing the coulmn values with particular transformation value
R1=R[,1]
R1n=(R1)^(1/4)
shapiro.test(R1n)
ggqqplot(R1n)
#replacing values
Mrto[,3]=(Mrto[,3])^(1/4)

#similarly for others
R2=R[,2]
R2n=(R2+0.8)^(1/4)
shapiro.test(R2n)
ggqqplot(R2n)
Mrto[,4]=(Mrto[,4]+0.8)^(1/4)

R3=R[,3]
R3n=(R3+0.85)^(1/4)
shapiro.test(R3n)
ggqqplot(R3n)
Mrto[,5]=(Mrto[,5]+0.85)^(1/4)

R4=R[,4]
R4n=(R4+0.85)^(1/4)
shapiro.test(R4n)
ggqqplot(R4n)
Mrto[,6]=(Mrto[,6]+0.85)^(1/4)
Mrto
Mrton=na.omit(Mrto)  # omit rows containing NA
Mrton
# do the box plot again to check the normality and outliers if any
# now we have removed the outliers and made it normal

ggboxplot(
  Mrton, x = c("Market Cap"), y = c("1 year Return", "3 Years Return","5 Years Return","10 Years Return"), 
  merge = TRUE, palette = "jco"
)

ggboxplot(
  Mrton, x = c("Price to Book Ratio (High/Low)"), y = c("1 year Return", "3 Years Return","5 Years Return","10 Years Return"), 
  merge = TRUE, palette = "jco"
)

# so now you know, getting a good data is so much important it took lot of effort to reach till here..


##additivity
# checking correlation should have some correlation but coefficents should be less than 0.99 
# correlation close to 1 makes Manova unstable

correl=cor(Mrton[,-c(1,2)],use="pairwise.complete.obs")
symnum(correl)
correl
# all good

##assumption set up
random=rchisq(nrow(Mrton),7) # any no. let say 7 more than 3
fake=lm(random~.,data=Mrton[,])
standardized=rstudent(fake)
fitted=scale(fake$fitted.values)

##normality
hist(standardized)

#linearity
qqnorm(standardized)
abline(0,1)

##homogenity
plot(fitted,standardized)
abline(0,0)
abline(v=0)


#install.packages("energy")
library(energy)

##leven's test

library(car)
#install.packages("car")
leveneTest(Mrton$`1 year Return` ~ Mrton$`Market Cap`*Mrton$`Price to Book Ratio (High/Low)`,
           data= Mrton, center= mean)
#Levene's Test of Equality of Variance: Used to examine whether or not the variance between 
#independent variable groups are equal; also known as homogeneity of variance. assumption for Manova
# since the p value is not significant or greater than 0.05 we cannot reject the hypothesis of 
# homogeneity in variance

leveneTest(Mrton$`3 Years Return` ~ Mrton$`Market Cap`*Mrton$`Price to Book Ratio (High/Low)`,
           data=Mrton, center= mean)


leveneTest(Mrton$`5 Years Return` ~ Mrton$`Market Cap`*Mrton$`Price to Book Ratio (High/Low)`,
           data= Mrton, center= mean)
leveneTest(Mrton$`10 Years Return` ~ Mrton$`Market Cap`*Mrton$`Price to Book Ratio (High/Low)`,
           data= Mrton, center= mean)

# for 5 year and 10 years return Levins test has a significant p value to reject the hypothesis
# It voilates the assumption of Manova for this specific data

# even then lets consider all the dependent variable
# we are checking for two dimension multi normality test
# multinorm

DV=cbind(Mrton$`1 year Return`,Mrton$`3 Years Return`,Mrton$`5 Years Return`,Mrton$`10 Years Return`)
Dv1=cbind(Mrton$`1 year Return`,Mrton$`3 Years Return`)
mvnorm.etest(Dv1, R=200)
Dv2=cbind(Mrton$`1 year Return`,Mrton$`5 Years Return`)
mvnorm.etest(Dv2, R=200)
Dv3=cbind(Mrton$`1 year Return`,Mrton$`10 Years Return`)
mvnorm.etest(Dv3, R=200)
Dv4=cbind(Mrton$`5 Years Return`,Mrton$`10 Years Return`)
mvnorm.etest(Dv4, R=200)
#p values are high enough to not to reject the hypothesis they are indeed multi variate normal
output=lm(DV~ Mrton$`Market Cap`*Mrton$`Price to Book Ratio (High/Low)`, data=Mrton)
#contrasts=list(Manova$`Market Cap`= contr.Sum , Manova$`Price to Book Ratio (High/Low)`=contr.sum))
manova_out=Manova(output, type= "III")   
summary(manova_out, multivariate=T)

# from the summary of Manova we can see that for our factor 1  which is Market Cap
# the wilks coeeficient section the p value is less than 0.05 we can easily reject the null hypothesis that the 
# Mean of return vectors for all the Market Caps are equal, similarly for factor 2 which Price to book ratio.
# since for interaction we have high P value we cannot reject the null hypothesis that there is zero interaction 
# which means since there is no interaction we have to perform analysis for factor 1 and factor 2 seperately to 
# from where the difference is coming from

# consider for factor1
# Now we have to find out from where the difference is coming from which level of Market Cap

R1.lm <- lm(Mrton$`1 year Return` ~ Mrton$`Market Cap`, data = Mrton)
R1.av <- aov(R1.lm)
summary(R1.av)
#p value less than 0.05 reject the null hypothesis now we have to see from bornferonii intervals to know from where the difference is actually coming from
tukey.test <- TukeyHSD(R1.av)
tukey.test
# for small cap - Large cap interval is always positive 

R2.lm <- lm(Mrton$`3 Years Return` ~ Mrton$`Market Cap`, data = Mrton)
R2.av <- aov(R2.lm)
summary(R2.av)
#p value less than 0.05 reject the null hypothesis now we have to see from bornferonii intervals to know from where the difference is actually coming from
tukey.test <- TukeyHSD(R2.av)
tukey.test
# for small cap - Large cap interval is always negative

R3.lm <- lm(Mrton$`5 Years Return` ~ Mrton$`Market Cap`, data = Mrton)
R3.av <- aov(R3.lm)
summary(R3.av)
#p value less than 0.05 reject the null hypothesis now we have to see from bornferonii intervals to know from where the difference is actually coming from
tukey.test <- TukeyHSD(R3.av)
tukey.test
# for small cap - Large cap interval is always negative

R4.lm <- lm(Mrton$`10 Years Return` ~ Mrton$`Market Cap`, data = Mrton)
R4.av <- aov(R4.lm)
summary(R4.av)
#p value less than 0.05 reject the null hypothesis now we have to see from bornferonii intervals to know from where the difference is actually coming from
tukey.test <- TukeyHSD(R4.av)
tukey.test
# for small cap - Large cap interval is always negative
# and for small cap - Mid cap interval is always negative as well


ggboxplot(
  Mrton, x = c("Market Cap"), y = c("1 year Return", "3 Years Return","5 Years Return","10 Years Return"), 
  merge = TRUE, palette = "jco"
)



# final result from this entire analysis:
#Theres is no significant difference between the returns of Mid cap and Large Cap companies irrespective of no. of years of return choosen
#for 10 years of return theres a significant difference between samll cap and mid cap companies returns was higher for Mid cap compared to small cap companies
#except for 10 years of returns small and mid cap companies dont have significantly different return
# small cap companies gave higher  1 years of returns as compared to large cap companies which is little unobvious
# as obvious except for 1 years of return Large cap companies gave higher returns as compared to Small cap companies

#similarly we have to do analysis for factor 2 seperately..

# for visualization..
ggboxplot(
  Mrton, x = c("Price to Book Ratio (High/Low)"), y = c("1 year Return", "3 Years Return","5 Years Return","10 Years Return"), 
  merge = TRUE, palette = "jco"
)

R1_f2.lm <- lm(Mrton$`1 year Return` ~ Mrton$`Price to Book Ratio (High/Low)`, data = Mrton)
R1_f2.av <- aov(R1_f2.lm)
summary(R1_f2.av)
#p value is greater than 0.5 we cannot reject the null hypothesis 
# theres no significant difference between 1 year returns of company with high pb ratio as compared to company with low pb ratio
# no need to look at bornferroni interval as only two levels and theres no difference seen

R2_f2.lm <- lm(Mrton$`3 Years Return` ~ Mrton$`Price to Book Ratio (High/Low)`, data = Mrton)
R2_f2.av <- aov(R2_f2.lm)
summary(R2_f2.av)
# p values is very less we are rejecting the null hypothesis
# theres indeed a signifacant difference between the 3 years return for company with high pb ratio and company with low pb ratio
tukey.test <- TukeyHSD(R2_f2.av)
tukey.test
# Low -High is always negative 
# the company with high Pb ratio had higher 3 years return as compared to company with low pb ratio


R3_f2.lm <- lm(Mrton$`5 Years Return` ~ Mrton$`Price to Book Ratio (High/Low)`, data = Mrton)
R3_f2.av <- aov(R3_f2.lm)
summary(R3_f2.av)
# p values is very less we are rejecting the null hypothesis
# theres indeed a signifacant difference between the 5 years return for company with high pb ratio and company with low pb ratio
tukey.test <- TukeyHSD(R3_f2.av)
tukey.test
# Low -High is always negative 
# the company with high Pb ratio had higher 5 years return as compared to company with low pb ratio


R4_f2.lm <- lm(Mrton$`10 Years Return` ~ Mrton$`Price to Book Ratio (High/Low)`, data = Mrton)
R4_f2.av <- aov(R4_f2.lm)
summary(R4_f2.av)
# p values is very less we are rejecting the null hypothesis
# theres indeed a signifacant difference between the 10 years return for company with high pb ratio and company with low pb ratio
tukey.test <- TukeyHSD(R4_f2.av)
tukey.test
# Low -High is always negative 
# the company with high Pb ratio had higher 10 years return as compared to company with low pb ratio

#final analysis:
# except for the 1 years return all other had significant difference between the company with high and low pb ratio
# as predicted from box plot the company with high pb ratio had higher 3 , 5 and 10 years returns
# for 1 years return there was no difference between them

# here we came at the end of our Analysis for return

### Analysis for Risk..


str(Mrso)
Mrso

# visualization
# wrt to Market Caps:
ggboxplot(
  Mrso, x = c("Market Cap"), y = c("1 Year Risk (SD)", "3 Years Risk (SD)","5 Years Risk (SD)","10 Years Risk (SD)"), 
  merge = TRUE, palette = "jco"
)
# As we have already removed the outliers the box plot looks good and data is perfect 
#wrt to Price to Book Ratio (High/Low):
ggboxplot(
  Mrso, x = c("Price to Book Ratio (High/Low)"), y = c("1 Year Risk (SD)", "3 Years Risk (SD)","5 Years Risk (SD)","10 Years Risk (SD)"), 
  merge = TRUE, palette = "jco"
)

# lets try to remove few more outliers
cutoff=qchisq(1-0.05,4)
cutoff
mahal=mahalanobis(Mrso[,-c(1,2)],colMeans(Mrso[,-c(1,2)]),cov(Mrso[,-c(1,2)]))
summary(mahal<cutoff)

# 15 more outliers 
noout=subset(Mrso,mahal<cutoff)
Mrsoo=noout
Mrsoo

# see the box plot again
ggboxplot(
  Mrsoo, x = c("Market Cap"), y = c("1 Year Risk (SD)", "3 Years Risk (SD)","5 Years Risk (SD)","10 Years Risk (SD)"), 
  merge = TRUE, palette = "jco"
)
ggboxplot(
  Mrsoo, x = c("Price to Book Ratio (High/Low)"), y = c("1 Year Risk (SD)", "3 Years Risk (SD)","5 Years Risk (SD)","10 Years Risk (SD)"), 
  merge = TRUE, palette = "jco"
)

# now it look more good

# lets check for normality

Rs=Mrsoo[,-c(1,2)]
Rs=as.matrix(Rs)

shapiro.test(Rs[,1])
ggqqplot(Rs[,1])

shapiro.test(Rs[,2])
ggqqplot(Rs[,2])

shapiro.test(Rs[,3])
ggqqplot(Rs[,3])

shapiro.test(Rs[,4])
ggqqplot(Rs[,4])

#Data looks quite normal p value is not very less, no need to do the transformation

correl_rs=cor(Mrso[,-c(1,2)],use="pairwise.complete.obs")
correl_rs

# therse a quite good correlation between the dependent variable since the correlation values should be less than 
# 0.99 we are still allowed to apply Manova if the correlation is 1 very close to 1 it makes Manova unstable
# we are good to go..

##assumption set up

random=rchisq(nrow(Mrsoo),7) # any no. let say 7 more than 3
fake=lm(random~.,data=Mrsoo[,])
standardized=rstudent(fake)
fitted=scale(fake$fitted.values)

##normality
hist(standardized)

#linearity
qqnorm(standardized)
abline(0,1)

##homogenity
plot(fitted,standardized)
abline(0,0)
abline(v=0)

#Levins test

library(car)
#install.packages("car")
leveneTest(Mrsoo$`1 Year Risk (SD)` ~ Mrsoo$`Market Cap`*Mrsoo$`Price to Book Ratio (High/Low)`,
           data= Mrsoo, center= mean)
# since the p value is greater than 0.05 we cannot reject the null hypothesis
# 1 years Risk has homogeneous variance across all the levels of both the factor..
leveneTest(Mrsoo$`3 Years Risk (SD)` ~ Mrsoo$`Market Cap`*Mrsoo$`Price to Book Ratio (High/Low)`,
           data= Mrsoo, center= mean)
#since the p value is greater than 0.05 we cannot reject the null hypothesis
# 3 years Risk has homogeneous variance across all the levels of both the factor..
leveneTest(Mrsoo$`5 Years Risk (SD)` ~ Mrsoo$`Market Cap`*Mrsoo$`Price to Book Ratio (High/Low)`,
           data= Mrsoo, center= mean)
#since the p value is greater than 0.05 we cannot reject the null hypothesis
# 5 years Risk has homogeneous variance across all the levels of both the factor..
leveneTest(Mrsoo$`10 Years Risk (SD)` ~ Mrsoo$`Market Cap`*Mrsoo$`Price to Book Ratio (High/Low)`,
           data= Mrsoo, center= mean)
#since the p value is greater than 0.05 we cannot reject the null hypothesis
# 10 years Risk has homogeneous variance across all the levels of both the factor..

# our assumptions for doing Manova stands true..


# Manova
DV_rs=cbind(Mrsoo$`1 Year Risk (SD)`,Mrsoo$`3 Years Risk (SD)`,Mrsoo$`5 Years Risk (SD)`,Mrsoo$`10 Years Risk (SD)`)
output=lm(DV_rs~ Mrsoo$`Market Cap`*Mrsoo$`Price to Book Ratio (High/Low)`, data=Mrsoo)
#contrasts=list(Manova$`Market Cap`= contr.Sum , Manova$`Price to Book Ratio (High/Low)`=contr.sum))
manova_out=Manova(output, type= "III")   
summary(manova_out, multivariate=T)
# look at wilks coefficient row p value
# we can clearly see from the interaction part that p value is large we, cannot reject the null hypothesis of
# zero interaction
# p value for Market cap and PB ratio is very less we are rejecting the null hypothesis that there is no difference 
# between the mean Risk vector for both the factors for there respective levels

#Now since there is no interaction between the two factor we have to consider both the factor seperately
#Basically now we have to do Analysis to check from where the difference is actually coming from
# lets start with factor 1 Market Cap (Analysis):
# for visualization see box plot
ggboxplot(
  Mrsoo, x = c("Market Cap"), y = c("1 Year Risk (SD)", "3 Years Risk (SD)","5 Years Risk (SD)","10 Years Risk (SD)"), 
  merge = TRUE, palette = "jco"
)

Rs1.lm <- lm(Mrsoo$`1 Year Risk (SD)` ~ Mrsoo$`Market Cap`, data = Mrsoo)
Rs1.av <- aov(Rs1.lm)
summary(Rs1.av)
#p value less than 0.05 reject the null hypothesis now we have to see from bornferonii intervals to know from where the difference is actually coming from
tukey.test <- TukeyHSD(Rs1.av)
tukey.test
# for Mid Cap - Large Cap, Small Cap - Large Cap, and Small Cap-Mid cap  the difference of means are always positive

Rs2.lm <- lm(Mrsoo$`3 Years Risk (SD)` ~ Mrsoo$`Market Cap`, data = Mrsoo)
Rs2.av <- aov(Rs2.lm)
summary(Rs2.av)
#p value less than 0.05 reject the null hypothesis now we have to see from bornferonii intervals to know from where the difference is actually coming from
tukey.test <- TukeyHSD(Rs2.av)
tukey.test
# for Small Cap - Large Cap, and Small Cap-Mid cap  the difference of means are always positive

Rs3.lm <- lm(Mrsoo$`5 Years Risk (SD)` ~ Mrsoo$`Market Cap`, data = Mrsoo)
Rs3.av <- aov(Rs3.lm)
summary(Rs3.av)
#p value less than 0.05 reject the null hypothesis now we have to see from bornferonii intervals to know from where the difference is actually coming from
tukey.test <- TukeyHSD(Rs3.av)
tukey.test
# for Small Cap - Large Cap, and Small Cap-Mid cap  the difference of means are always positive

Rs4.lm <- lm(Mrsoo$`10 Years Risk (SD)` ~ Mrsoo$`Market Cap`, data = Mrsoo)
Rs4.av <- aov(Rs4.lm)
summary(Rs4.av)
#p value less than 0.05 reject the null hypothesis now we have to see from bornferonii intervals to know from where the difference is actually coming from
tukey.test <- TukeyHSD(Rs4.av)
tukey.test
# for Mid Cap - Large Cap, Small Cap - Large Cap, and Small Cap-Mid cap  the difference of means are always positive

# final result :

# theres a significant difference for 1 year return risk between all the Market caps, of which Small cap companies
# have higher 1 year return risk than Mid cap companies and for the Large cap companies we have the least 1 year return risk

# Theres no significant difference between the 3 years return risk for Mid cap and Large Cap companies
# while theres a significant difference for 3 years return risk between Small and Large cap comp and between Small and Mid cap comp
# again for small cap companies we have higher 3 year return risk among all, though Mid cap and large cap have no difference

# Theres no significant difference between the 5 years return risk for Mid cap and Large Cap companies
# while theres a significant difference for 5 years return risk between Small and Large cap comp and between Small and Mid cap comp
# again, for small cap companies we have higher 5 year return risk among all, though Mid cap and large cap have no difference

# Theres a significant difference for 10 year return risk between all the Market caps, of which Small cap companies
# have higher 10 year return risk than Mid cap companies and for the Large cap companies we have the least 10 year return risk

### lets start with factor 2 PB ratio (Analysis):
## graph for visualization:
ggboxplot(
  Mrsoo, x = c("Price to Book Ratio (High/Low)"), y = c("1 Year Risk (SD)", "3 Years Risk (SD)","5 Years Risk (SD)","10 Years Risk (SD)"), 
  merge = TRUE, palette = "jco"
)

Rs1_f2.lm <- lm(Mrsoo$`1 Year Risk (SD)` ~ Mrsoo$`Price to Book Ratio (High/Low)`, data = Mrsoo)
Rs1_f2.av <- aov(Rs1_f2.lm)
summary(Rs1_f2.av)
#p value less than 0.05 reject the null hypothesis, 
tukey.test <- TukeyHSD(Rs1_f2.av)
tukey.test
#low - high mean is always positive
#there is a significant difference for 1 year return risk between the companies with high and low pb ratio
# companies with Low pb ratio have higher 1 year return risk as compared to other one

Rs2_f2.lm <- lm(Mrsoo$`3 Years Risk (SD)` ~ Mrsoo$`Price to Book Ratio (High/Low)`, data = Mrsoo)
Rs2_f2.av <- aov(Rs2_f2.lm)
summary(Rs2_f2.av)
#p value less than 0.05 reject the null hypothesis, 
tukey.test <- TukeyHSD(Rs2_f2.av)
tukey.test
#low - high mean is always positive
#there is a significant difference for 3 year return risk between the companies with high and low pb ratio
# companies with Low pb ratio have higher 1 year return risk as compared to other one

Rs3_f2.lm <- lm(Mrsoo$`5 Years Risk (SD)` ~ Mrsoo$`Price to Book Ratio (High/Low)`, data = Mrsoo)
Rs3_f2.av <- aov(Rs3_f2.lm)
summary(Rs3_f2.av)
#p value less than 0.05 reject the null hypothesis, 
tukey.test <- TukeyHSD(Rs3_f2.av)
tukey.test
#low - high mean is always positive
#there is a significant difference for 5 year return risk between the companies with high and low pb ratio
# companies with Low pb ratio have higher 1 year return risk as compared to other one

Rs4_f2.lm <- lm(Mrsoo$`10 Years Risk (SD)` ~ Mrsoo$`Price to Book Ratio (High/Low)`, data = Mrsoo)
Rs4_f2.av <- aov(Rs4_f2.lm)
summary(Rs4_f2.av)
#p value less than 0.05 reject the null hypothesis, 
tukey.test <- TukeyHSD(Rs4_f2.av)
tukey.test
#low - high mean is always positive
#there is a significant difference for 5 year return risk between the companies with high and low pb ratio
# companies with Low pb ratio have higher 1 year return risk as compared to other one
# finally one can say that irrespective of no. of years of return risk the companies with low PB ratio will have 
# higher risk than the company with high PB ratio..

# inferences and final conclusion are on slides











