setwd("C:/Users/Emal Ismail/Desktop/AI/University/2019 - Fall/ITC - 255 - Statistical Data Analysis/Project - Emal Ismail/Project 2 - Bi-Variate a simple regression project")
library(frequency)
library(plyr)
library(Hmisc)
library(xtable)
library(DescTools)
library(e1071)
library(ggplot2)
library(colorspace)

data=read.csv("Emal Ismail.csv")
data=as.data.frame(data)
View(data)

##-------------------------------------------------
##------- Removing Outliers of Livestock Net Income
##-------------------------------------------------

net_income = data$Livestock.Net.Income
q1 = quantile(net_income, 0.25)
q3 = quantile(net_income, 0.75)
IQR = q3-q1
lower_f = q1-0.5*IQR
upper_f = q3+0.5*IQR
lower_f
upper_f

income_outlier_free=net_income[net_income>lower_f & net_income < upper_f]
min(income_outlier_free)
max(income_outlier_free)

Par(mfrow=c(1,2))

boxplot(net_income, horizontal = TRUE, 
        title = "Livestock Net Income Boxplot with outliers",
        col="chocolate",
        border="brown",
        xlab="Livestock net Income in Afs"
        )

boxplot(income_outlier_free, horizontal = TRUE,
        mian = "Livestock Net Income Boxplot with outliers",
        col="chocolate",
        border="brown",
        xlab="Livestock net Income in Afs")


## hist for livestock income


hist(net_income,
     main="Farmers Livestock Net Income - Outliers",
     xlab="Livestock Net Income in Afs",
     col="chocolate",
     ylab = "Density",
     border="brown"
)

curve(dnorm(x, mean=mean(net_income), 
            sd=sd(net_income)), 
      add=TRUE, col="red") 

### with and without outliers
hist(income_outlier_free, freq=FALSE, 
     xlab="Livestock Net Income in Afs",
     ylab = "Density",
     col="chocolate",
     border="brown",
     main="Livestock Net Income - No Outliers")

curve(dnorm(x, mean=mean(income_outlier_free), 
            sd=sd(income_outlier_free)), 
      add=TRUE, col="red") 



## vraibales discrete table statistical methods


# skewness and Kurtosis of live stock for caption of Histogram and explaination of outliers:
skew_kur_fun = function(x,y){
  
  re = skewness(x)
  ku = kurtosis(x)
  
  re1 = skewness(y)
  ku1 = kurtosis(y)
  
  if (re < -1 || re > 1){
    cat(" Based on the skewness of first dataset which is equal to: ", re , "\
and also shown in the first graph we can conclude that the distribution seems to be highly skewed.")
  } else if(re > -1 && re < -0.5){
    cat(" Based on the skewness of first dataset which is equal to: ", re , "\
and also shown in the first graph we can conclude that the distribution is moderatley Skewed. \
Where it has long left tail therefore, it is Left-skewed distributions also called negatively-skewed distribution.")
  } else if (re >0.5 && re <1){
    cat(" Based on the skewness of first dataset which is equal to: ", re ,"\
and also shown in the first graph we can conclude that the distribution is moderatley Skewed. \
Where it has long right tail therefore it is Right-skewed distributions a also called positive-skew distribution.")
  }else if(re > -0.5 && re < 0.5){
    cat(" Based on the skewness of first dataset which is equal to: ", re ,"\
and also shown in the first graph we can conclude that the distribution is approximately Symmetric.")
  }
  
  if (ku > -2 && ku < 2){
    cat("\n")
    cat("Subsequently, kurtosis of first dataset which is equal to: ", ku ,"is between -2 and +2 and it is considered acceptable in\
order to prove normal univariate distribution")
  } else {
    cat("\n")
    cat("Subsequently, kurtosis of first dataset which is equal to: ", ku ,"is greater or less than -2 and +2. \
Therefore, it's not considered to be acceptable in order to prove normal univariate distribution.")
  }
  
  
  ##------------------------- Second Dataset analysis
  cat("\n")
  cat("\n")
  cat("-------------------------------------------------------------------")
  cat("\n")
  cat("Furthermore the second datasets Skewness and Kurtosis is as follow:  ")
  cat("\n")
  
  if (re1 < -1 || re1 > 1){
    cat(" Based on the skewness of second dataset which is equal to: ", re1 , "\
and also shown in the second graph we can conclude that the distribution seems to be highly skewed.")
  } else if(re1 > -1 && re1 < -0.5){
    cat(" Based on the skewness of second dataset which is equal to: ", re1 , "\
and also shown in the second graph we can conclude that the distribution is moderatley Skewed. \
Where it has long left tail therefore, it is Left-skewed distributions also called negatively-skewed distribution.")
  } else if (re1 >0.5 && re1 <1){
    cat(" Based on the skewness of second dataset which is equal to: ", re1 ,"\
and also shown in the second graph we can conclude that the distribution is moderatley Skewed. \
Where it has long right tail therefore it is Right-skewed distributions a also called positive-skew distribution.")
  }else if(re1 > -0.5 && re1 < 0.5){
    cat(" Based on the skewness of second dataset which is equal to: ", re1 ,"\
and also shown in the second graph we can conclude that the distribution is approximately Symmetric.")
  }
  
  if (ku1 > -2 && ku1 < 2){
    cat("\n")
    cat("Subsequently, kurtosis of second dataset which is equal to: ", ku1 ,"is between -2 and +2 and \
it is considered acceptable in order to prove normal univariate distribution.")
  } else {
    cat("\n")
    cat("Subsequently, kurtosis of second dataset which is equal to: ", ku1 ,"is greater or less than -2 and +2. \
Therefore, it's not considered to be acceptable in order to prove normal univariate distribution.")
  }
  
  par(mfrow=c(2,1))
  plot(density(x), col= "red")
  plot(density(y), col= "darkgreen")
}


### Functions for finding mean, median, skewness, Kurtosis, min , max, standard divation

cont_fun = function(result){
  
  maxx=max(result)
  minn= min(result)
  meanX=mean(result)
  #modex=mode(result)
  skew=skewness(result)
  kur=kurtosis(result)
  stan.Dev=sd(result)
  
  
  C=c("Min",minn,"max: ",maxx,"mean: ", meanX ,"Std. Dev",stan.Dev ,
      "skewness: ", skew, "kurtosis: ", kur)
  final = as.data.frame(C)
  return(final)
  
}

data_rainfall = data$r5
data_temperature = data$t5
data_livestock = data$Livestock.Net.Income
### -------------------
#Temperature
#----------------------
cov(data_livestock,data_temperature)
#co-efficient

cor(data_temperature,data_livestock)*100
#corrolation

l_temperature = lm(data_livestock~data_temperature)
l_temperature

#Linear Model
summary(l_temperature)

##-------------------
#rainfall
#--------------------

cov(data_rainfall,data_livestock)
#co-efficient

cor(data_rainfall,data_livestock)*100
#corrolation

l_Rainfall = lm(data_livestock~data_rainfall)
l_Rainfall
#Linear Model
summary(l_Rainfall)

#------------------------ plots of the temperature data
## --- output

scatter.smooth(data_livestock, data_temperature,
               col = "blue", 
               main="Temperature vs Live Stock", 
               pch=20,
               xlim = c(-10000, 150000))

abline(lm(data_livestock~data_rainfall, data=data))

#------------------------ plots of the rainfall data

scatter.smooth(data_livestock, data_rainfall,
               col = "blue", 
               main="Temperature vs Live Stock", 
               pch=20,
               xlim = c(-10000, 100000))

abline(lm(data_rainfall~data_livestock, data=data))
