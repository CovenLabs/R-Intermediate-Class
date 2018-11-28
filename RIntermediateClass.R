###Data Manipulation
round(3.1415)
factorial(3)
sqrt(9)
4+2
6*3
18-6
12/3
A<-1
rm(A)
A
pi
a<-1
b<-5
c<-3
x1<-(-b-sqrt(b^2-4*a*c))/2*a
x2<-(-b+sqrt(b^2-4*a*c))/2*a
x1
x2
View(mtcars)
str(mtcars)
summary(mtcars)
gather(mtcars,"gear or carb", "n",10:11)
getwd()
setwd("~C:/")
datamtc<-read.csv("usdata.csv")
#correlation of mpg and cyl
cormpg_cyl<-cor(datamtc$mpg,datamtc$cyl)
cormpg_cyl
#correlation of all the variables
library(corrplot)
cordata<-cor(datamtc)
cordata
View(cordata)
plot(cordata)
cormpg_cyl<-cor(datamtc$cyl,datamtc$mpg)
cormpg_cyl
corrplot(cordata)
#changes the corrplot visualization to different format
corrplot(cordata, method="number")
corrplot(cordata, method="ellipse")
corrplot(cordata, method="shade")
titanic<-read.csv("titanic.csv")
titanic
View(titanic)
#shows the sum of not availabe in the dataset
sum(is.na(titanic))
#shows the sum of not availabe data for survived variable
sum(is.na(titanic$survived))
sum(is.na(titanic$pclass))
sum(is.na(titanic$sex))
sum(is.na(titanic$age))
count(is.na(titanic$survived))
summary(titanic)
#structure of titanic data
str(titanic)
#data dimension
dim(titanic)
titanic1<-read.csv("titanic.csv")
titanic1
View(titanic1)
titanic1$age[is.na(titanic1$age)]<-mean(titanic1$age,na.rm= T)#replaces missing values by the age mean
library(Amelia)
View(Ameila)
titanic2<-read.csv("titanic.csv")
titanic2
View(titanic2)
#Shows the visualization of missing values in the dataset
missmap(titanic2)
#shows the number of missing values for all variables
sapply(titanic2, function(x) sum(is.na(x)))
usdata<-read.csv("usdata.csv")
View(usdata)
mpgandcyl2<-lm(usdata$mpg~.,data=usdata)
mpgandcyl2
summary(mpgandcyl2)
set.seed(1234)
sutrain<-sample(2,nrow(usdata),prob=c(0.60,0.40), replace=TRUE) #split into to samples, the probability is 0.6,0.4 and replacement is true
sutrain
train<-usdata[sutrain==1, ] 
test<-usdata[sutrain==2, ]     
train
test
nrow(train)
nrow(test)
mymodel<-lm(mpg~.,data=train)
mymodel
summary(mymodel)
pra<-predict(mymodel,test)
pra
summary(pra)
View(pra)
View(test)
titanicnew<-read.csv("titanic.csv")
titanicnew
View(titanicnew)
View(Titanic)
tita<- Titanic
#to save file as .csv into my my system
write.csv(tita, "titan.csv")
#To arrive at a fixed splitting set and result
set.seed(1234)
###Read in the file
mydata<-read.csv("~C:/titan.csv")
###Handling missing numeric values
mydata$age[is.na(mydata$age)]<-mean(mydata$age,na.rm= T)
#Splitting into train and test set.
titrain<-sample(2,nrow(mydata),prob=c(0.60,0.40), replace=TRUE)
train<-mydata[titrain==1, ]
test<-mydata[titrain==2, ]
nrow(train)
nrow(test)
#generalised linear model with a family of binomial (logit) function.
mytitamodel2<-glm(survived~.,family=binomial(link="logit"),data=train)
mytitamodel2
summary(mytitamodel2)
pred<-predict(mytitamodelnew, newdata = test, type = "response")
View(pred)
names(test)[8]<-"predicted"
pred<-ifelse(pred>0.5,1,0)
mis<-mean(pred!=test$survived)
1-mis
ACC<-print(paste('Accuracy',1-mis))
####WEB SCRAPING AND TWEETS MINING
library(rvest)
library(tidyverse)
webpage<-read_html("https://en.wikipedia.org/wiki/Beverly_Aadland")
table<-webpage%>%html_nodes("table.vcard")%>%html_table(header=FALSE)
table<-table[[1]]
Bev<-as.data.frame(table)
View(Bev)
#####TWEETS MINING
library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)
consumerKey <- "###############################"
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerSecret <- "#############################################"
accessToken <- "#################################################"
accessTokenSecret <- "#############################################"
Cred <- OAuthFactory$new(consumerKey="#############################",
                         consumerSecret="###########################################",
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL= "https://api.twitter.com/access_token",
                         authURL="https://api.twitter.com/oauth/authorize")
Cred$handshake()
setup_twitter_oauth("Consumerkey","ConsumerSecret","Access Token","AccessToken Secret")
mydata<-searchTwitter("#keysearch", n=1000)
mydata.df<-ldply(mydata, function(t) t$toDataFrame())
write.csv(mydata.df,"saved.csv")
