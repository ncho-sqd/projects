cat("\014")
rm(list = ls())

#Load header
source("D:/git/header.R")

#Import data
train <- read.csv(paste0(importdir,"/kaggle/titanic/train.csv"),stringsAsFactors = F)
test <- read.csv(paste0(importdir,"/kaggle/titanic/test.csv"),stringsAsFactors = F)
answer <- read.csv(paste0(importdir,"/kaggle/titanic/gender_submission.csv"),stringsAsFactors = F)

#Summarize data
str(train)
summary(train)

#Revise datatype
dedup <- sapply(train,unique)
train$Survived <- factor(train$Survived,levels = c(1,0))
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
str(train)

#Visualize

