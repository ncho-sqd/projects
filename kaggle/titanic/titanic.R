# Clear workspace and load header--------------
cat("\014")
rm(list = ls())
source("D:/git/header.R")

#Import data
all_files <- list.files(paste0(importdir,'/kaggle/titanic'),
                        full.names=T,
                        pattern = '.csv$')
df_list <- lapply(all_files, read_csv)
filenames <- sapply(strsplit(sub('.*/','',all_files), split = "\\."), "[", 1)
names(df_list) <- filenames
list2env(df_list, envir = .GlobalEnv)

#Summarize data
sapply(df_list, summary)
missmap(train)
missmap(test)

#Baseline logit model and prediction
regressors <- names(train)[!(names(train) %in% c('PassengerId', 'Name', 'Survived', 'Ticket', 'Cabin'))]
base_logit <- glm(paste('Survived','~',paste(regressors, collapse = '+')), 
                 data = train,
                 family = 'binomial'(link = 'logit'))
summary(base_logit)
anova(base_logit, test='Chisq')
test_pred <- test %>% 
  mutate(Age = mean(Age, na.rm = T),
         Survived_logit = predict(base_logit, test_pred, type = 'response'),
         Survived_pred = ifelse(Survived_logit >= 0.5, 1, 0))

#Assess baseline logit accuracy
test_acc<-
gender_submission %>% 
  left_join(test_pred, by = c('PassengerId' = 'PassengerId')) %>% 
  mutate(pred = test_acc$Survived == test_acc$Survived_pred)
mean(test_acc$pred, na.rm = T)

#Where can the model be improved 
drivars <- c('Sex', 'Pclass', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked')
lapply(select(filter(test_acc, pred==F), drivars), table)