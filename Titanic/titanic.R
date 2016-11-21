library(mice)
setwd("/mnt/it159/pdb/Marketing_Analysis/Brian/File/Data Science/Kaggle/")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

head(train)
head(test)

summary(train)

train <- train[-c(1,4,9:12)]
test <- test[-c(1,4,9:12)]

train$Sex <- gsub("female", 1, train$Sex)
train$Sex <- gsub("male", 0, train$Sex)

test$Sex <- gsub("female", 1, test$Sex)
test$Sex <- gsub("male", 0, test$Sex)

train.glm <-  glm(Survived ~ Pclass + Sex + SibSp + Age, family = binomial, data = train)

summary(train.glm)

p.hats <- predict.glm(train.glm, newdata = test, type = "response")
p.hats
survival <- vector()

for(i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}

ID <- test$PassengerId

kaggle.sub <- cbind(ID, survival) 
colnames(kaggle.sub) <- c("PassengerID", "Survived")
write.csv(kaggle.sub, file = "titanic.csv", row.names = FALSE)
