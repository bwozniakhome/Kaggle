library("mice")
setwd("/mnt/it159/pdb/Marketing_Analysis/Brian/File/Data Science/Kaggle/")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train <- train[-c(1, 4, 9:12)]

train$Sex <- gsub("female", 1, train$Sex)
train$Sex <- gsub("male", 0, train$Sex)

test$Sex <- gsub("female", 1, test$Sex)
test$Sex <- gsub("male", 0, test$Sex)

train.m <- mice(train)
test.m <- mice(test)

train.glm <- with(train.m, glm(Survived ~ Pclass + Sex +  + Age, family = binomial))
train.glm.pool <- pool(train.glm)
summary(train.glm)

p.hats <- predict.glm(train.glm.pool, newdata = test, type = "response")

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
