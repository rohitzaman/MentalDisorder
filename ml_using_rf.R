install.packages("randomForest")
library(randomForest)

data <- read.csv(file.choose(),header = TRUE)
View(data)

set.seed(2)
id<-sample(2,nrow(data),prob = c(0.8,0.2),replace = TRUE)
train_data<-data[id==1,]
test_data<-data[id==2,]

data$Disorder <- as.factor(data$Disorder)
train_data$Disorder <- as.factor(train_data$Disorder)

disorder_forest <- randomForest(Disorder~.,data = train_data)
disorder_forest


p <- predict(disorder_forest, newdata = test_data, type = "prob")
p
