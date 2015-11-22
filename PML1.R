library(caret)
library(kernlab)
library(ggplot2)

ptrain <- read.csv("E:/New folder/pml-training.csv", na.strings=c("NA","#DIV/0!", ""))
ptest <- read.csv("E:/New folder/pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))

#removing NA --- 
varsNA <- sapply(ptrain, function(x) mean(is.na(x))) > 0.10
ptrain <- ptrain[, varsNA==F]
ptest <- ptest[, varsNA==F]

#features in first few columns are not important for training (name, timestamp etc.)
ptrain <- ptrain[, -(1:7)]
ptest <- ptest[, -(1:7)]

########################################################################
#First, training on sliced data to check model
########################################################################

set.seed(32323)
sliceTrain <- createDataPartition(ptrain$classe, p=0.7, list=F)
train1 <- ptrain[sliceTrain,]
valid1 <- ptrain[-sliceTrain,]

#training
tControl <- trainControl(method="cv", number=4)

fit <- train(classe ~ ., data=train1, method="rf", trControl=tControl)


#on Validation set
pred <- predict(fit, newdata=valid1)

#Checking accuracy for the RF model
confusionMatrix(valid1$classe, pred)


#################################################################
#Primary Training and testing done
#################################################################
#Now training again on full set
#################################################################

# fitting model using full training set
tControl <- trainControl(method="cv", number=4)
fit <- train(classe ~ ., data=ptrain, method="rf", trControl=tControl)


#ON TEST SET
pred <- predict(fit, newdata=ptest)
pred

pred <- as.character(pred)

# for writing in files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# write in files
pml_write_files(pred)