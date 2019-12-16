library(caret)
library(randomForest)

# Load in the data
TrainingData<-read.csv('pml-training.csv',header=TRUE,na.strings=c("NA","#DIV/0!", ""))
TestingData<-read.csv('pml-testing.csv',header=TRUE,na.strings=c("NA","#DIV/0!", ""))

# Remove columns with blank or NA data
TrainingData<-TrainingData[,colSums(is.na(TrainingData))==0]
TestingData<-TestingData[,colSums(is.na(TestingData))==0]

# Remove columns containing time stamps or index data
TrainingData<-TrainingData[,c(2,8:60)]
TestingData<-TestingData[,c(2,8:60)]

# Split into training and validation sets
set.seed(1)
TrainPartition<-createDataPartition(TrainingData$classe,p=0.8,list=FALSE)
TrainingPart<-TrainingData[TrainPartition,]
ValidationPart<-TrainingData[-TrainPartition,]

# Program a random forest model with cross validation and confirm on validation data
CrossVal<-trainControl(method='cv',number=2)
RandFor<-train(classe~.,data=TrainingPart,method="rf",trControl=CrossVal,verbose=FALSE)
RandForValidate<-predict(RandFor,newdata=ValidationPart)
RandForValidResult<-confusionMatrix(ValidationPart$classe,RandForValidate)
RandForValidResult

# Finally predict the results for the testing set
RandForTest<-predict(RandFor,newdata=TestingData)
RandForTest