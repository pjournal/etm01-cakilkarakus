require(data.table)
install.packages('TunePareto')
require(TunePareto)
require(glmnet)
setwd('C:/Users/cakil.karakus/Documents/RStudio/doc/hw2')
require(lubridate)
testStart=as.Date('2018-11-16')
trainStart=as.Date('2012-07-15')
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

#sources were updated depend on over/under bet type.
source('data_preprocessinghw2_3.r')
source('feature_extractionhw2.r')
source('performance_metrics.r')
source('train_models.r')

matches_data_path='C:/Users/cakil.karakus/Documents/RStudio/doc/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_data_path='C:/Users/cakil.karakus/Documents/RStudio/doc/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)


# preprocess matches
matches=matches_data_preprocessing(matches_raw)

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)

# extract open and close odd type features from multiple bookmakers
features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)


# divide data based on the provided dates 
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
test_features=features[Match_Date>=testStart] 

# run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
str(train_features)
train_features <- train_features[,-(1:4)]     
model=glm(Result_Total~.,data=train_features,family="binomial")
summary(model)
predictions=predict(model, newdata = test_features,type = "response")


## Confusion matrix  different threshold=cutoff values
table(test_features$Result_Total,predictions>=0.8)
table(test_features$Result_Total,predictions>=0.35)
table(test_features$Result_Total,predictions>=0.5)
table(test_features$Result_Total,predictions>=0.4)
table(test_features$Result_Total,predictions>=0.3)

