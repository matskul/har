## Run analysis on UCI HAR dataset

##TEMP >>>>>>
setwd("~/Desktop/datacourse/har/")
##TEMP <<<<<<

## Script settings variables:
# Base directory of datasets
ucidatasetdir <- "./ucidataset/"
# Features names source file
ffeatures <- paste0(ucidatasetdir,"features.txt")
# Train and test dataset file names
ftestx <- paste0(ucidatasetdir,"test/X_test.txt")
ftesty <- paste0(ucidatasetdir,"test/y_test.txt")
ftestsubj <- paste0(ucidatasetdir,"test/subject_test.txt")
ftrainx <- paste0(ucidatasetdir,"train/X_train.txt")
ftrainy <- paste0(ucidatasetdir,"train/y_train.txt")
ftrainsubj <- paste0(ucidatasetdir,"train/subject_train.txt")
factivity <- paste0(ucidatasetdir,"activity_labels.txt")
#Features filter keywords
features_filter <- c("mean","std") #Select only mean and standard deviation

#File name for the clean output dataset
fcleanout <- "ucihar-clean.csv"

# Read the feature names from corresponding file
features <- read.table(ffeatures,header = F, col.names = c("FeatureID","FeatureName") ,colClasses = c("numeric","character"), sep = " ")
features_num<-nrow(features)
rm(ffeatures) #save the ram :)

ftgrep <- ""
for (fil in features_filter) {
    ftgrep <- paste0(ftgrep,ifelse(ftgrep == "","","|"),fil,"\\(\\)")
}
rm(features_filter)

selected_features<-features[grep(ftgrep,features$FeatureName),]
features_skip <- !(features$FeatureID %in% grep(ftgrep,features$FeatureName))
rm(ftgrep)
coltypes<-vector("character")
for (i in 1:features_num) {
    coltypes<-c(coltypes,ifelse(features_skip[i],"NULL","numeric"))
}
rm(features_skip)

#Read measurements data skipping unneeded columns to save some RAM and time
trainx<-read.table(ftrainx, header = F, colClasses = coltypes,col.names = features$FeatureName)
testx<-read.table(ftestx, header = F, colClasses = coltypes,col.names = features$FeatureName)

#Merge test and train data into one dataset
hardata <- rbind(trainx,testx)
rm(trainx)
rm(testx)

#Get Subject and Activity Names
testy <- read.table(ftesty,header = F, col.names = c("Activity"))
trainy <- read.table(ftrainy,header = F, col.names = c("Activity"))
testsub <- read.table(ftestsubj,header = F, col.names = c("Subject"))
trainsub <- read.table(ftrainsubj,header = F, col.names = c("Subject"))

# And add them to dataset
hardata <- cbind(hardata,rbind(trainy,testy),rbind(trainsub,testsub))
rm(trainy)
rm(testy)
rm(testsub)
rm(trainsub)

# Get readable activity labels and push it to our clean dataset
activity_labels <- read.table(factivity, header = F, sep = " ")
hardata$Activity<-setNames(activity_labels[,2],activity_labels[,1])[unlist(hardata$Activity)]

# Calculate the mean for each Activity and Subject for the output dataset
cleandata <- aggregate(hardata,by = list(hardata$Activity,hardata$Subject), FUN = "mean")[,1:68]
colnames(cleandata)[1] <- "Activity"
colnames(cleandata)[2] <- "Subject"
rm(hardata)
rm(features)
rm(coltypes)

#Write the clean output dataset to CSV file
write.csv(cleandata, fcleanout)


##TEMP
#Write the draft for the Codebook.md
#write.csv(names(cleandata),paste0(fcleanout,"-codebook"))

