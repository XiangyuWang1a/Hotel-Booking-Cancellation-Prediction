#==============================================================================#
#                                LIBRARIES                                     #
#==============================================================================#
#library(tidyverse)
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
#==============================================================================#
#                              DATA INITIALIZATION                             #
#==============================================================================#
setwd("C:\\Users\\xiang\\Downloads\\Resort01.csv")
hoteldata =read_csv("Resort01.csv");
#1000 rows sample data using
set.seed(123)
Cancellations <- as.data.frame(sapply(hoteldata, sample, 1000, replace = TRUE))
anyNA(Cancellations) #checks for NAs in the Dataset (if returns false, it's good)
dim(Cancellations)
str(Cancellations)
# Data Cleaning
library(dplyr)
anyNA(Cancellations) #checks for NAs in the Dataset (if returns false, it's good)
Cancellations <- mutate_if(Cancellations, is.character, factor) #makes the categoricals as factors
Cancellations$IsCanceled <-  as.factor(Cancellations$IsCanceled) #makes the binary as factors
Cancellations$IsRepeatedGuest <-  as.factor(Cancellations$IsRepeatedGuest) #makes the binary as factors
levels(Cancellations$Meal) <- list(UndefinedSC=c("Undefined","SC"),BB=c("BB"),FB=c("FB"),HB=c("HB")) #combine Undefined and SC into one (since they're the same)

CanceledBookingData<- Cancellations %>% filter(IsCanceled == 1) #subset of canceled bookings
NonCanceledBookingData <- Cancellations %>% filter(IsCanceled == 0) #subset of noncanceled bookings
RepeatGuestData <- Cancellations %>%  filter(IsRepeatedGuest == 1) #subset of repeat or loyal customers
FirstTimeGuestData <- Cancellations %>%  filter(IsRepeatedGuest == 0) #subset of first time customers
write.csv(Cancellations, "Hotel.csv")

IsCanceledBookingData<- CanceledBookingData #backup data

summary(Cancellations$LeadTime)
table(Cancellations$LeadTime) 
table(Cancellations$Meal)
library(dplyr)
table(Cancellations$Country) %>% as.data.frame() %>% arrange(desc(Freq))

table(Cancellations$CustomerType)
table(Cancellations$LeadTime)

summary(as.numeric((Cancellations$LeadTime)))
# Concatenate the three vectors
leadtimep <- c(Cancellations$LeadTime)

# Compute the largest y value used in the autos
max_num <- max(leadtimep)
max_num

hist(as.numeric(leadtimep), color='blue', breaks=70, 
     xlim=c(0,500), right=F, main="Lead Time Histogram", las=1)
h <- hist(as.numeric(leadtimep), breaks = 10, xaxt = 'n')

library(dplyr)
dfCountryCanceled <- Cancellations %>% group_by(Country) %>% 
  filter(IsCanceled == 1) %>%summarize(IsCanceled = n()) %>%
  arrange(desc(IsCanceled))

##Map data
dfCountryCanceled <- subset(dfCountryCanceled, Country != 'GIB' )
dfCountryCanceled <- subset(dfCountryCanceled, Country != 'NULL' )
dfCountryCanceled$Country[dfCountryCanceled$Country == "CN"] <- "CHN"
#sum(table(dfCountryCanceled$Country))
# table(dfCountryCanceled$Country)
# 70 out of 72 were corrected hence plotting the map
# taking only values greater than 10 cancellations
dfCountryCanceled <- dfCountryCanceled %>% filter(IsCanceled > 10)
#tail(dfCountryCanceled)
par(mar=c(2,2,2,2))
library(rworldmap)
worldmap <- joinCountryData2Map(dfCountryCanceled, joinCode="ISO3",
                                nameJoinColumn="Country",)
map<-mapCountryData(worldmap, nameColumnToPlot='IsCanceled',
                    catMethod="logFixedWidth",
                    colourPalette=c('white',
                                    'lightblue','purple','blue','grey','black','red'))
table(Cancellations$MarketSegment)
table(Cancellations$Country) %>% as.data.frame() %>% arrange(desc(Freq))

sapply(Cancellations,function(x) sum(is.na(x)))
sapply(Cancellations,function(x) sum(is.null(x)))
cancellations=as.data.frame(Cancellations)

#==============================================================================#
#                                   CoRRELATION TEST                           #
#==============================================================================#

#Import data from part 1: 

dfm=hoteldata
str(dfm)

#Create local copy of dfm for manipulation 

dfm_temp <- dfm 

#Change 'IsCanceled' back to numeric 

dfm_temp$IsCanceled <- as.numeric(dfm_temp$IsCanceled) 


#Check correlation: 

dfnum = dplyr::select_if(dfm_temp, is.numeric) #select only numeric variables 
dfnum = data.frame(lapply(dfnum, function(x) as.numeric(as.character(x)))) #loop through columns and change factor variables into numeric 
res=cor(dfnum) 
library(corrplot) 
dev.new() 
corrplot(res, method="color", type="upper", tl.col="black") 
#Percentage of cancelled bookings 
table(cancellations$IsCanceled)/nrow(cancellations)
cancelled <- c("is_cancelled","not_cancelled")
percent <- c(70.70,29.30)
cancel_percent <- data.frame(cancelled, percent)
cancel_percent$cancelled <- factor(cancel_percent$cancelled, levels=cancel_percent$cancelled) 
mylabel<-paste(cancel_percent[,2],"%")  
library("ggplot2")
praph1<-ggplot(data=cancel_percent,mapping=aes(x=cancelled,y=percent,fill=cancelled,group=factor(1)))+
  geom_bar(stat="identity")+geom_text(aes(y= 79, x= 1),label="70.70%",size=5)+
  geom_text(aes(y= 35, x= 2),label="29.30%",size=5)+
  labs(x="Cancelled or Not",y="Percentage",title='Percentage of the Cancellation')+
  ylim(0,100)
praph1


#Average lead time for cancelled and not cancelled 
library("RSQLite")
library("sqldf")
a = 'select avg(LeadTime) from cancellations where iscanceled=1'   
cancel_cnt <- sqldf(a)  
print(cancel_cnt)

b = 'select avg(LeadTime) from cancellations where iscanceled=0'   
cancel_cnt <- sqldf(b)  
print(cancel_cnt)

cancelled <- c("is_cancelled","not_cancelled")
percent <- c(93.70,90.55)
cancel_LeadTime <- data.frame(cancelled, percent)
cancel_LeadTime$cancelled <- factor(cancel_LeadTime$cancelled, levels=cancel_LeadTime$cancelled) 
mylabel<-paste(cancel_LeadTime[,2],"%")  
graph2<-ggplot(data=cancel_LeadTime,mapping=aes(x=cancelled,y=percent,fill=cancelled,group=factor(1)))+
  geom_bar(stat="identity")+geom_text(aes(y= 138, x= 1),label="93.70",size=5)+
  geom_text(aes(y= 88, x= 2),label="90.55",size=5)+
  labs(x="Cancelled or Not",y="Lead Time",title='Average lead time for cancelled and not cancelled')+
  ylim(0,150)

graph2

#percentage of special request for cancelled and not cancelled

cancel_SpecialRequest <- data.frame(table(cancellations$TotalOfSpecialRequests,cancellations$IsCanceled))
cancel_SpecialRequest$Is_Canceled <- c("Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled")
graph3<-ggplot(cancel_SpecialRequest,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Special Requests with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.75, x= 2),label="52.34%",size=5)+
  geom_text(aes(y= 0.33, x= 2),label="31.82%",size=5)+
  geom_text(aes(y= 0.10, x= 2),label="12.79%",size=5)+
  geom_text(aes(y= 0.65, x= 1),label="64.88%",size=5)+
  geom_text(aes(y= 0.25, x= 1),label="23.35%",size=5)+
  geom_text(aes(y= 0.07, x= 1),label="10.13%",size=5)
graph3 

#Percentage booking for deposit type for cancellation and no cancellation 
cancel_DepositType <- data.frame(table(cancellations$DepositType,cancellations$IsCanceled))
cancel_DepositType
cancel_DepositType$Is_Canceled <- c("Not_Canceled","Not_Canceled","Not_Canceled","Is_Canceled","Is_Canceled","Is_Canceled")
graph4<-ggplot(cancel_DepositType,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Deposit Type with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.51, x= 2),label="99.35%",size=5)+
  geom_text(aes(y= 0.65, x= 1),label="84.97%",size=5)+
  geom_text(aes(y= 0.07, x= 1),label="14.84%",size=5)
graph4 

#Percentage booking doe type of customer segmentation cancellation and no cancellation
cancel_CustomerType <- data.frame(table(cancellations$CustomerType,cancellations$IsCanceled))
cancel_CustomerType
cancel_CustomerType$Is_Canceled <- c("Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled")
graph4<-ggplot(cancel_CustomerType,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Customer Type with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.975, x= 2),label="5.60%",size=5)+
  geom_text(aes(y= 0.60, x= 2),label="71.85%",size=5)+
  geom_text(aes(y= 0.12, x= 2),label="21.67%",size=5)+
  geom_text(aes(y= 0.07, x= 1),label="13.66%",size=5)+
  geom_text(aes(y= 0.60, x= 1),label="84.66%",size=5)
graph4 
#Country specific cancellation
cancel_Country <- data.frame(table(cancellations$Country,cancellations$IsCanceled))
cancel_Country
cancel_Country$Is_Canceled <- c(1)
b = 'update cancel_Country set Is_Canceled="Is_Canceled"  where Var2=1 ' 
sqldf(c("
       UPDATE cancel_Country
       set Is_Canceled='s_Canceled'
       where Var2=1
       ", "select * from main.cancel_Country"))
cancel_cnt <- sqldf(b)  
print(cancel_cnt)
graph5<-ggplot(cancel_Country,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Customer Type with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.975, x= 2),label="5.60%",size=5)+
  geom_text(aes(y= 0.60, x= 2),label="71.85%",size=5)+
  geom_text(aes(y= 0.12, x= 2),label="21.67%",size=5)+
  geom_text(aes(y= 0.07, x= 1),label="13.66%",size=5)+
  geom_text(aes(y= 0.60, x= 1),label="84.66%",size=5)
graph5 

#percentage of parking space for cancelled and not cancelled
library(ggplot2)
cancel_ParkingSpaces <- data.frame(table(cancellations$RequiredCarParkingSpaces,cancellations$IsCanceled))
cancel_ParkingSpaces
cancel_ParkingSpaces$Is_Canceled <- c("Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Not_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled","Is_Canceled")
graph6<-ggplot(cancel_ParkingSpaces,aes(Is_Canceled,Freq,fill=Var1))+
  geom_bar(stat="identity",position="fill",width = 0.5)+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  labs(y="Percentage",title="Percentage of Parking Spaces with and without cancellation")+
  guides(fill=guide_legend(title=NULL))+
  geom_text(aes(y= 0.55, x= 2),label="81.03%",size=5)+
  geom_text(aes(y= 0.09, x= 2),label="18.87%",size=5)+
  geom_text(aes(y= 0.50, x= 1),label="100%",size=5)
graph6 




cancellations=as.data.frame(Cancellations)
cancellations$IsCanceled=as.factor(cancellations$IsCanceled)
cancellations$Meal=as.factor(cancellations$Meal)
cancellations$Country=as.factor(cancellations$Country)
cancellations$MarketSegment=as.factor(cancellations$MarketSegment)
cancellations$ReservedRoomType=as.factor(cancellations$ReservedRoomType)
cancellations$AssignedRoomType=as.factor(cancellations$AssignedRoomType)
cancellations$DepositType=as.factor(cancellations$DepositType)
cancellations$CustomerType=as.factor(cancellations$CustomerType)

length(sort(unique(c(which(cancellations$Meal=="Undefined"),which(cancellations$Country=="NULL")))))
length(sort(unique(c(which(cancellations$Meal=="Undefined"),which(cancellations$Country=="NULL")))))/nrow(cancellations)



##Regression tre for entire data
set.seed(1)
trainList=createDataPartition(y=cancellations$IsCanceled,p=2/3,list=FALSE)
trainSet=cancellations[trainList,]
testSet=cancellations[-trainList,]



#Regression Tree (all of the variables) to diagnose the important variables.
cartTree=rpart(IsCanceled~.,data=trainSet)
cartTree
prp(cartTree,faclen=0,cex=0.8,extra=1)

varImp(cartTree)

CartPrediction=predict(cartTree,newdata=testSet)
CartPrediction[CartPrediction>0.5]=1
CartPrediction[CartPrediction!=1]=0
confusionMatrix(as.factor(CartPrediction[,2]),testSet$IsCanceled)

Accuracy=confusionMatrix(as.factor(CartPrediction[,2]),testSet$IsCanceled)$overall[1]

CIV=cancellations[,c(1,2,19,10,12,9,4)] #"CIV" stands for CancellationsImportantVariables.

#Linear Model
CIV[,1]=(as.numeric(CIV[,1]))-1

LinearModel=lm(IsCanceled~.,data=CIV)
summary(LinearModel)
LinearModelPrediction=predict(LinearModel,CIV[,-1])
LinearModelPrediction[LinearModelPrediction>0.5]=1
LinearModelPrediction[LinearModelPrediction!=1]=0
confusionMatrix(as.factor(LinearModelPrediction),as.factor(CIV[,1]))

LinearModelAccuracy=confusionMatrix(as.factor(LinearModelPrediction),as.factor(CIV[,1]))$overall[1]

#Regression Trees (important variables only).
#Pre-processing the data so the training doesn't have to do it on the SVM and to have data uniformity across all models.
#The pre-processed data was not used in Linear Regression because it makes the interpretation more complicated with the same results.
CIV2=CIV
CIV2$LeadTime=as.numeric(scale(CIV2$LeadTime))
CIV2$RequiredCarParkingSpaces=as.numeric(scale(CIV2$RequiredCarParkingSpaces))
CIV2$PreviousCancellations=as.numeric(scale(CIV2$PreviousCancellations))
CIV2$StaysInWeekNights=as.numeric(scale(CIV2$StaysInWeekNights))

CIV[,1]=as.factor(CIV[,1]) #Important for the rest
CIV2[,1]=as.factor(CIV2[,1]) #Important for the rest of the models.

library(caret)

set.seed(1)
trainList=createDataPartition(y=CIV2$IsCanceled,p=2/3,list=FALSE)
trainSet=CIV2[trainList,]
testSet=CIV2[-trainList,]

cartTree=rpart(IsCanceled~.,data=trainSet)
cartTree
prp(cartTree,faclen=0,cex=0.8,extra=1)

varImp(cartTree)

CartPrediction=predict(cartTree,newdata=testSet)
CartPrediction[CartPrediction>0.5]=1
CartPrediction[CartPrediction!=1]=0
confusionMatrix(as.factor(CartPrediction[,2]),testSet$IsCanceled)

Accuracy1=confusionMatrix(as.factor(CartPrediction[,2]),testSet$IsCanceled)$overall[1]

c(Accuracy,Accuracy1) #19 predictors (original) vs 6 predictors (important variables).

#Treebag:
library(caret)

set.seed(1)
trainList=createDataPartition(y=CIV2$IsCanceled,p=2/3,list=FALSE)
trainSet=CIV2[trainList,]
testSet=CIV2[-trainList,]

TreebagFit=train(IsCanceled~.,data=trainSet,method="treebag",preProcess=NULL)

varImp(TreebagFit)

TreebagPrediction=predict(TreebagFit,newdata=testSet)
confusionMatrix(as.factor(TreebagPrediction),testSet$IsCanceled)

TreebagAccuracy=confusionMatrix(as.factor(TreebagPrediction),testSet$IsCanceled)$overall[1]

TreebagAccuracy




#Overall model comparison:

ModelComparison=c(LinearModelAccuracy,Accuracy1)
names(ModelComparison)=c("Linear.Regression","Regression.Tree.Cart")
ModelComparison
which.max(ModelComparison)

tabledf <- data.frame(Model = c("Linear Regression Model","CART","Bagtree"),
                      Accuracy_Percent = c(81.19, 82.26,84.16),
                      NIR_Percent = c(72.24, 72.24,72.24),
                      Kappa_Percent = c(47.58,58.77,52.40))
head(tabledf)

