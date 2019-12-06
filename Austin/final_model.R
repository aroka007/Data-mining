###################
####Functions######
###################
inx <- function (tours, inp.n) { # data: current dataframe; inp.n: position for non-inputs
  # numeric input indicator
  indx <- sapply(tours, is.numeric)
  indx[inp.n]<-FALSE
  
  # nominal input indicator
  index.cat<-sapply(tours, is.factor)
  index.cat[inp.n]<-FALSE
  
  # missing value indicator
  index.na<-sapply(tours,function(x) any(is.na(x)))
  index.na[inp.n]<-FALSE
  
  data.frame(indx, index.cat, index.na)
}

impute <- function(x,y) {
  x[is.na(x)]<-y
  x
}

# Nominal Input: By Mode #
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
## Mean function ##
means <- function(x) { mean(x, na.rm = TRUE) }




tours<-read.csv("modeling_data.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))
#str(tours)
tours$Book_12Mo<-as.factor(tours$Book_12Mo)

#Turn inputs into factors:
tours$Recommend_GAT<-as.factor(tours$Recommend_GAT)
tours$TravelAgain<-as.factor(tours$TravelAgain)
tours$Groups_Interest<-as.factor(tours$Groups_Interest)
tours$Reference<-as.factor(tours$Reference)

tours$Overall_Impression<-factor(tours$Overall_Impression,ordered = T)
tours$Pre_Departure<-factor(tours$Pre_Departure,ordered = T)
tours$Flight_Itin<-factor(tours$Flight_Itin,ordered=T)
tours$TD_Overall<-factor(tours$TD_Overall,ordered = T)

tours$Extension<-as.factor(tours$Extension)
tours$Insurance<-as.factor(tours$Insurance)
tours$FltGty<-as.factor(tours$FltGty)
tours$Complaint_Event<-as.factor(tours$Complaint_Event)
tours$Voucher_Event<-as.factor(tours$Voucher_Event)


#tours$Outbound_Connections <- as.factor(tours$Outbound_Connections)
#tours$Return_Connections <- factor(tours$Return_Connections,exclude=c(-3,-1,5,4))



### recoding date and time variabes
#install.packages("chron")
library("chron")

tours$TourDate <- dates(as.character(tours$TourDate))

tours$Domestic_Depart_Time <- times(tours$Domestic_Depart_Time)
tours$Intr_Depart_Time <- times(tours$Intr_Depart_Time)
tours$Intr_Arrival_Time <- times(tours$Intr_Arrival_Time)
tours$Domestic_Arrival_Time <- times(tours$Domestic_Arrival_Time)




#from WOE consolidation:
tours[94:97] <- dat[94:97]
tours$Tour_Days_Con <- as.factor(tours$Tour_Days_Con)
tours$Grp_Size_ratio_Con <- as.factor(tours$Grp_Size_ratio_Con)
tours$Age_Con <- as.factor(tours$Age_Con)
#tours$State_Con <- as.factor(tours$State_Con)


#Split : 50/25/25
library(caTools)
set.seed(27947)
split = sample.split(tours$Book_12Mo, SplitRatio = 0.5)
split.valid<-!split
split.test<-!split
split2<-sample.split(tours$Book_12Mo[!split],SplitRatio = 1/2)
split.valid[split.valid==TRUE]= split2
split.test[split.test==TRUE]= !split2




######################
##### Imputation #####
######################

#Routine Update#
vars <- -grep("^(Grp_Size|Grp_Size_Cat|Capacity|Grp_Size_Ratio|EvalID|Cus_ID|ProdTour_ID|SalesTourID|HH_ID|Trip_No|FY|Complaint_Event|Tour_Region|TD_Overall|Trip_no|Promo_Disc|Domestic_Depart_Time|Return_Connect_Time_Mins_1|Return_Connect_Time_Mins_2|Eval_Contact_Days|Outbound_Connect_Time_Mins_1|Outbound_Connect_Time_Mins_2|Return_Connections|Optionals_Avg|Bus_Avg|Meals_Avg|Hotels_Avg|GUSS_Avg|Hotel_2orUnder|Meals_2orUnder|GUSS_2orUnder|Optionals_2orUnder|Bus_2orUnder|Start_Day|End_Day|FltGty|Outbound_Domestic_Gateway|Outbound_Intr_Gateway|Outbound_Connect_Gateway1|Outbound_Connect_Gateway2|Return_Intr_Gateway|Return_Domestic_Gateway|Return_Connect_Gateway1|Return_Connect_Gateway2|State|TourCode)$",names(tours.imp))
inx3<-inx(tours, vars) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
#####################


names(tours)[index.na==T]
summary(tours[index.na])


tours.imp <- tours

#Mean<-sapply(data.imp[split.down,indx],means)
#Mode<-sapply(tours.imp[split,index.cat],mode)

#data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))


#Time variables
time.vars <- grep("Time$",names(tours))
time.means <- times(sapply(tours.imp[split,time.vars],means))
for (i in 1:4){
  tours.imp[time.vars[i]][is.na(tours.imp[time.vars[i]])] <- time.means[i]
}
#summary(tours.imp[time.vars])



#tours.imp$Outbound_Connections <- mapply(impute,x=tours.imp$Outbound_Connections, y = "Referral")
#tours.imp$Return_Connections <- mapply(impute,x=tours.imp$Return_Connections, y = "Referral")




tours.imp$SourceType <- mapply(impute,x=tours.imp$SourceType,y = "Referral")
tours.imp$Email <- mapply(impute,x=tours.imp$Email,y = "Available")
tours.imp$Outbound_Connections[tours.imp$Outbound_Connections==-1]<-0


tours.imp$Outbound_Domestic_Gateway <- mapply(impute,x=tours.imp$Outbound_Domestic_Gateway,y = "NYC")
tours.imp$Return_Domestic_Gateway <- mapply(impute,x=tours.imp$Return_Domestic_Gateway,y = "NYC")




#State regions
tours.imp$StateRegion <- state.region[match(tours.imp$State,state.abb)]
levels(tours.imp$StateRegion) <- c(levels(tours.imp$StateRegion),"Other")
tours.imp$StateRegion <- mapply(impute,x=tours.imp$StateRegion,y = "Other")

#State divisions
tours.imp$StateDivision <- state.division[match(tours.imp$State,state.abb)]
levels(tours.imp$StateDivision) <- c(levels(tours.imp$StateDivision),"Other")
tours.imp$StateDivision <- mapply(impute,x=tours.imp$StateDivision,y = "Other")

#adding in DC
tours.imp[tours.imp$State=="DC",]$StateRegion <- "South"
tours.imp[tours.imp$State=="DC",]$StateDivision <- "South Atlantic"


####################################
####### Consolidation ##############
####################################



library(tree.bins)
library(rpart)
library(dplyr)
consldt<-tours.imp[split,] %>% 
  select(TourCode,Outbound_Domestic_Gateway,Return_Domestic_Gateway,
         Book_12Mo)

binned <- tree.bins(data = consldt, y = Book_12Mo,
                    bin.nm = "Group.", control = rpart.control(cp = .001),
                    return = "new.factors")

#Add consolidated categories for training data to new.tours df
tours.imp["TourCode_Groups"] <- as.factor(binned[[1]]$Categories[match(tours.imp$TourCode,binned[[1]]$TourCode)])
tours.imp["Outbound_Domestic_Gateway_Groups"] <- as.factor(binned[[2]]$Categories[match(tours.imp$Outbound_Domestic_Gateway,binned[[2]]$Outbound_Domestic_Gateway)])
tours.imp["Return_Domestic_Gateway_Groups"] <- as.factor(binned[[3]]$Categories[match(tours$Return_Domestic_Gateway,binned[[3]]$Return_Domestic_Gateway)])





vars <- -grep("^(Grp_Size|Grp_Size_Cat|Capacity|Grp_Size_Ratio|EvalID|Cus_ID|ProdTour_ID|SalesTourID|HH_ID|Trip_No|FY|Complaint_Event|Tour_Region|TD_Overall|Trip_no|Promo_Disc|Return_Connect_Time_Mins_1|Return_Connect_Time_Mins_2|Eval_Contact_Days|Outbound_Connect_Time_Mins_1|Outbound_Connect_Time_Mins_2|Optionals_Avg|Bus_Avg|Meals_Avg|Hotels_Avg|GUSS_Avg|Hotel_2orUnder|Meals_2orUnder|GUSS_2orUnder|Optionals_2orUnder|Bus_2orUnder|Start_Day|End_Day|FltGty|Outbound_Domestic_Gateway|Outbound_Intr_Gateway|Outbound_Connect_Gateway1|Outbound_Connect_Gateway2|Return_Intr_Gateway|Return_Domestic_Gateway|Return_Connect_Gateway1|Return_Connect_Gateway2|State|TourCode)$",names(tours.imp))

names(tours.imp[vars])


#######################
#### Random Forest #### internal downsampling
#######################

tours.rf<-tours.imp
str(tours.rf)

minor<-unname(summary(tours.rf$Book_12Mo[split])[2])


library(randomForest)
set.seed(27947)

m.best<-12



################################
#### Parameter Tuning: mtry ####
################################

m<-round(seq(5,30,length.out=8))
m<-seq(6,11)
fscore.seq<-numeric()

for(i in 1:length(m)){
  set.seed(27947)
  rf <- randomForest(Book_12Mo~., data=tours.rf[split,vars],
                     ntree = 200,
                     strata= tours.rf$Book_12Mo[split],
                     sampsize=c(minor,minor),
                     importance =TRUE,
                     mtry=m[i],
                     do.trace=TRUE)
  
  rf.class<- predict(rf, newdata=tours.rf[split.valid,], type="response")
  fscore.seq[i]<-confusionMatrix(table(rf.class,tours.rf$Book_12Mo[split.valid]),
                                 positive = "1")$byClass["F1"]
}

plot(m, fscore.seq, pch=19 , col="blue", type="b",
     ylab="F-score",xlab="Number of Predictors considered at each split")
#
#
m.best<- m[which.max(fscore.seq)] #8
max(fscore.seq)
#0.55679


m.best=8

#######################
#### Random Forest ####
#######################
set.seed(27947)
RF <- randomForest(Book_12Mo~., data=tours.rf[split,vars],
                   ntree = 200, 
                   strata= tours.rf$Book_12Mo[split], 
                   sampsize=c(minor,minor),
                   importance =TRUE,mtry=m.best,#from below
                   do.trace=TRUE)
print(RF)
plot(RF)  



# Make predictions #
library(caret)
RF.class<- predict(RF, newdata=tours.rf[split.valid,], type="response")
confusionMatrix(table(RF.class,tours.rf[split.valid,]$Book_12Mo),positive = "1")
fscore<-confusionMatrix(table(RF.class,tours.rf[split.valid,]$Book_12Mo),
                        positive = "1")$byClass["F1"]  
fscore #0.55679

RF$importance
varImpPlot(RF) 





confusionMatrix(table(RF.class,tours.rf[split.valid,]$Book_12Mo),positive = "1")







#######
## Saving to use on the scoring data
save(binned, reg.step,  regThresh, file="myscore_raw.RData")



