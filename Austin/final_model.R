###################
####Functions######
###################
library(tidyverse)
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

## Mode function ##
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
## Mean function ##
means <- function(x) { mean(x, na.rm = TRUE) }

####################

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


tours$Outbound_Connections <- as.factor(tours$Outbound_Connections)
tours$Return_Connections <- as.factor(tours$Return_Connections)



### recoding date and time variabes
#install.packages("chron")
library(chron)

tours$TourDate <- dates(as.character(tours$TourDate))

tours$Domestic_Depart_Time <- times(tours$Domestic_Depart_Time)
tours$Intr_Depart_Time <- times(tours$Intr_Depart_Time)
tours$Intr_Arrival_Time <- times(tours$Intr_Arrival_Time)
tours$Domestic_Arrival_Time <- times(tours$Domestic_Arrival_Time)



#from WOE consolidation:
tours <- cbind(tours,dat[grep("(Tour_Days_Con|Grp_Size_ratio_Con|Age_Con|State_Con)",names(dat))])
tours$Tour_Days_Con <- as.factor(tours$Tour_Days_Con)
tours$Grp_Size_ratio_Con <- as.factor(tours$Grp_Size_ratio_Con)
tours$Age_Con <- as.factor(tours$Age_Con)
tours$State_Con <- as.factor(tours$State_Con)



#Remove variables that may not be potential predictors! (Check later!)
var.rem<--grep("^(FY|Complaint_Event|Grp_Size_Cat|Grp_Size|Capacity|TD_Overall|Trip_no|Promo_Disc|Return_Connect_Time_Mins_2|Return_Connect_Time_Mins_1|Return_Connect_Time_Mins_2|Eval_Contact_Days|Outbound_Connect_Time_Mins_2|Optionals_2orUnder|Bus_2orUnder|Outbound_Connect_Time_Mins_1|Outbound_Connect_Time_Mins_2|Cus_ID|ProdTour_ID|SalesTourID|Trip_No|HH_ID| Optionals_Avg|Bus_Avg|Meals_Avg|Hotels_Avg|GUSS_Avg|Hotel_2orUnder|Meals_2orUnder|GUSS_2orUnder|TourRegion|Outbound_Connect_Gateway1|Outbound_Connect_Gateway2|Return_Connect_Gateway1|Return_Connect_Gateway2|Start_Day|End_Day|FltGty|Optionals_Avg|Tour_Region|Outbound_Intr_Gateway|Return_Intr_Gateway)$",names(tours))

new.tours<-tours[var.rem]
str(new.tours)
#-1 is missing here so assign na
new.tours$Outbound_Connections[new.tours$Outbound_Connections %in% c(-3,-2,-1,4,5)]<-NA
new.tours$Return_Connections[new.tours$Return_Connections %in% c(-3,-2,-1,4,5)]<-NA


sort(sapply(new.tours, function(x) sum(is.na(x))),decreasing = T)


####################
##Split : 50/25/25##
####################
library(caTools)
set.seed(27947)
split = sample.split(tours$Book_12Mo, SplitRatio = 0.5)
split.valid<-!split
split.test<-!split
split2<-sample.split(tours$Book_12Mo[!split],SplitRatio = 1/2)
split.valid[split.valid==TRUE]= split2
split.test[split.test==TRUE]= !split2

######################
##tree consolidation##
#####################
library(tree.bins)
library(rpart)
consldt<-tours.imp[split,] %>% 
  select(TourCode,Outbound_Domestic_Gateway,Return_Domestic_Gateway,
         Book_12Mo)

binned <- tree.bins(data = consldt, y = Book_12Mo,
                    bin.nm = "Group.", control = rpart.control(cp = .001),
                    return = "new.factors")

#Add consolidated categories for training data to new.tours df
tours["TourCode_Groups"] <- as.factor(binned[[1]]$Categories[match(tours$TourCode,binned[[1]]$TourCode)])
tours["Outbound_Domestic_Gateway_Groups"] <- as.factor(binned[[2]]$Categories[match(tours$Outbound_Domestic_Gateway,binned[[2]]$Outbound_Domestic_Gateway)])
tours["Return_Domestic_Gateway_Groups"] <- as.factor(binned[[3]]$Categories[match(tours$Return_Domestic_Gateway,binned[[3]]$Return_Domestic_Gateway)])



######################
####Transformation ####
######################
tours.xf<-new.tours

#Update#
inp.n <- grep("^(EvalID|Book_12Mo|TourCode|Outbound_Domestic_Gateway|Return_Domestic_Gateway)$", names(tours.xf))
inx3<-inx(tours.xf, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na

#Numeric transformation
library(caret)
TransformParams <- preProcess(tours.xf[split,indx], method=c("YeoJohnson"))
TransformParams$yj
###
summary(predict(TransformParams,tours.xf[split,indx]))


vars.xf <- grep("^(Tour_Days$|Excellent|Good_Hotels$|Good_Meals$|Optionals$)", names(tours.xf))
tours.xf[vars.xf]<-log(tours.xf[vars.xf]+1)

#names(tours.xf[vars.xf])
library(Amelia)
#missmap(tours.xf)
missmap(tours.xf[index.na])
sort(sapply(tours.xf, function(x) sum(is.na(x))),decreasing = T)

#just remove the variables that were consolidated!!!
tours.xf[c("State","TourCode","Outbound_Domestic_Gateway","Return_Domestic_Gateway")]=NULL


######################
##### Imputation #####
######################

tours.imp <- tours.xf


#Routine Update#
inp.n <- grep("^(EvalID|Book_12Mo)$",names(tours.imp))
inx3<-inx(tours.imp, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
#####################


names(tours.imp)[index.na==T]
summary(tours.imp[index.na])


tours.imp <- tours


# numeric impute: By mean #
Mean<-sapply(tours.imp[split,indx],means)##remove in scoring already saved!!
tours.imp[indx]<-as.data.frame(mapply(impute,x=tours.imp[indx],y = Mean))

# Nominal Input: By Mode #
Mode<-sapply(tours.imp[split, index.cat],mode)
tours.imp[index.cat]<-as.data.frame(mapply(impute,x=tours.imp[index.cat],y = Mode))



#Time variables back to "times" format
time.vars <- grep("Time$",names(tours.imp))
tours.imp[time.vars] <- lapply(tours.imp[time.vars],times)

summary(tours.imp[time.vars])






#State divisions
tours.imp$StateDivision <- state.division[match(tours.imp$State,state.abb)]
levels(tours.imp$StateDivision) <- c(levels(tours.imp$StateDivision),"Other")
tours.imp$StateDivision <- mapply(impute,x=tours.imp$StateDivision,y = "Other")


#adding in DC
tours.imp[tours.imp$State=="DC",]$StateDivision <- "South Atlantic"




#######################
#### Random Forest #### internal downsampling
#######################

tours.rf<-tours.imp
str(tours.rf)
vars1 <- -grep("^(EvalID)$",names(tours.rf))


minor<-unname(summary(tours.rf$Book_12Mo[split])[2])


library(randomForest)
set.seed(27947)

m.best<-12



################################
#### Parameter Tuning: mtry ####
################################

m<-round(seq(5,30,by=8))
m<-seq(6,20,by=3)
fscore.seq<-numeric()

for(i in 1:length(m)){
  set.seed(27947)
  rf <- randomForest(Book_12Mo~., data=tours.rf[split,vars1],
                     ntree = 200,
                     strata= tours.rf$Book_12Mo[split],
                     sampsize=c(minor,minor),
                     importance =TRUE,
                     mtry=m[i])
  rf.class<- predict(rf, newdata=tours.rf[split.valid,], type="response")
  fscore.seq[i]<-confusionMatrix(table(rf.class,tours.rf$Book_12Mo[split.valid]),
                                 positive = "1")$byClass["F1"]
  cat(i,"/",length(m),": Finished making forest #",i," with mtry =",m[i],"and F1 =",round(fscore.seq[i],3))
  cat("\n")
}

dev.off()
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
RF <- randomForest(Book_12Mo~., data=tours.rf[split,vars1],
                   ntree = 200, 
                   strata= tours.rf$Book_12Mo[split], 
                   sampsize=c(minor,minor),
                   importance =TRUE,mtry=m.best,#from below
                   do.trace=TRUE)
print(RF)
plot(RF)  



# Make predictions #
library(caret)
RF.class<- predict(RF, newdata=tours.rf[split.test,], type="response")
confusionMatrix(table(RF.class,tours.rf[split.test,]$Book_12Mo),positive = "1")
fscore<-confusionMatrix(table(RF.class,tours.rf[split.test,]$Book_12Mo),
                        positive = "1")$byClass["F1"]  
fscore #0.55679

RF$importance
varImpPlot(RF) 


confusionMatrix(table(RF.class,tours.rf[split.test,]$Book_12Mo),positive = "1")





#######
## Saving to use on the scoring data
save(binned, Mean, Mode,  RF, file="myscore_raw.RData")
