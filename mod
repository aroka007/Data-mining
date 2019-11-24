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
#Mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Mean function ##
means <- function(x) { mean(x, na.rm = TRUE) }

tours<-read.csv("Data mining 8480/modeling_data (1).csv", header=TRUE, na.strings=c(".", "NA", "", "?"))
#str(tours)
tours$Book_12Mo<-as.factor(tours$Book_12Mo)
#levels(tours$Book_12Mo)<-c("2","1")#not booked is 2

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

#Remove variables that may not be potential predictors! (Check later!)
var.rem<--grep("^(Complaint_Event|TD_Overall|Trip_no|Promo_Disc|Return_Connect_Time_Mins_2|Return_Connect_Time_Mins_1|Return_Connect_Time_Mins_2|Eval_Contact_Days|Outbound_Connect_Time_Mins_2|Optionals_2orUnder|Bus_2orUnder|Return_Connections|Domestic_Depart_Time|Outbound_Connect_Time_Mins_1|Outbound_Connect_Time_Mins_2|Domestic_Arrival_Time|Intr_Arrival_Time|Intr_Depart_Time|Cus_ID|ProdTour_ID|SalesTourID|Trip_No|HH_ID| Optionals_Avg|Bus_Avg|Meals_Avg|Hotels_Avg|GUSS_Avg|Hotel_2orUnder|Meals_2orUnder|GUSS_2orUnder|TourRegion|Outbound_Connect_Gateway2|Return_Connect_Gateway2|Start_Day|End_Day|FltGty|Domestic_Depart_Time|Optionals_Avg|Tour_Region)$",names(tours))

new.tours<-tours[var.rem]
str(new.tours)
#-1 is missing here so assign na
new.tours$Outbound_Connections[new.tours$Outbound_Connections==-1]<-NA

sort(sapply(new.tours, function(x) sum(is.na(x))),decreasing = T)

#Split : 50/25/25
library(caTools)
set.seed(27947)
split = sample.split(new.tours$Book_12Mo, SplitRatio = 0.5)
split.valid<-!split
split.test<-!split
split2<-sample.split(new.tours$Book_12Mo[!split],SplitRatio = 1/2)
split.valid[split.valid==TRUE]= split2
split.test[split.test==TRUE]= !split2

#$ Outbound_Intr_Gateway       : Factor w/ 62 levels " **
#$ Return_Intr_Gateway         : Factor w/ 79 levels "  **
#State and tour region use WOE

#consolidate using training data
consldt<-new.tours[split,] %>% 
  select(TourCode,TourDate,Outbound_Domestic_Gateway,
         Outbound_Connect_Gateway1,Return_Connect_Gateway1,Return_Domestic_Gateway,
         Book_12Mo)
library(tree.bins)
binned <- tree.bins(data = consldt, y = Book_12Mo,
                    bin.nm = "Group.", control = rpart.control(cp = .001),
                    return = "new.factors") 
####################################
######### CHECK############
####################################

#Add consolidated categories for training data to new.tours df
new.tours[split,"TourCode_Groups"] <-as.factor(binned[[1]]$Categories[match(new.tours[split,"TourCode"],binned[[1]]$TourCode)])# ###########################################################################################
new.tours[split,"TourDate_Groups"] <- as.factor(binned[[2]]$Categories[match(new.tours[split,"TourDate"],binned[[2]]$TourDate)])# ###########################################################################################
new.tours[split,"Outbound_Domestic_Gateway_Groups"] <- as.factor(binned[[3]]$Categories[match(new.tours[split,"Outbound_Domestic_Gateway"],binned[[3]]$Outbound_Domestic_Gateway)])# ###########################################################################################
new.tours[split,"Outbound_Connect_Gateway1_Groups"] <- as.factor(binned[[4]]$Categories[match(new.tours[split,"Outbound_Connect_Gateway1"],binned[[4]]$Outbound_Connect_Gateway1)])# ###########################################################################################
new.tours[split,"Return_Connect_Gateway1_Groups"] <- as.factor(binned[[5]]$Categories[match(new.tours[split,"Return_Connect_Gateway1"],binned[[5]]$Return_Connect_Gateway1)])# ###########################################################################################
new.tours[split,"Return_Domestic_Gateway_Groups"] <- as.factor(binned[[6]]$Categories[match(new.tours[split,"Return_Domestic_Gateway"],binned[[6]]$Return_Domestic_Gateway)])# ###########################################################################################

str(new.tours)
########################################################################################
####################################
######### Decision Tree ############
####################################
set.seed(27947)
library(rpart)
var1<--grep("^(EvalID|TourCode|TourDate|Outbound_Domestic_Gateway|Outbound_Connect_Gateway1|Return_Connect_Gateway1|Return_Domestic_Gateway)$",names(new.tours))
Dtree<- rpart(Book_12Mo~.,data = new.tours[split,var1], control = rpart.control(cp=.001))

library(pROC)
library(caret) 
cp.seq=Dtree$cptable[,1]
fscore<-numeric()
fscore[1]<-0  # Set root node F-score zero
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(Dtree, cp=cp.seq[i]), new.tours[split2,var1],type="prob")[,2] 
  rocCurve.tree <- roc(new.tours[split2,]$Book_12Mo, tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft", transpose = FALSE)
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(table(tree.class,new.tours[split2,var1]$Book_12Mo),
                             positive = "1")$byClass["F1"]
}

plot(Dtree$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")
#tree$cptable
# Final model
tree.final=prune(Dtree,cp=cp.seq[fscore==max(fscore)]) #0.5067185
library(partykit)
# plot(as.party(tree.final))
# tree.final$variable.importance

## Model Evaluation: Lift Graph ##
test.prob<-predict(tree.final, new.tours[!split2,var1], type = "prob")[,2]
evaluate.prob <- predict(tree.final, new.tours[split2,var1], type = "prob")[,2]
train.prob <- predict(tree.final, new.tours[split,var1], type = "prob")[,2]

library(ROCR)
pred.test<-prediction(test.prob, new.tours[!split2,var1]$Book_12Mo)
pred.eva <- prediction(evaluate.prob, new.tours[split2,var1]$Book_12Mo)
pred<-prediction(train.prob, new.tours[split,var1]$Book_12Mo)

perf.test<-performance(pred.test,"lift","rpp")
perf.eva <- performance(pred.eva,"lift","rpp")
perf <- performance(pred,"lift","rpp")

plot(perf, col='blue', type="b", main="Lift Curve")
plot(perf.eva, col= 'red', type="b",add = TRUE,main="Lift Curve")
plot(perf.test, col= 'green', type="b",add = TRUE,main="Lift Curve")


legend('topright', legend=c('train', 'validation','test'), col=c("blue","red","green"),lty=c(1,1))
#######################################################################################

######################
####Transformation ####
######################
tours.xf<-new.tours

#Update#
inp.n <- grep("^(EvalID|Book_12Mo|TourCode|TourDate|Outbound_Domestic_Gateway|Outbound_Connect_Gateway1|Return_Connect_Gateway1|Return_Domestic_Gateway)$", names(tours.xf))
inx3<-inx(tours.xf, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na

#Numeric transformation
library(caret)
TransformParams <- preProcess(tours.xf[split,indx], method=c("YeoJohnson"))
TransformParams$yj
###
vars.xf <- grep("^(Tour_Days$|Grp_Size$|Capacity$|Excellent|Good_Hotels$|Good_Meals$|Optionals$)", names(tours.xf))
tours.xf[vars.xf]<-log(tours.xf[vars.xf]+1)

#names(tours.xf[vars.xf])
library(Amelia)
#missmap(tours.xf)
sort(sapply(tours.xf, function(x) sum(is.na(x))),decreasing = T)

######################
##### Imputation #####
######################
any(is.na(tours$Age))
#Update#
tours.imp<-tours.xf
#removed them bcuz already have consolidated categories column for them plus we only impute using training data
tours.imp[c("TourCode","TourDate","Outbound_Domestic_Gateway","Outbound_Connect_Gateway1","Return_Connect_Gateway1","Return_Domestic_Gateway")]=NULL

str(tours.imp)
#sort(sapply(tours.imp, function(x) sum(is.na(x))),decreasing = T)
#1 numeric missing-outbound connections other factors

inp.n <- grep("^(EvalID|Books_12Mo)", names(tours.xf))
inx3<-inx(tours.imp, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na

# check missing data
names(tours.imp)[index.na==TRUE]

# numeric impute: By mean #
Mean<-sapply(tours.imp[split,indx],means)##remove in scoring already saved!!
tours.imp[indx]<-as.data.frame(mapply(impute,x=tours.imp[indx],y = Mean))

# Nominal Input: By Mode #
Mode<-sapply(tours.imp[split, index.cat],mode)
tours.imp[index.cat]<-as.data.frame(mapply(impute,x=tours.imp[index.cat],y = Mode))

#missmap(tours.imp)
#######
# create missing value flag #
tours.imp[paste(names(tours.imp)[index.na], "NA", sep=".")] <- ifelse(
  is.na(tours.xf[index.na]), 1, 0)

tours.imp[grep("NA$",names(tours.imp))]<-lapply(
  tours.imp[grep("NA$",names(tours.imp))], as.factor) 


#sort(sapply(tours.imp, function(x) sum(is.na(x))),decreasing = T)

########################################
######### Logistic Regression ##########
########################################


tours.mdl<-tours.imp
vars<--grep("^(EvalID|Email.NA|Outbound_Intr_Gateway.NA|Outbound_Connections.NA|Return_Intr_Gateway.NA|Outbound_Domestic_Gateway_Groups.NA|TourCode_Groups.NA|Return_Connect_Gateway1_Groups.NA|Return_Domestic_Gateway_Groups.NA)$",names(tours.imp))

levels(tours.mdl$Book_12Mo)

# Build full model( . means using all inputs)
full <- glm(Book_12Mo ~., family=binomial, data=tours.mdl[split, vars])
# summary(full) 
# Set up null model
null<-glm(Book_12Mo ~1, family=binomial, data=tours.mdl[split, vars])
n<-sum(split)

#Stepwise
reg.step <- step(null, scope=formula(full), direction="both",trace=FALSE)
# Validation F-score
reg.step.prob<-predict(reg.step,tours.mdl[split2, vars], type = "response") 
rocCurve.reg <- roc(tours.mdl[split2,]$Book_12Mo, reg.step.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft", transpose = FALSE)
#regThresh
reg.class <- as.factor(ifelse(reg.step.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(table(reg.class,new.tours[split2,vars]$Book_12Mo),
                            positive = "1")$byClass["F1"]
reg.fscore #bic f score=0.5167822  using aic=0.518996 

#Backwards
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
# Validation F-score
reg.bwd.prob<-predict(reg.bwd,tours.mdl[split2, vars], type = "response") 
rocCurve.reg <- roc(tours.mdl[split2,]$Book_12Mo, reg.bwd.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft", transpose = FALSE)
#regThresh
reg.class <- as.factor(ifelse(reg.bwd.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(table(reg.class,new.tours[split2,vars]$Book_12Mo),
                            positive = "1")$byClass["F1"]
reg.fscore 

#Forward
reg.fwd <- step(null, scope=formula(full), direction="forward",trace=FALSE)
# Validation F-score
reg.fwd.prob<-predict(reg.fwd,tours.mdl[split2, vars], type = "response") 
rocCurve.reg <- roc(tours.mdl[split2,]$Book_12Mo, reg.fwd.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft", transpose = FALSE)
#regThresh
reg.class <- as.factor(ifelse(reg.fwd.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(table(reg.class,new.tours[split2,vars]$Book_12Mo),
                            positive = "1")$byClass["F1"]
reg.fscore   


















