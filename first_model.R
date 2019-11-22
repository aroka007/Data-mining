tours<-read.csv("modeling_data.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))
str(tours)


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


### recoding date and time variabes
#install.packages("chron")
library("chron")

tours$TourDate <- dates(as.character(tours$TourDate))

tours$Domestic_Depart_Time <- times(tours$Domestic_Depart_Time)
tours$Intr_Depart_Time <- times(tours$Intr_Depart_Time)
tours$Intr_Arrival_Time <- times(tours$Intr_Arrival_Time)
tours$Domestic_Arrival_Time <- times(tours$Domestic_Arrival_Time)

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

vars <- -grep("(ID|Trip_no)",names(tours))



####################################
####### Consolidation Tree #########
####################################

library(rpart)
library(partykit)

#TourCode
tree <- rpart(formula = Book_12Mo ~ Tour_Region, data=tours[,vars], control=rpart.control(cp=-1, minbucket=2000))
print(tree)
plot(as.party(tree))





#TourCode
tree <- rpart(formula = Book_12Mo ~ TourCode, data=tours[,vars], control=rpart.control(cp=0.001))
print(tree)
plot(as.party(tree))


nodes <- tree$where
table(nodes)

Grp1 <- names(which(table(tours[nodes == 2,]$TourCode)>0))
Grp2 <- names(which(table(tours[nodes == 4,]$TourCode)>0))
Grp3 <- names(which(table(tours[nodes == 6,]$TourCode)>0))
Grp4 <- names(which(table(tours[nodes == 7,]$TourCode)>0))


TourCode_split <- as.factor(ifelse(tours$TourCode %in% Grp1, 1, 
                         ifelse(tours$TourCode %in% Grp2, 2, 
                                ifelse(tours$TourCode %in% Grp3, 3, 
                                       ifelse(tours$TourCode %in% Grp4, 4, 0)))))


sort(prop.table(table(TourCode_split,tours$Book_12Mo),1)[,2])*100


tours <- cbind(tours,TourCode_split)


######################
##### Imputation #####
######################

#Routine Update#
inp.n <- grep("(ID|Trip_no|Book_12Mo|TourDate|Arrival|Depart_Time)",names(tours))
inx3<-inx(tours, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
#####################

impute <- function(x,y) {
  x[is.na(x)]<-y
  x
}

# Nominal Input: By Mode #
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

names(tours)[index.na==T]
summary(tours[index.na])

tours.imp <- tours

#Mean<-sapply(data.imp[split.down,indx],means)
#Mode<-sapply(tours.imp[split,index.cat],mode)



#data.imp[indx]<-as.data.frame(mapply(impute,x=data.imp[indx],y = Mean))



#tours.imp$Intr_Arrival_Time <- mapply(impute,x=tours.imp,y = mean(tours.imp$Intr_Arrival_Time,na.rm=TRUE))
tours.imp$SourceType <- mapply(impute,x=tours.imp$SourceType,y = "Referral")
tours.imp$Email <- mapply(impute,x=tours.imp$Email,y = "Available")





library(caTools)
set.seed(27947)
split = sample.split(tours$Book_12Mo, SplitRatio = 0.5)  





#########################
##Variable Importance!!!###
##########################
library(Amelia,quietly = T)
library(caret,quietly = T)

# nominal input - chi-square
chi2pvalues<- sapply(tours[index.cat], 
                     function(x) chisq.test(x, tours$Book_12Mo)$p.value)
sort(chi2pvalues)

# numeric input - t stat
tTestpvalues<-sapply(tours[indx], 
                     function(x) t.test(x ~ tours$Book_12Mo)$p.value)
sort(tTestpvalues)

#ROC
rocValues<- filterVarImp(x = tours[,vars], y = tours$Book_12Mo)
rocValues[order(-rocValues$X1),]

#Past trips most important!!#




#####################################
####### Logistic Regression #########
#####################################

tours.mdl<-tours.imp

library(pROC)
library(caret)
#vars <- -grep("^(EvalID|Cus_ID|ProdTour_ID|SalesTourID|Trip_no|HH_ID)$",names(tours.mdl))
vars <- -grep("(ID|Trip_no|Poor|Fair|Good|Excellent|TourDate|Arrival|Depart_Time|Promo|State|TourCode|Gateway|Region)",names(tours.mdl))

#vars <- grep('(Email|Book_Months|Past_Trips|Overall_Impression|Age|TourCode_split|Pax_Category$)',names(tours.mdl))



full = glm(tours.mdl[split,]$Book_12Mo ~., family=binomial, data=tours.mdl[split, vars])
summary(full)

null<-glm(tours.mdl[split,]$Book_12Mo ~1, family=binomial, data=tours.mdl[split, vars])



#######
####
#using top 20 from variable importance
top20vars <- rownames(rocValues[order(-rocValues$X1),][1:20,])
top20vars
full = glm(tours.mdl[split,]$Book_12Mo ~ ., family = binomial, data = tours.mdl[split,top20vars])
summary(full)

null<-glm(tours.mdl[split,]$Book_12Mo ~1, family=binomial, data=tours.mdl[split, top20vars])




# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = TRUE)
summary(reg.step)
reg.step.prob<-predict(reg.step,tours.mdl[!split, top20vars], type = "response")


# Use alternative cutoff
rocCurve.reg <- roc(tours.imp[!split,]$Book_12Mo, reg.step.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft", transpose = FALSE)
reg.class <- as.factor(ifelse(reg.step.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(table(reg.class,tours.imp[!split,]$Book_12Mo),
                            positive = "1")$byClass["F1"]

reg.fscore

reg.results <- confusionMatrix(table(reg.class,tours[!split,]$Book_12Mo),
                positive = "1", mode= "everything")
reg.results




#####################
####### ANN  ########
#####################

tours.ann <- tours.imp
vars.ann<-attr(terms(reg.step), "term.labels") # extract variable names from step model
#vars.ann <- vars



## Standardization ## 
library(caret)
ScaleParams <- preProcess(tours.ann[split, vars.ann], method=c("center", "scale"))
tours.ann[vars.ann]<-predict(ScaleParams, tours.ann[vars.ann])

## Hot encoding ##
dummy <- dummyVars( ~ ., data = tours.ann[split, vars.ann], fullRank = TRUE)
tours.ann.encode<-as.data.frame(predict(dummy, tours.ann[vars.ann])) 
tours.ann.encode$Book_12Mo<-tours.ann$Book_12Mo


## Prepare train/validation sets as matrices ##
inp.n <- grep("^(Book_12Mo)", names(tours.ann.encode)) 

x.train <- as.matrix(tours.ann.encode[split,-inp.n])
y.train<- as.matrix(tours.ann.encode[split,"Book_12Mo"])
x.valid<-as.matrix(tours.ann.encode[!split,-inp.n])
y.valid<-as.matrix(tours.ann.encode[!split,"Book_12Mo"])


library(keras)
library(tensorflow)
use_session_with_seed(27947)
ann <- keras_model_sequential() 
ann %>%
  layer_dense(units = 6, activation = "tanh", input_shape = ncol(x.train)) %>%   # update input shape
  layer_dense(units = 1, activation = "sigmoid")




ann %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

callbacks.list = list(
  callback_model_checkpoint(filepath="my_ann_ex7.h5", 
                            monitor = "val_loss", 
                            save_best_only = TRUE
  ))


history <- ann %>% fit(
  x= x.train,
  y= y.train,
  epochs = 40,
  validation_data = list(x.valid,y.valid),
  verbose = 1,
  callbacks = callbacks.list
)


ann.select<- load_model_hdf5("my_ann_ex7.h5") 



## Prediction ##
ann.prob<-predict_proba(ann.select,x.valid)

# Use alternative cutoff
rocCurve.ann <- roc(tours[!split,]$Book_12Mo, ann.prob, quiet=TRUE)
annThresh <-  coords(rocCurve.ann, x = "best", best.method = "closest.topleft", transpose = FALSE)
ann.class <- as.factor(ifelse(ann.prob >= annThresh$threshold, 1,0))
ann.fscore<-confusionMatrix(table(ann.class,tours[!split,]$Book_12Mo),
                            positive = "1")$byClass["F1"]

ann.fscore  # f-score=0.5125

ann.results <- confusionMatrix(table(ann.class,tours[!split,]$Book_12Mo),
                  positive = "1", mode= "everything")


ann.results



reg.results # f-score=0.5132
reg.fscore
