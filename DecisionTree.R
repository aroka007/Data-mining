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

vars <- -grep("^(EvalID|Cus_ID|ProdTour_ID|SalesTourID|Trip_no|HH_ID)$",names(tours))


#Routine Update#
inp.n <- grep("^(EvalID|Cus_ID|ProdTour_ID|SalesTourID|Trip_no|HH_ID|Book_12Mo)$",names(tours))
inx3<-inx(tours, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
#####################


### recoding date and time variabes
#install.packages("chron")
library("chron")

tours$TourDate <- dates(as.character(tours$TourDate))

tours$Domestic_Depart_Time <- times(tours$Domestic_Depart_Time)
tours$Intr_Depart_Time <- times(tours$Intr_Depart_Time)
tours$Intr_Arrival_Time <- times(tours$Intr_Arrival_Time)
tours$Domestic_Arrival_Time <- times(tours$Domestic_Arrival_Time)






##adding TourCode, "Gateways", and State to list of excluded variables
#vars <- -grep("(EvalID|Cus_ID|ProdTour_ID|SalesTourID|Trip_no|HH_ID|TourCode|Gateway|State)",names(tours))





#proportion tables (show the same as the mosaic plots)
prop.table(table(tours$Age,tours$Book_12Mo),1)
prop.table(table(tours$Past_Trips,tours$Book_12Mo),1)
prop.table(table(tours$Email,tours$Book_12Mo),1)
prop.table(table(tours$TravelAgain,tours$Book_12Mo),1)
prop.table(table(tours$DB_Enter_Months,tours$Book_12Mo),1)

# Percentages of "booked" in each category
sort(prop.table(table(tours$TravelAgain,tours$Book_12Mo),1)[,2])*100
sort(prop.table(table(tours$Email,tours$Book_12Mo),1)[,2])*100
sort(prop.table(table(tours$DB_Enter_Months,tours$Book_12Mo),1)[,2])*100
sort(prop.table(table(tours$Age,tours$Book_12Mo),1)[,2])*100
sort(prop.table(table(tours$TourCode,tours$Book_12Mo),1)[,2])*100
sort(prop.table(table(tours$Outbound_Domestic_Gateway,tours$Book_12Mo),1)[,2])*100




##adding TourCode, "Gateways", and State to list of excluded variables
#vars <- -grep("(EvalID|Cus_ID|ProdTour_ID|SalesTourID|Trip_no|HH_ID|TourCode|Gateway|State)",names(tours))


####################################
######### Decision Tree ############
####################################

library(rpart)
library(partykit)
tree <- rpart(formula = Book_12Mo ~ .,data=tours[,vars],control=rpart.control(cp=0.003))
print(tree)


# Model pruning
cp.seq=tree$cptable[,1]
misc<-numeric()
for (i in 1:length(cp.seq)) {
  tree.predict = predict(prune(tree, cp=cp.seq[i]), tours,type="class") 
  cm=table(tours[vars]$Book_12Mo, tree.predict) # confusion matrix 
  misc[i]=(cm[1,2]+cm[2,1])/sum(cm) # misclassification rate
}

plot(tree$cptable[,'nsplit']+1,misc,
     type="o", xlab="Number of Leaves", ylab="Misclassification Rate")


# Final model
tree.final=prune(tree,cp=cp.seq[misc==min(misc)])
plot(as.party(tree.final))
tree.misc<-min(misc)


print(tree.final)



nodes <- tree.final$where
table(nodes)



#selecting nodes of the tree in the dataset
#tables to see if the counts match up to decision tree
tours[nodes==2,]
table(tours[nodes == 2,]$Past_Trips) #Node 2
table(tours[nodes > 2,]$Past_Trips) #Everything after


table(tours[nodes == 4,]$Email) #Node 4
table(tours[nodes > 4,]$Email) #Everything after


table(tours[nodes %in% c(7,8),]$TourCode) #Nodes 7,8
table(tours[nodes > 8,]$TourCode) #Everything after



#trying to get a list of the tour codes in each group (without manually typing in from the tree output)
table(tours[nodes %in% c(7,8),]$TourCode)[table(tours[nodes %in% c(7,8),]$TourCode)>0]
table(tours[nodes > 8,]$TourCode)[table(tours[nodes > 8,]$TourCode)>0]


tourCode_Grp1 <- names(table(tours[nodes %in% c(7,8),]$TourCode)[table(tours[nodes %in% c(7,8),]$TourCode)>0])
tourCode_Grp2 <- names(table(tours[nodes > 8,]$TourCode)[table(tours[nodes > 8,]$TourCode)>0])
tourcodesplit <- ifelse(tours$TourCode %in% tourCode_Grp1, 1, 
                ifelse(tours$TourCode %in% tourCode_Grp2, 2, 3))





#####################
#Weight of Evidence

#used an example of how to do woe
#still have to look at results and see what changes to make
#getting a lot of NA's right now


#install.packages("klaR")
library(klaR)

train <- sample(nrow(tours), round(0.6*nrow(tours)))

woemodel <- woe(Book_12Mo~., data = tours[train,], zeroadj=0.5, applyontrain = TRUE)
woemodel

## plot variable information values and woes
plot(woemodel)
plot(woemodel, type = "woes")

## apply woes 
traindata <- predict(woemodel, tours[train,], replace = TRUE)
str(traindata)

## fit logistic regression model
glmodel     <- glm(Book_12Mo~., traindata, family=binomial)
summary(glmodel)
pred.trn <- predict(glmodel, traindata, type = "response")

## predict validation data
validata <- predict(woemodel, tours[-train,], replace = TRUE)
pred.val <- predict(glmodel, validata, type = "response")











##################
#Principal Component Analysis
#I tried using PCA to combine some variables
#The guideline is that components that explain 80% of the data can be used instead of the variables they represent

names(tours)

#Hotel_Avg through to Excellent_Buses
tours.pca <- prcomp(tours[28:57], center = T, scale = T)
summary(tours.pca)
#In this case, it takes about 13-14 components to reach 80%, which represent 30 variables
#


