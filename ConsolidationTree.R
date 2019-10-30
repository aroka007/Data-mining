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

vars <- -grep("^(EvalID|Cus_ID|ProdTour_ID|SalesTourID|Trip_no|HH_ID)$",names(tours))




######################
##### Imputation #####
######################

#Routine Update#
inp.n <- grep("^(EvalID|Cus_ID|ProdTour_ID|SalesTourID|Trip_no|HH_ID|Book_12Mo)$",names(tours))
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



####################################
####### Consolidation Tree #########
####################################

library(rpart)
library(partykit)

tree <- rpart(formula = Book_12Mo ~ .,data=tours[,vars],control=rpart.control(cp=0.003))
print(tree)


summary(tours[index.cat])

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


TourCode_split <- ifelse(tours$TourCode %in% Grp1, 1, 
                      ifelse(tours$TourCode %in% Grp2, 2, 
                             ifelse(tours$TourCode %in% Grp3, 3, 
                                    ifelse(tours$TourCode %in% Grp4, 4, 0))))

sort(prop.table(table(TourCode_split,tours$Book_12Mo),1)[,2])*100


#State
tree <- rpart(formula = Book_12Mo ~ State, data=tours[,vars], control=rpart.control(cp=0.0001))
print(tree)
plot(as.party(tree))


nodes <- tree$where
table(nodes)

Grp1 <- names(which(table(tours[nodes == 2,]$State)>0))
Grp2 <- names(which(table(tours[nodes == 4,]$State)>0))
Grp3 <- names(which(table(tours[nodes == 6,]$State)>0))
Grp4 <- names(which(table(tours[nodes == 7,]$State)>0))


State_split <- ifelse(tours$State %in% Grp1, 1, 
                         ifelse(tours$State %in% Grp2, 2, 
                                ifelse(tours$State %in% Grp3, 3, 
                                       ifelse(tours$State %in% Grp4, 4, 0))))

sort(prop.table(table(State_split,tours$Book_12Mo),1)[,2])*100



#Outbound_Domestic_Gateway
tours$Outbound_Domestic_Gateway <- impute(tours$Outbound_Domestic_Gateway,mode(tours$Outbound_Domestic_Gateway))

tree <- rpart(formula = Book_12Mo ~ Outbound_Domestic_Gateway, data=tours[,vars], control=rpart.control(cp=0.001))
print(tree)
plot(as.party(tree))



nodes <- tree$where
table(nodes)


Grp1 <- names(which(table(tours[nodes == 2,]$Outbound_Domestic_Gateway)>0))
Grp2 <- names(which(table(tours[nodes == 4,]$Outbound_Domestic_Gateway)>0))
Grp3 <- names(which(table(tours[nodes == 6,]$Outbound_Domestic_Gateway)>0))
Grp4 <- names(which(table(tours[nodes == 7,]$Outbound_Domestic_Gateway)>0))


Outbound_Domestic_Gateway_split <- ifelse(tours$Outbound_Domestic_Gateway %in% Grp1, 1,
                                          ifelse(tours$Outbound_Domestic_Gateway %in% Grp2, 2, 
                                                 ifelse(tours$Outbound_Domestic_Gateway %in% Grp3, 3,
                                                        ifelse(tours$Outbound_Domestic_Gateway %in% Grp4, 4, 0))))

sort(prop.table(table(Outbound_Domestic_Gateway_split,tours$Book_12Mo),1)[,2])*100



