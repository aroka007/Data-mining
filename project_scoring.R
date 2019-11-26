##### Scoring ######
load("project_RF.RData")

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

scoring<- function(tours, Mean, Mode, RF) {
  raw<-tours
  
  library(tidyverse) #UNCOMMENT
  
  #str(tours)
  #tours$Book_12Mo<-as.factor(tours$Book_12Mo)
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
  
  #tours$Tour_Days<-as.factor(tours$Tour_Days)
  
  #Remove variables that may not be potential predictors! (Check later!)
  var.rem<--grep("^(FY|Complaint_Event|TD_Overall|Trip_no|Promo_Disc|Return_Connect_Time_Mins_2|Return_Connect_Time_Mins_1|Return_Connect_Time_Mins_2|Eval_Contact_Days|Outbound_Connect_Time_Mins_2|Optionals_2orUnder|Bus_2orUnder|Return_Connections|Domestic_Depart_Time|Outbound_Connect_Time_Mins_1|Outbound_Connect_Time_Mins_2|Domestic_Arrival_Time|Intr_Arrival_Time|Intr_Depart_Time|Cus_ID|ProdTour_ID|SalesTourID|Trip_No|HH_ID| Optionals_Avg|Bus_Avg|Meals_Avg|Hotels_Avg|GUSS_Avg|Hotel_2orUnder|Meals_2orUnder|GUSS_2orUnder|TourRegion|Outbound_Connect_Gateway2|Return_Connect_Gateway2|Start_Day|End_Day|FltGty|Domestic_Depart_Time|Optionals_Avg|Tour_Region|Outbound_Intr_Gateway|Return_Intr_Gateway|Outbound_Connect_Gateway1|Return_Connect_Gateway1)$",names(tours))
  
  new.tours<-tours[var.rem]
  str(new.tours)
  #-1 is missing here so assign na
  new.tours$Outbound_Connections[new.tours$Outbound_Connections==-1]<-NA
  
  sort(sapply(new.tours, function(x) sum(is.na(x))),decreasing = T)


  #$ Look into Age and Tour Days...
  #State and tour region(just to look into, tour code can be used instead of it) use WOE
  str(new.tours)
  #consolidate using training data
  
  #UNCOMMENT THE FOLLOWING

    # consldt<-new.tours %>%
    #   select(TourCode,TourDate,Outbound_Domestic_Gateway,Return_Domestic_Gateway,
    #          Book_12Mo)
    # library(tree.bins)
    # binned <- tree.bins(data = consldt, y = Book_12Mo,
    #                     bin.nm = "Group.", control = rpart.control(cp = .001),
    #                     return = "new.factors")



  ####################################
  ######### CHECK############
  ####################################

  #Add consolidated categories for training data to new.tours df
  # new.tours["TourCode_Groups"] <- as.factor(binned[[1]]$Categories[match(new.tours$TourCode,binned[[1]]$TourCode)])
  # new.tours["TourDate_Groups"] <- as.factor(binned[[2]]$Categories[match(new.tours$TourDate,binned[[2]]$TourDate)])# ###########################################################################################
  # new.tours["Outbound_Domestic_Gateway_Groups"] <- as.factor(binned[[3]]$Categories[match(new.tours$Outbound_Domestic_Gateway,binned[[3]]$Outbound_Domestic_Gateway)])# ###########################################################################################
  # new.tours["Return_Domestic_Gateway_Groups"] <- as.factor(binned[[4]]$Categories[match(new.tours$Return_Domestic_Gateway,binned[[4]]$Return_Domestic_Gateway)])# ###########################################################################################
  # 
  #new.tours$State_Cons<-dat$State_con
  #new.tours$State<-NULL
  
  #str(new.tours[,var1])


  ######################
  ####Transformation ####
  ######################
  tours.xf<-new.tours
  
  #Update#
  inp.n <- grep("^(EvalID|TourCode|TourDate|Outbound_Domestic_Gateway|Outbound_Connect_Gateway1|Return_Connect_Gateway1|Return_Domestic_Gateway)$", names(tours.xf))
  inx3<-inx(tours.xf, inp.n) 
  indx<-inx3$indx
  index.cat<-inx3$index.cat
  index.na<-inx3$index.na
  
  #Numeric transformation
  library(caret)
  TransformParams <- preProcess(tours.xf[,indx], method=c("YeoJohnson"))
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
  
  ######## Routine: Update Input Info ########
  inp.n <- grep("^(EvalID)", names(tours.xf))
  inx3<-inx(tours.imp, inp.n) 
  indx<-inx3$indx
  index.cat<-inx3$index.cat
  index.na<-inx3$index.na
  #########################################
  
  # check missing data
  names(tours.imp)[index.na==TRUE]
  
  # numeric impute: By mean #
  tours.imp[indx]<-as.data.frame(mapply(impute,x=tours.imp[indx],y = Mean))
  
  # Nominal Input: By Mode #
  tours.imp[index.cat]<-as.data.frame(mapply(impute,x=tours.imp[index.cat],y = Mode))
  
  #missmap(tours.imp)
  #######
  # create missing value flag #
  tours.imp[paste(names(tours.imp)[index.na], "NA", sep=".")] <- ifelse(
    is.na(tours.xf[index.na]), 1, 0)
  
  tours.imp[grep("NA$",names(tours.imp))]<-lapply(
    tours.imp[grep("NA$",names(tours.imp))], as.factor) 
  
  #sort(sapply(tours.imp, function(x) sum(is.na(x))),decreasing = T)

  ###################################################################################
  
  #######################
  #### Random Forest #### internal downsampling
  #######################
  
  tours.rf<-tours.imp
  str(tours.rf)
  vars1<--grep("^(EvalID|Outbound_Intr_Gateway|Return_Intr_Gateway|State)$", names(tours.rf))
  
  #minor<-unname(summary(tours.rf$Book_12Mo)[2])
  
  # Make predictions #
  library(caret)
  RF.class<- predict(RF, newdata=tours.rf, type="response")

  raw["pred.class"] <- data.frame(RF.class)
  raw
}


tours.score<- read.csv("project_scoring_data.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))
tours.score.pred<-scoring(tours.score, Mean, Mode, RF)

summary(tours.score.pred$pred.class)
prop.table(table(tours.score.pred$pred.class))

########
# Need to save EvalID and Predictions as a 2 column data frame and save it into a csv to turn in

scores<-as.data.frame(tours.score.pred$EvalID,tours.score.pred$pred.class)
write.csv(scores,"Path where you'd like to export the DataFrame\\File Name.csv", row.names = FALSE) # Change Path
