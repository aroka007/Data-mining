##################  Load Data ##################

model <- read.csv("modeling_data.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

score <- read.csv("scoring_data.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))

load(file = "scoring.RData")

##################  Load Function  ############

inx <- function (data, inp.n) { # data: current dataframe; inp.n: position for non-inputs
  # numeric input indicator
  indx <- sapply(data, is.numeric)
  indx[inp.n]<-FALSE
  
  # nominal input indicator
  index.cat<-sapply(data, is.factor)
  index.cat[inp.n]<-FALSE
  
  # missing value indicator
  index.na<-sapply(data,function(x) any(is.na(x)))
  index.na[inp.n]<-FALSE
  
  data.frame(indx, index.cat, index.na)
}




########################################################################################
########################################################################################

inputs <- as.data.frame(seq(from=1, to=93), col.names="index")
colnames(inputs)[1] <-"index"
inputs$model <- names(model) 
inputs <- inputs[-93,]
inputs$score <- names(score)

## The following variables we want them to be factor variables
indx <- sapply(model, is.factor) 
indice <- which(indx==TRUE)
indice <- unname(indice)
diff1 <- vector(mode = "list", length = length(indice))   # create an empty list
diff2 <- vector(mode = "list", length = length(indice))   # create an empty list
for (i in seq(1:34)){
  temp <- indice[i]
  level1 <- levels(model[,temp])
  level2 <- levels(score[,temp])
  diff1[[i]] <- setdiff(level1, level2)    ## setdiff(x,y) are are those elements in x but not in y
  diff2[[i]] <- setdiff(level2, level1)
}


for (i in seq(1:34)){
  temp <- indice[i]
  level1 <- levels(new.tours[,temp])
  level2 <- levels(score[,temp])
  diff1[[i]] <- setdiff(level1, level2)    ## setdiff(x,y) are are those elements in x but not in y
  diff2[[i]] <- setdiff(level2, level1)
}



#############################
### Fix "TourDate" levels ###
#############################
model$TourDate <- as.Date(model$TourDate, "%m/%d/%Y")
model$TourDate <- format(model$TourDate, "%m/%Y")

score$TourDate <- as.Date(score$TourDate, "%m/%d/%Y")
score$TourDate <- format(score$TourDate, "%m/%Y")

### OK, let's check "TourDate" now
score_date <- unique(score$TourDate)
model_date <- unique(model$TourDate)
setdiff(model_date, score_date)

### OK, nice, there is no difference now


# 2. For "time" variable, only keep hours
# library(chron)
ind <- c(76,82,85,91)
## convert to time 
for (i in seq(1:4)){
  temp <- ind[i]
  score[,temp] <- times(score[,temp])
}

## only keep hour
for (i in seq(1:4)){
  temp <- ind[i]
  score[,temp] <- hours(score[,temp])
}

for (i in seq(1:4)){
  temp <- ind[i]
  score[ ,temp] <- as.factor(score[ ,temp])
}

#####################################################
################  Data Consolidation  ###############
#####################################################

#################################  Summary  #################################
## We consoliidated variables by using WOE and decision tree

## Get the names for every column, and column #
Varnames <- as.data.frame(seq(from=1, to=ncol(dat)), col.names="index")
colnames(Varnames)[1] <-"index"
Varnames$name <- names(dat) 

# library(Information)
#########################################
##########  1. Tour_Days  ###############
#########################################

fctr1 <- c(6,4,17,9,11,15,12,21,24)
fctr2 <- c(16,13,10,14,23,18,1)
fctr3 <- c(7,8,19)
score$Tour_Days_Con <- as.character(score$Tour_Days)

ind1 <- which(score$Tour_Days_Con %in% fctr1)
ind2 <- which(score$Tour_Days_Con %in% fctr2)
ind3 <- which(score$Tour_Days_Con %in% fctr3)
score$Tour_Days_Con[ind1] <- "fctr1"
score$Tour_Days_Con[ind2] <- "fctr2"
score$Tour_Days_Con[ind3] <- "fctr3"
length(ind1)+length(ind2)+length(ind3)

score$Tour_Days_Con <- as.factor(score$Tour_Days_Con)
score$Tour_Days <- NULL  ## delete "Tour_Days"


##############################################
########## Grp_size_ratio_Con  ###############
##############################################
score$Grp_Size_ratio <- score$Grp_Size/score$Capacity

score$Grp_Size_ratio_Con <- as.character(score$Grp_Size_ratio) 
ind1 <- which(score$Grp_Size_ratio<=0.55)
ind2 <- which(score$Grp_Size_ratio<=0.73 & score$Grp_Size_ratio>0.55)
ind3 <- which(score$Grp_Size_ratio>0.73)

score$Grp_Size_ratio_Con[ind1] <- "low"
score$Grp_Size_ratio_Con[ind2] <- "med"
score$Grp_Size_ratio_Con[ind3] <- "compact"
score$Grp_Size_ratio_Con <- factor(score$Grp_Size_ratio_Con, order =T,levels = c("compact","med","low"))

dat[ ,c("Grp_Size_ratio","Grp_Size_Cat","Grp_Size","Capacity")] <- NULL


###################################
########## TourDate ###############
###################################

## now,we are going to find which dates will be assigned to which group
low <- woe.list[["TourDate"]]$TourDate[which(woe.list[["TourDate"]]$WOE < -0.14)]
fair <-woe.list[["TourDate"]]$TourDate[which(woe.list[["TourDate"]]$WOE > -0.14 & woe.list[["TourDate"]]$WOE<0.07)]
good <-woe.list[["TourDate"]]$TourDate[which(woe.list[["TourDate"]]$WOE>0.07)]

score$TourDate_Con <- as.character(score$TourDate) 

ind1 <- which(score$TourDate %in% low) # find the indices that will be assigned to "low" group
ind2 <- which(score$TourDate %in% fair) # find the indices that will be assigned to "med" group
ind3 <- which(score$TourDate %in% good) # find the indices that will be assigned to "good" group
# length(ind1)+length(ind2)+length(ind3) # check the number is right 

score$TourDate_Con[ind1] <- "poor"
score$TourDate_Con[ind2] <- "fair"
score$TourDate_Con[ind3] <- "good"

score$TourDate_Con <- factor(score$TourDate_Con, order=T,levels = c("poor","fair","good"))
# str(score$TourDate_Con) check the levels

score$TourDate <- NULL

#########################################
########## Recommend_GAT  ###############
#########################################
ind <- which(score$Recommend_GAT == 0)
# length(ind)
# 540 "0"s, so we should put "0" into another group, and we will assign "0" to group "2"
score$Recommend_GAT[ind] = 2
# unique(score$Recommend_GAT)  check
score$Recommend_GAT <- as.factor(score$Recommend_GAT)


#######################################
########## TravelAgain  ###############
#######################################
ind <- which(score$TravelAgain == 0)
# length(ind)
# 705 "0"s, we will assign "0" to group "2"
score$TravelAgain[ind] = 2
# unique(score$Recommend_GAT)  check
score$TravelAgain <- as.factor(score$TravelAgain)

###########################################
########## Groups_Interest  ###############
###########################################
# we are not going to use this variable
score$Groups_Interest <- NULL

########## Reference  ###############
ind <- which(score$Reference == 0)
# length(ind)
# 768 "0"s, we will assign "0" to group "1"
score$Reference[ind] = 1
# unique(score$Recommend_GAT)  check
score$Reference <- as.factor(score$Reference)

##########  Overall_Impression  ###############
# put 0, 1, 2 into 3
ind0 <- which(score$Overall_Impression == 0)
ind1 <- which(score$Overall_Impression == 1)
ind2 <- which(score$Overall_Impression == 2)
ind <-c(ind0,ind1,ind2)
score$Overall_Impression[ind] = 3
score$Overall_Impression <- factor(score$Overall_Impression, order =T, levels = c("3","4","5"))
# str(score$Overall_Impression)

##########  Pre_Departure  ###############
# put 0 into 4
ind0 <- which(score$Pre_Departure == 0)
score$Pre_Departure[ind0] = 4
score$Pre_Departure <- factor(score$Pre_Departure, order =T, levels = c("1","2","3","4","5"))
# str(score$Pre_Departure)

##########  Flight_Itin  ###############
# put 0,1,2 into 3
ind0 <- which(score$Flight_Itin == 0)
ind1 <- which(score$Flight_Itin == 1)
ind2 <- which(score$Flight_Itin == 2)
ind <-c(ind0,ind1,ind2)
score$Flight_Itin[ind] = 3
score$Flight_Itin <- factor(score$Flight_Itin, order =T, levels = c("3","4","5"))
# str(score$Flight_Itin)

##########  TD_Overall  ###############
# put 0,1 into 2
ind0 <- which(score$TD_Overall == 0)
ind1 <- which(score$TD_Overall == 1)
ind <-c(ind0,ind1)
score$TD_Overall[ind] = 2
score$TD_Overall <- factor(score$TD_Overall, order =T, levels = c("2","3","4"))
# str(score$TD_Overall)


################  hotel_rating  ##############
score$hotel_num <- score$Poor_Hotels + score$Fair_Hotels + score$Good_Hotels + score$Excellent_Hotels
# summary(score$hotel_num)

ind <- which(score$hotel_num == 0)
x<- score[ind,]
# summary(x$Book_12Mo)
# 28.9% repeat customer
score$hotel_rating <- 100 + (-4)*score$Poor_Hotels +  score$Good_Hotels + 2*(score$Excellent_Hotels)
# summary(score$hotel_rating)

# prop <- dat %>% group_by(hotel_rating, Book_12Mo) %>% tally() %>%
mutate(pct=n/sum(n))%>% filter(Book_12Mo==1)
score$hotel_rating[ind] = 120

#prop <- dat %>% group_by(hotel_rating, Book_12Mo) %>% tally() %>%
#  mutate(pct=n/sum(n))%>% filter(Book_12Mo==1)
#ggplot(prop, aes(hotel_rating, log(pct))) +geom_point()+geom_smooth(method = "lm")

################################################
################  meal_rating  ##############
################################################
score$meal_num <- score$Poor_Meals + score$Fair_Meals + score$Good_Meals + score$Excellent_Meals
# summary(score$meal_num)

ind <- which(score$meal_num == 0)
# 2990 "0"
#x<- score[ind,]
#summary(x$Book_12Mo)
# 24.11% repeat customer
score$meal_rating <- 50 + (-4)*score$Poor_Meals - 2*score$Fair_Meals + score$Good_Meals
#summary(score$meal_rating)

#prop <- dat %>% group_by(meal_rating, Book_12Mo) %>% tally() %>%
#  mutate(pct=n/sum(n))%>% filter(Book_12Mo==1)
#ggplot(prop, aes(meal_rating, pct)) +geom_point()
#ggplot(prop, aes(meal_rating, log(pct))) +geom_point()
score$meal_rating[ind] = 55


################################################
################ guss_rating ##############
################################################
score$guss_num <- score$Poor_GUSS + score$Fair_GUSS + score$Good_GUSS + score$Excellent_GUSS
# summary(score$guss_num)

ind <- which(score$guss_num == 0)
# 3782 "0"
#x<- score[ind,]
#summary(x$Book_12Mo)
# 24.11% repeat customer

#prop <- dat %>% group_by(Excellent_GUSS, Book_12Mo) %>% tally() %>%
#  mutate(pct=n/sum(n))%>% filter(Book_12Mo==1)
#ggplot(prop, aes(Excellent_GUSS, pct)) +geom_point()

score$guss_rating <- 50 - score$Poor_GUSS - 2*score$Fair_GUSS


################################################
################  Optionals_Avg_Con  ###########
################################################

poor <- woe.list[["Optionals_Avg"]]$Optionals_Avg[which(woe.list[["Optionals_Avg"]]$WOE < -0.2)]
poor <- c(poor, 1.2,1.3,1.4)
fair <- woe.list[["Optionals_Avg"]]$Optionals_Avg[which(woe.list[["Optionals_Avg"]]$WOE <=0 & woe.list[["Optionals_Avg"]]$WOE > -0.2)]
good <- woe.list[["Optionals_Avg"]]$Optionals_Avg[which(woe.list[["Optionals_Avg"]]$WOE > 0)]
# length(poor)+length(fair)+length(good)
# length(unique(score$Optionals_Avg))

score$Optionals_Avg_Con <- as.character(score$Optionals_Avg)

ind1 <- which(score$Optionals_Avg_Con %in% poor)
ind2 <- which(score$Optionals_Avg_Con %in% fair)
ind3 <- which(score$Optionals_Avg_Con %in% good)

# length(ind1)+length(ind2)+ length(ind3)

score$Optionals_Avg_Con[ind1] <- "poor"
score$Optionals_Avg_Con[ind2] <- "fair"
score$Optionals_Avg_Con[ind3] <- "good"

score$Optionals_Avg_Con <- factor(score$Optionals_Avg_Con, order = T,
                                levels = c("poor","fair","good"))
# str(score$Optionals_Avg_Con)
score$Optionals_Avg <- NULL

###############################
########## Age  ###############
###############################


score$Age_Con <- as.character(score$Age) 
ind1 <- which(score$Age %in% c("30-39","40-44","45-49"))
ind2 <- which(score$Age %in% c("50-54","55-59","60-69","70-79"))
ind3 <- which(score$Age %in% c("No Age","Over 80","Under 30"))
# length(ind1)+length(ind2)+length(ind3)

score$Age_Con[ind1] <- "mid"
score$Age_Con[ind2] <- "old"
score$Age_Con[ind3] <- "mix"

score$Age_Con <- factor(score$Age_Con, order = T,
                      levels = c("mid","old","mix"))
score$Age <- NULL

#################################
########## State  ###############
#################################

low <- woe.list[["State"]]$State[which(woe.list[["State"]]$WOE < (-0.2) | woe.list[["State"]]$WOE==0)]
fair <- woe.list[["State"]]$State[which(woe.list[["State"]]$WOE > (-0.2) & woe.list[["State"]]$WOE<0)]
med <- woe.list[["State"]]$State[which(woe.list[["State"]]$WOE>0 & woe.list[["State"]]$WOE<0.15)]
good <- woe.list[["State"]]$State[which(woe.list[["State"]]$WOE>0.15)]

st <- c(low, fair,med,good)
diff <- setdiff(levels(score$State), st) # find the states which are not in the training dataset
low <- c(low, diff) # Put thoes states into low group

# length(low) + length(fair)+length(med)+length(good)
# str(score$State)

score$State_Con <- as.character(score$State) 
ind1 <- which(score$State %in% low)
ind2 <- which(score$State %in% fair)
ind3 <- which(score$State %in% med)
ind4 <- which(score$State %in% good)
# length(ind1)+length(ind2)+length(ind3)+length(ind4)

score$State_Con[ind1] <- "low"
score$State_Con[ind2] <- "fair"
score$State_Con[ind3] <- "med"
score$State_Con[ind4] <- "good"

score$State_Con <- factor(score$State_Con, order = T,
                        levels = c("low","fair","med","good"))
levels(score$State_Con)
score$State <- NULL

##################################
##########  Email  ###############
##################################
# we combine "Bounced" and "Unavailable" together
score$Email <- as.character(score$Email)
ind <- which(score$Email == "Bounced")
score$Email[ind] <- "Unavailable"
score$Email <- as.factor(score$Email)
# levels(score$Email)

######################################
##########  Optionals  ###############
######################################
ind <- which(score$Optionals == 0)
# length(ind)
# 8024 "0"

poor <- woe.list[["Optionals"]]$Optionals[which(woe.list[["Optionals"]]$WOE <= -0.13)]
fair <- woe.list[["Optionals"]]$Optionals[which(woe.list[["Optionals"]]$WOE <=0 & woe.list[["Optionals"]]$WOE > -0.13)]
med <- woe.list[["Optionals"]]$Optionals[which(woe.list[["Optionals"]]$WOE <= 0.22 & woe.list[["Optionals"]]$WOE > 0)]
good <- woe.list[["Optionals"]]$Optionals[which(woe.list[["Optionals"]]$WOE > 0.22)]


good <- c(good,120)
med <- c(med, 38,45)

score$Optionals_Con <- as.character(score$Optionals) 

ind1 <- which(score$Optionals_Con %in% poor)
ind2 <- which(score$Optionals_Con %in% fair)
ind3 <- which(score$Optionals_Con %in% med)
ind4 <- which(score$Optionals_Con %in% good)

# length(ind1)+length(ind2)+ length(ind3)+length(ind4)

score$Optionals_Con[ind1] <- "poor"
score$Optionals_Con[ind2] <- "fair"
score$Optionals_Con[ind3] <- "med"
score$Optionals_Con[ind4] <- "good"

score$Optionals_Con <- factor(score$Optionals_Con, order = T,
                            levels = c("poor","fair","med","good"))
# str(score$Optionals_Con)
score$Optionals <- NULL


#######################################
########  Domestic_Depart_Time  #######
#######################################

low <- woe.list[["DomDepTime"]]$Domestic_Depart_Time[which(woe.list[["DomDepTime"]]$WOE < -0.18)]
fair <- woe.list[["DomDepTime"]]$Domestic_Depart_Time[which(woe.list[["DomDepTime"]]$WOE > -0.18 & woe.list[["DomDepTime"]]$WOE<0.01)]
good <- woe.list[["DomDepTime"]]$Domestic_Depart_Time[which(woe.list[["DomDepTime"]]$WOE>0.01)]
# length(low) + length(fair)+length(good)
# levels(score$Domestic_Depart_Time)

score$Domestic_Depart_Time_Con <- as.character(score$Domestic_Depart_Time)

ind1 <- which(score$Domestic_Depart_Time_Con %in% low)
ind2 <- which(score$Domestic_Depart_Time_Con %in% fair)
ind3 <- which(score$Domestic_Depart_Time_Con %in% good)
# length(ind1)+length(ind2)+length(ind3)

score$Domestic_Depart_Time_Con[ind1] <- "poor"
score$Domestic_Depart_Time_Con[ind2] <- "fair"
score$Domestic_Depart_Time_Con[ind3] <- "good"
score$Domestic_Depart_Time_Con <- factor(score$Domestic_Depart_Time_Con,
                                       order=T,levels = c("poor","fair","good"))
# str(score$Domestic_Depart_Time_Con)
score$Domestic_Depart_Time <- NULL


######################################
########  Domestic_Arrival_Time  #####
######################################

low <- woe.list[["DomArrTime"]]$Domestic_Arrival_Time[which(woe.list[["DomArrTime"]]$WOE < -0.2)]
fair <- woe.list[["DomArrTime"]]$Domestic_Arrival_Time[which(woe.list[["DomArrTime"]]$WOE > -0.2 & woe.list[["DomArrTime"]]$WOE<0.01)]
med <- woe.list[["DomArrTime"]]$Domestic_Arrival_Time[which(woe.list[["DomArrTime"]]$WOE > 0.01 & woe.list[["DomArrTime"]]$WOE<0.1)]
good <- woe.list[["DomArrTime"]]$Domestic_Arrival_Time[which(woe.list[["DomArrTime"]]$WOE>0.1)]
# length(low) + length(fair)+length(med)+length(good)
# length(levels(score$Domestic_Arrival_Time))
# it's alright we have one more level since I put NA in good level
score$Domestic_Arrival_Time_Con <- as.character(score$Domestic_Arrival_Time)
ind1 <- which(score$Domestic_Arrival_Time_Con %in% low)
ind2 <- which(score$Domestic_Arrival_Time_Con %in% fair)
ind3 <- which(score$Domestic_Arrival_Time_Con %in% med)
ind4 <- which(score$Domestic_Arrival_Time_Con %in% good)
# length(ind1)+length(ind2)+length(ind3)+length(ind4)

score$Domestic_Arrival_Time_Con[ind1] <- "poor"
score$Domestic_Arrival_Time_Con[ind2] <- "fair"
score$Domestic_Arrival_Time_Con[ind3] <- "med"
score$Domestic_Arrival_Time_Con[ind4] <- "good"
score$Domestic_Arrival_Time_Con <- factor(score$Domestic_Arrival_Time_Con,
                                        order=T,levels = c("poor","fair","med","good"))
# str(score$Domestic_Arrival_Time_Con)
score$Domestic_Arrival_Time <- NULL


#####################################
########  Intr_Depart_Time  #########
#####################################

low <- woe.list[["IntDepTime"]]$Intr_Depart_Time[which(woe.list[["IntDepTime"]]$WOE < 0)]
good <- woe.list[["IntDepTime"]]$Intr_Depart_Time[which(woe.list[["IntDepTime"]]$WOE>=0)]
# length(low) + length(good)
# length(levels(score$Intr_Depart_Time))
# it's alright we have one more level since I put NA in good level

score$Intr_Depart_Time_Con <- as.character(score$Intr_Depart_Time)
ind1 <- which(score$Intr_Depart_Time_Con %in% low)
ind2 <- which(score$Intr_Depart_Time_Con %in% good)
# length(ind1)+length(ind2)

score$Intr_Depart_Time_Con[ind1] <- "poor"
score$Intr_Depart_Time_Con[ind2] <- "good"
score$Intr_Depart_Time_Con <- factor(score$Intr_Depart_Time_Con, levels = c("poor","good"))
# levels(score$Intr_Depart_Time_Con)

score$Intr_Depart_Time <- NULL


#####################################
########  Intr_Arrival_Time  ########
#####################################
low <- woe.list[["IntArrTime"]]$Intr_Arrival_Time[which(woe.list[["IntArrTime"]]$WOE < -0.1)]
fair <- woe.list[["IntArrTime"]]$Intr_Arrival_Time[which(woe.list[["IntArrTime"]]$WOE > -0.1 & woe.list[["IntArrTime"]]$WOE <=0)]
med <- woe.list[["IntArrTime"]]$Intr_Arrival_Time[which(woe.list[["IntArrTime"]]$WOE > 0 & woe.list[["IntArrTime"]]$WOE <= 0.15)]
good <- woe.list[["IntArrTime"]]$Intr_Arrival_Time[which(woe.list[["IntArrTime"]]$WOE>0.15)]
# length(low) + length(fair)+ length(med)+length(good)
# length(levels(score$Intr_Depart_Time))
# it's alright we have one more level since I put NA in good level

score$Intr_Arrival_Time_Con <- as.character(score$Intr_Arrival_Time)

ind1 <- which(score$Intr_Arrival_Time_Con %in% low)
ind2 <- which(score$Intr_Arrival_Time_Con %in% fair)
ind3 <- which(score$Intr_Arrival_Time_Con %in% med)
ind4 <- which(score$Intr_Arrival_Time_Con %in% good)
# length(ind1)+length(ind2)+ length(ind3)+length(ind4)

score$Intr_Arrival_Time_Con[ind1] <- "poor"
score$Intr_Arrival_Time_Con[ind2] <- "fair"
score$Intr_Arrival_Time_Con[ind3] <- "med"
score$Intr_Arrival_Time_Con[ind4] <- "good"
score$Intr_Arrival_Time_Con <- factor(score$Intr_Arrival_Time_Con, 
                                    order = T, levels = c("poor","fair","med","good"))
# str(score$Intr_Arrival_Time_Con)
score$Intr_Depart_Time <- NULL


################################################
########  Outbound_Connect_Time_Mins_1  ########
################################################
ind1 <- which(score$Outbound_Connect_Time_Mins_1 >=-1 & score$Outbound_Connect_Time_Mins_1<=61)
ind2 <- which(score$Outbound_Connect_Time_Mins_1 >=62 & score$Outbound_Connect_Time_Mins_1<=80|score$Outbound_Connect_Time_Mins_1==-1270)
ind3 <- which(score$Outbound_Connect_Time_Mins_1 >=81 & score$Outbound_Connect_Time_Mins_1<=101)
ind4 <- which(score$Outbound_Connect_Time_Mins_1 >=102 & score$Outbound_Connect_Time_Mins_1<=126)
ind5 <- which(score$Outbound_Connect_Time_Mins_1 >=127 & score$Outbound_Connect_Time_Mins_1<=153)
ind6 <- which(score$Outbound_Connect_Time_Mins_1 >=154 & score$Outbound_Connect_Time_Mins_1<=248)
ind7 <- which(score$Outbound_Connect_Time_Mins_1 >248)

score$Out_Connect_Mins1_Con <- as.character(score$Outbound_Connect_Time_Mins_1)

score$Out_Connect_Mins1_Con[ind1] <- "grp1"
ind8 <- c(ind2,ind3,ind4,ind5,ind7)
score$Out_Connect_Mins1_Con[ind8] <- "grp2"
score$Out_Connect_Mins1_Con[ind6] <- "grp3"

score$Out_Connect_Mins1_Con <- as.factor(score$Out_Connect_Mins1_Con)
# levels(score$Out_Connect_Mins1_Con)
score$Outbound_Connect_Time_Mins_1 <- NULL

################################################
########  Outbound_Connect_Time_Mins_2  ########
################################################
ind1 <- which(score$Outbound_Connect_Time_Mins_2 >=-1 & score$Outbound_Connect_Time_Mins_2<=64)
ind2 <- which(score$Outbound_Connect_Time_Mins_2 >=65 & score$Outbound_Connect_Time_Mins_2<=134)
ind3 <- which(score$Outbound_Connect_Time_Mins_2 >=135)

score$Out_Connect_Mins2_Con <- as.character(score$Outbound_Connect_Time_Mins_2)

score$Out_Connect_Mins2_Con[ind1] <- "grp1"
score$Out_Connect_Mins2_Con[ind2] <- "grp2"
score$Out_Connect_Mins2_Con[ind3] <- "grp3"

score$Out_Connect_Mins2_Con <- as.factor(score$Out_Connect_Mins2_Con)
# levels(score$Out_Connect_Mins2_Con)
score$Outbound_Connect_Time_Mins_2 <- NULL


################################################
########  Return_Connect_Time_Mins_1  ########
################################################
ind1 <- which(score$Return_Connect_Time_Mins_1 >=110 & score$Return_Connect_Time_Mins_1<=134)

ind2a <- which(score$Return_Connect_Time_Mins_1 >=135 & score$Return_Connect_Time_Mins_1<=167)
ind2b <- which(score$Return_Connect_Time_Mins_1 >=215 & score$Return_Connect_Time_Mins_1<=269)
ind2 <- c(ind2a, ind2b)

ind3a <- which(score$Return_Connect_Time_Mins_1 <= -100)
ind3b <- which(score$Return_Connect_Time_Mins_1 >=-1 & score$Return_Connect_Time_Mins_1<=89)
ind3c <- which(score$Return_Connect_Time_Mins_1 >=168 & score$Return_Connect_Time_Mins_1<=214)
ind3 <- c(ind3a, ind3b, ind3c)

ind4a <- which(score$Return_Connect_Time_Mins_1 >= 270)
ind4b <- which(score$Return_Connect_Time_Mins_1 >=90 & score$Return_Connect_Time_Mins_1<=109)
ind4 <- c(ind4a, ind4b)
# length(ind1)+length(ind2)+length(ind3)+length(ind4)

score$Return_Connect_Mins1_Con <- as.character(score$Return_Connect_Time_Mins_1)

score$Return_Connect_Mins1_Con[ind1] <- "grp1"
score$Return_Connect_Mins1_Con[ind2] <- "grp2"
score$Return_Connect_Mins1_Con[ind3] <- "grp3"
score$Return_Connect_Mins1_Con[ind4] <- "grp4"

score$Return_Connect_Mins1_Con <- as.factor(score$Return_Connect_Mins1_Con)
# levels(score$Return_Connect_Mins1_Con)
score$Return_Connect_Time_Mins_1 <- NULL


################################################
########  Return_Connect_Time_Mins_2  ########
################################################

ind1 <- which(score$Return_Connect_Time_Mins_2 >=165)
ind2 <- which(score$Return_Connect_Time_Mins_2 >=86 & score$Return_Connect_Time_Mins_2<=164)
ind3 <- which(score$Return_Connect_Time_Mins_2 >=-1 & score$Return_Connect_Time_Mins_2<=85)
# length(ind1)+length(ind2)+length(ind3)

score$Return_Connect_Mins2_Con <- as.character(score$Return_Connect_Time_Mins_2)

score$Return_Connect_Mins2_Con[ind1] <- "grp1"
score$Return_Connect_Mins2_Con[ind2] <- "grp2"
score$Return_Connect_Mins2_Con[ind3] <- "grp3"


score$Return_Connect_Mins2_Con <- as.factor(score$Return_Connect_Mins2_Con)
# levels(score$Return_Connect_Mins1_Con)
score$Return_Connect_Time_Mins_2 <- NULL

######################################################################################################
#Total Connections#
####################
score$Total_Connections<-score$Outbound_Connections+score$Return_Connections
score$Total_Connections<-as.character(score$Total_Connections)

fctr1 <- c(0,2,3,5,6,7)
fctr2 <- c(1,4,8)

ind1 <- which(score$Total_Connections %in% fctr1)
ind2 <- which(score$Total_Connections %in% fctr2)

score$Total_Connections[ind1] <- "fctr1"
score$Total_Connections[ind2] <- "fctr2"

score$Total_Connections<-as.factor(score$Total_Connections)


############################
#####################
###########################

#Add consolidated categories for training data to dat df
score["TourCode_Groups"] <- as.factor(binned[[1]]$Categories[match(score$TourCode,binned[[1]]$TourCode)])
#dat["TourDate_Groups"] <- as.factor(binned[[2]]$Categories[match(score$TourDate,binned[[2]]$TourDate)])# ###########################################################################################
score["Outbound_Domestic_Gateway_Groups"] <- as.factor(binned[[2]]$Categories[match(score$Outbound_Domestic_Gateway,binned[[2]]$Outbound_Domestic_Gateway)])# ###########################################################################################
score["Return_Domestic_Gateway_Groups"] <- as.factor(binned[[3]]$Categories[match(score$Return_Domestic_Gateway,binned[[3]]$Return_Domestic_Gateway)])# ###########################################################################################




##################
#########################
##### Transformation #####
##########################

score.xf <- score
#Routine Update#
inp.n <- grep("^(EvalID|Book_12Mo)$",names(score.xf))
inx3<-inx(score.xf, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
#####################
indx <-c(22:24)

TransformParams <- preProcess(score.xf[split,indx], method=c("YeoJohnson"))
TransformParams$yj
trans <- predict(TransformParams, score.xf[,indx])

for (i in seq(1:3)){
  temp <- indx[i]
  score.xf[ ,temp] <-trans[,i]
}



######################
##### Imputation #####
######################
score.imp <- score.xf

#Routine Update#
inp.n <- grep("^(EvalID|Book_12Mo)$",names(score.imp))
inx3<-inx(score.imp, inp.n) 
indx<-inx3$indx
index.cat<-inx3$index.cat
index.na<-inx3$index.na
#####################

names(score.imp)[index.na==T]
# summary(score.imp[index.na])

# Nominal Input: By Mode #
Mode<-sapply(score.imp[split, index.cat],mode)
score.imp[index.cat]<-as.data.frame(mapply(impute,x=score.imp[index.cat],y = Mode))

# sort(sapply(score.imp, function(x) sum(is.na(x))),decreasing = T)

# create missing value flag #
score.imp[paste(names(score.imp)[index.na], "NA", sep=".")] <- ifelse(
  is.na(score[index.na]), 1, 0)

score.imp[grep("NA$",names(score.imp))]<-lapply(
  score.imp[grep("NA$",names(score.imp))], as.factor) 




############
##############
#############
score.rf <- score.imp

library(caret)
RF.class<- predict(RF, newdata=score.rf, type="response")
#RF.class[is.na(RF.class)]<-"1"
summary(RF.class)

raw["Score_pred.class"]<-as.data.frame(RF.class) #***** *

score<-raw %>% select(EvalID,Score_pred.class)

write.csv(score,"ScoredP2.csv",row.names = F)
#***** *


