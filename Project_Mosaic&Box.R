#Initial Project Data Cleaning#
data<- read.csv("project_modeling_data.csv", header=TRUE, na.strings=c(".", "NA", "", "?"))
str(data)
vars<- -grep("^(EvalID|Cus_ID|ProdTour_ID|SalesTourID|Trip_No|HH_ID)", names(data))

#investigate target variable (Book_12Mo)
str(data$Book_12Mo)
summary(data$Book_12Mo) #22.39% of data contains positive response

#notes:
#consider making Tour_Season ordinal
#would the day of the week that the tour starts or ends on ever have any relevance?? Start_Day, End_Day
#look at relationship between TravelAgain and Target