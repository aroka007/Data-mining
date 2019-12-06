
library(ggplot2)

ggplot(tours, aes(x=Tour_Days, fill=Book_12Mo))+
  geom_histogram(binwidth=1)


plot(weekdays(tours$TourDate))
plot(months(tours$TourDate))



plot(as.factor(hours(tours$Domestic_Depart_Time)))
hist(hours(tours$Domestic_Depart_Time))

hours(tours$Domestic_Depart_Time) + minutes(tours$Domestic_Depart_Time)/60






#Histograms of depart and arrival times

ggplot(tours, aes(x = hours(tours$Domestic_Depart_Time))) +
  geom_bar(colour = "grey") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Domestic Depart Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))



ggplot(tours, aes(x = hours(tours$Intr_Arrival_Time))) +
  geom_histogram(colour = "grey") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("International Arrival Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))



ggplot(tours, aes(x = hours(tours$Intr_Depart_Time))) +
  geom_bar(colour = "grey") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("International Depart Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))



ggplot(tours, aes(x = hours(tours$Domestic_Arrival_Time))) +
  geom_histogram(colour = "grey") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Domestic Arrival Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))








ggplot(tours, aes(x = hours(tours$Domestic_Arrival_Time))) +
  geom_histogram(colour = "grey") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Domestic Arrival Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))



#with fill=Book_12Mo

ggplot(tours, aes(x = hours(tours$Domestic_Depart_Time), fill=Book_12Mo)) +
  geom_bar(position = "fill") + 
  #coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("International Arrival Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))



ggplot(tours, aes(x = hours(tours$Intr_Arrival_Time), fill=Book_12Mo)) +
  geom_bar(position = "fill") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("International Arrival Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))



ggplot(tours, aes(x = hours(tours$Intr_Depart_Time), fill=Book_12Mo)) +
  geom_bar(position = "fill") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("International Depart Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))



ggplot(tours, aes(x = hours(tours$Domestic_Arrival_Time), fill=Book_12Mo)) +
  geom_bar(position = "fill") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Domestic Arrival Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))






##########

ggplot(tours, aes(x = hours(tours$Domestic_Depart_Time),fill=tours$Book_12Mo)) +
  geom_bar(colour = "grey") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Domestic Depart Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))

##############


#
ggplot(tours, aes(x = Outbound_Connections, fill=Book_12Mo)) +
  geom_bar(aes(y = ..prop..), position="fill", stat="count")



ggplot(tours, aes(x = Return_Connections, fill=Book_12Mo)) +
  geom_bar(aes(y = ..prop..), position="fill", stat="count")





ggplot(tours, aes(x = Outbound_Connections + Return_Connections, fill=Book_12Mo)) +
  geom_bar(aes(y = ..prop..), position="fill", stat="count")


ggplot(tours, aes(x = Outbound_Connections + Return_Connections, fill=Book_12Mo)) +
  geom_bar( stat="count")








#
ggplot(tours.rf, aes(x = tours.rf$StateDivision, fill=Book_12Mo)) +
  geom_bar(position="fill") +
  #geom_text(aes(label = ..count.., y= ..prop..), stat= "count", vjust = 0) +
  scale_fill_brewer() + ggtitle("State Division")
  #scale_y_continuous(labels="percent", limits=(1000))




ggplot(tours.rf, aes(x = tours.rf$StateDivision, fill=Book_12Mo)) +
  geom_bar(position = "fill") + 
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Domestic Arrival Time") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24)) +
  geom_text(aes(label="count"), vjust=-1)







tours$StateRegion <- state.region[match(tours$State,state.abb)]
tours$StateDivision <- state.division[match(tours$State,state.abb)]







##################
###   States  ####
##################

states <- map_data("state")
ggplot(data = states, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()



levels(state.region)
levels(state.division)


state.x77 <- as.data.frame(state.x77)

state.reg <- data.frame(state.abb,state.region)
state.div <- data.frame(state.abb,state.division)


states

levels(factor(tours[tours$State %in% state.abb[state.division=="Middle Atlantic"],]$State))


library(car)
library(dplyr)
library(tibble)






in.US <- tours$State %in% c(state.abb[c(-2,-11)], "DC")
mean(in.US)
summary(in.US)

state.info <- plyr::count(tours[in.US,]$State)
state.info <- add_column(state.info,"name"= tolower(sort(c(state.name[-c(2,11)],"District of Columbia"))), .after=0)


states$freq <- state.info$freq[match(states$region,state.info$name)]

state.count.map <- ggplot(mapping = aes(long, lat, group = group)) +
    geom_polygon(data = states, color = "black", aes(fill=freq), size = 0.3) +
    scale_fill_gradient(low = "khaki", high = "red") +
    coord_quickmap()
state.count.map


state.info <- add_column(state.info,"pop"=States[-c(2,12),"pop"])

states$pop <- state.info$pop[match(states$region,state.info$name)]

state.pop.map <- ggplot(mapping = aes(long, lat, group = group)) +
  geom_polygon(data = states, color = "black", aes(fill=pop), size = 0.3) +
  scale_fill_gradient(low = "khaki", high = "red") +
  coord_quickmap()
state.pop.map

state.info$freq.pop.ratio <- state.info$freq/state.info$pop *1000
states$freq.pop.ratio <- state.info$freq.pop.ratio[match(states$region,state.info$name)]

state.ratio.map <- ggplot(mapping = aes(long, lat, group = group)) +
  geom_polygon(data = states, color = "black", aes(fill=freq.pop.ratio), size = 0.3) +
  scale_fill_gradient(low = "khaki", high = "red") +
  coord_quickmap()
state.ratio.map
state.info




