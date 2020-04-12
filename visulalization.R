rm(list=ls())


#setwd('E:\\jupiter\\Big Data')       ## Mohamed Directory
setwd("D:\\Major & Interests\\MyProjects\\BDA-Project") ## Omar Directory
library(ggplot2)

# 2- Importing the dataset Passenger_satisification_processed.csv into a data frame. 
Passenger_Data <- read.csv(file = 'Passenger_satisfaction_processed.csv')

dim(Passenger_Data)

# 3.b Showing the structure of the data frame.
str(Passenger_Data)

# 3.c Get more insight into data by exploring the first and the last ten rows in the dataset.
head(Passenger_Data, 10)

tail(Passenger_Data, 10)

# 3.d Show summary of all variables in the data frame.
summary(Passenger_Data)

# 4.a Show a summary for the variable age only.
summary(Passenger_Data$Age)


# 4 - b  the first and third quartile values
quantile(Passenger_Data$Age)

is.na(Passenger_Data$Age)


# 6 - a Show the number of males and females
m<- sum(Passenger_Data$Gender == 1)
f<- sum(Passenger_Data$Gender == 0)
Passenger_Data$Gender


# Pie Chart with Percentages
colors = c("blue", "red") 
slices <- c(m, f) 
lbls <- c("males", "females")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=colors, main="Pie Chart of Gender")


# 6 - c,d

males_satisfied <- sum(Passenger_Data$Gender == 1 & Passenger_Data$satisfaction_v2 == '1')
females_satisfied<- sum(Passenger_Data$Gender == 0 & Passenger_Data$satisfaction_v2 == '1')
males_notsatisfied<- sum(Passenger_Data$Gender == 1 & Passenger_Data$satisfaction_v2 == '0')
females_notsatisfied<- sum(Passenger_Data$Gender == 0 & Passenger_Data$satisfaction_v2 == '0')

############## satisfied males and females ###############
colors = c("blue", "red") 
slices <- c(males_satisfied, females_satisfied) 
lbls <- c("satisfied males", "satisfied females")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=colors, main="Pie Chart of statisification percentage")

########## dissatisfied ################

colors = c("blue", "red") 
slices <- c(males_notsatisfied, females_notsatisfied) 
lbls <- c("dissatisfied males", "dissatisfied females")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=colors, main="Pie Chart of statisification percentage")


######################################################################################

# 6- visualizing the type of travel class with satisification

Eco_satisfied <- sum(Passenger_Data$satisfaction_v2 == '1' & Passenger_Data$Class == '1')
Eco_notsatisfied<- sum(Passenger_Data$satisfaction_v2 == '0' & Passenger_Data$Class == '1')

EcoPlus_satisfied<- sum(Passenger_Data$satisfaction_v2 == '1' & Passenger_Data$Class == '2')
EcoPlus_notsatisfied<- sum(Passenger_Data$satisfaction_v2 == '0' & Passenger_Data$Class == '2')

Bussiness_satisfied<- sum(Passenger_Data$satisfaction_v2 == '1' & Passenger_Data$Class == '0')
Bussiness_notsatisfied<- sum(Passenger_Data$satisfaction_v2 == '0' & Passenger_Data$Class == '0')

classes <-c(
        Bussiness_satisfied,Bussiness_notsatisfied,
        EcoPlus_satisfied,EcoPlus_notsatisfied,
        Eco_satisfied,Eco_notsatisfied)

############# Relation between class  and degree of statisfication ###############
# barchart with added parameters
barplot(classes,
        main = "Relation between classes  and degree of statisfication",
        xlab = "Classes",
        ylab = "satisfication and dissatisfication",
        names.arg = c("Bussiness class satisification","Bussiness class dissatisification",
                      "EcoPlus satisification","EcoPlus dissatisification",
                      "Eco satisification", "Eco dissatisification"),
        col = c("blue", "red"))
boxplot(Passenger_Data$Age, main="Age")


############ stacked bars (loyal - disloyal with satisification) ##############

counts <- table(Passenger_Data$Customer_Type, Passenger_Data$satisfaction_v2)
barplot(counts, main="satsification of loyal and disloyal customers",
        xlab="Satisfication level", col=c("darkblue","red"),
        legend.text = c("dissatisfied customer","satisfied customers"))
##########################################################################
Passenger_Data

# type of travel ( CLASS bussiness and type of travel )
# legroom service & age 50 with satisfication and legroom service & age 25 with satisfication


# cleanliness vs satisfication
counts <- table(Passenger_Data$satisfaction_v2, Passenger_Data$Cleanliness)
barplot(counts, main="satsification related to cleanliness ratings",
        xlab="cleanliness ratings", col=c("darkblue","red"),
        legend.text = c("dissatisfied customer","satisfied customers"))


# Kernel Density Plot of the age
denisty_plotting <- density(Passenger_Data$Age) # returns the density data 
plot(denisty_plotting) # plots the results

########### Relation between age 40 and legroom service (median) #################

age30  <- Passenger_Data$Age[Passenger_Data$Age == 40] 
legroom30 <- Passenger_Data$Leg_room_service[Passenger_Data$Age == 40 ]

# Grouped Bar Plot
counts <- table( legroom30, age30)
barplot(counts, main="Relation between age and legroom",
        xlab="Rating of Legroom service", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


############ Relation between age 80 and legroom service  #################

age80  <- Passenger_Data$Age[Passenger_Data$Age == 80] 
legroom80 <- Passenger_Data$Leg_room_service[Passenger_Data$Age == 80 ]

# Grouped Bar Plot
counts2 <- table( legroom80, age80)
barplot(counts2, main="Relation between age and legroom",
        xlab="Rating of Legroom service", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)











