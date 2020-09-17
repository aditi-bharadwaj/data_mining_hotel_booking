#reading the file
read.csv("D:/Minning project/Data Set/hotel_bookings.csv",header = TRUE)-> hotel_booking
View(hotel_booking)
#checking the datatype of variables
str(hotel_booking)
library(dplyr)
colnames(hotel_booking)
#Checking if the data is balanced or not
prop.table(table(hotel_booking$is_canceled))
#checking for any null value
sapply(hotel_booking, function(x) sum(grepl("NULL", x)))
#removing the columns which are not usefull and contains null value
hotel_booking$meal <- NULL
hotel_booking$agent <- NULL
hotel_booking$company <- NULL
hotel_booking$arrival_date_year <- NULL
hotel_booking$arrival_date_month <- NULL
hotel_booking$arrival_date_week_number <- NULL
hotel_booking$arrival_date_day_of_month <- NULL
hotel_booking$required_car_parking_spaces <- NULL
hotel_booking$children <- NULL
hotel_booking$country <- NULL
hotel_booking$distribution_channel <- NULL
hotel_booking$reservation_status_date <- NULL
summary(hotel_booking)
str(hotel_booking)
#plotting the graph to see the cancellation in city and resort hotel
library(tidyverse)
ggplot(data = hotel_booking,aes(is_canceled))+ geom_histogram(binwidth = 0.5, col='black', fill='blue', alpha = 0.4) + facet_wrap(~hotel)
#checking weather people prfer city hotel or resort hotel
#library(dplyr)
hotel_booking%>%count(hotel)%>%ggplot(aes(hotel,n)) + geom_bar(stat = 'identity', fill = 'blue', alpha = 0.4, col = "black") +
  xlab('Hotels ') + ylab('Frecuency')
#graph against cancellation on leadtime
ggplot(data = hotel_booking , aes(lead_time)) + geom_histogram(binwidth = 0.8) + facet_wrap(~ is_canceled) 


hotel_booking%>%group_by(arrival_date_month)%>%summarise(Count = n())%>%arrange(-Count)%>%ggplot(aes(x = arrival_date_month, y = Count)) +
  geom_bar(stat = 'identity',fill = "dodgerblue") + coord_flip() + geom_text(aes(x =arrival_date_month, y=0.02, label= Count),
                                                                             hjust=-1, vjust=0, size=4, 
                                                                             colour="black", fontface="bold",
                                                                             angle=360)
plot(x=hotel_booking$is_canceled, y=hotel_booking$lead_time) 
#####
#converting is canceeled to factor
#hotel_booking$is_canceled <- ifelse(hotel_booking$is_canceled=="Yes", 1, 0)
hotel_booking$is_canceled= factor(hotel_booking$is_canceled)
summary(hotel_booking$is_canceled)
prop.table(table(hotel_booking$is_canceled))
View(hotel_booking)
str(hotel_booking)
#converting is canceeled to factor
#hotel_booking$is_canceled <- ifelse(hotel_booking$is_canceled=="Yes", 1, 0)
#hotel_booking$is_canceled= factor(hotel_booking$is_canceled)
View(hotel_booking)
str(hotel_booking)
library(tidyverse)
library(dplyr)
library(caret)
library(lattice)
library(ggplot2)
library(magrittr)
#checking data balanced or imbalanced
prop.table(table(hotel_booking$is_canceled))
# Visualize the distribution
library(tidyverse)
ggplot(data = hotel_booking, aes(x = hotel)) +
  geom_bar(stat = "count") +
  labs(title = "Booking Request by Hotel type",
       x = "Hotel type",
       y = "No. of bookings") +
  theme_classic() + scale_color_brewer(palette = "Set2")
# Check the distribution of hotel type for cancellation
table(hotel_booking$is_canceled, hotel_booking$hotel)
# Visualize the cancellation by hotel type
ggplot(data = hotel_booking,
       aes(
         x = hotel,
         y = prop.table(stat(count)),
         fill = factor(is_canceled),
         label = scales::percent(prop.table(stat(count)))
       )) +
  geom_bar(position = position_dodge()) +
  geom_text(
    stat = "count",
    position = position_dodge(.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cancellation Status by Hotel Type",
       x = "Hotel Type",
       y = "Count") +
  theme_classic() +
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Cancelled", "Not Cancelled")
  )
# Cancellation ratio by Hotel Type based on the lead time. Lead time is the time gap between
# Booking made and the actual date of check in. We will visualize the data by using BoxPlot

ggplot(data = hotel_booking, aes(
  x = hotel,
  y = lead_time,
  fill = factor(is_canceled)
)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Cancellation By Hotel Type",
    subtitle = "Based on Lead Time",
    x = "Hotel Type",
    y = "Lead Time (Days)"
  ) +
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Cancelled", "Not Cancelled")
  ) + theme_light()
# Organize the Month in proper order
hotel_booking$arrival_date_month <-
  factor(hotel_booking$arrival_date_month, levels = month.name)
#checking corelation
##first define the function to apply
Chsq <- function(x){
  ## input is a row of your data
  ## creating a table from each row
  x <- matrix(as.factor(x))
  ### this will return the p value
  return(chisq.test(x,hotel_booking$is_canceled)$p.value)
}
## Now apply this function
chi_data = hotel_booking
chi_data
## by using as.vector convert the output into a vector
P_Values <- as.vector(sapply(chi_data[c(2,4,5,6,7,10,12,13,14,15,17,19,20)],FUN=Chsq))
P_Values

chi<- names(which(sapply(hotel_booking, class) == "factor"))
chi
chi4 <- sapply(hotel_booking[,chi], function(x) chisq.test(hotel_booking$is_canceled, x))
chi4
#chi1=hotel_booking[,chi]
#for(i in 2:length(chi)-1){
#  print(i)
  #chi3=table(chi1[,2],chi1[,i])
  
#  print(chi[i])
#  print(chisq.test(chi1[,2],chi1[,i]))
#}

#result <- cbind(rownames(chi_data),P_Values)
#result
#dividing data into train and test set
library(caTools)
set.seed(123)
split = sample.split(hotel_booking$is_canceled, SplitRatio = 0.75)
training_set = subset(hotel_booking, split == TRUE)
test_set = subset(hotel_booking, split == FALSE)

#library(polycor)
#library(mvtnorm)
#polS <- polyserial(hotel_booking$lead_time,hotel_booking$is_canceled, ML=TRUE, std.err=TRUE)
#polS
#polC <- polychor(hotel_booking$is_repeated_guest,hotel_booking$is_canceled, ML=TRUE, std.err=TRUE)
#polS
#random forest
library(randomForest)
library(rpart)
set.seed(123)
classifier = randomForest(x = training_set[-c(2,4,5,6,7,10,12,13,14,15,17,19,20)],
                          y = training_set$is_canceled,
                          ntree = 501)




# Predicting the Test set results
y_pred_random_forest = predict(classifier, newdata = test_set[-c(2,4,5,6,7,10,12,13,14,15,17,19,20)])
y_pred_random_forest

# Making the Confusion Matrix
cm = table(test_set[-c(2,4,5,6,7,8,10,11,12,13,14,15,17,19,20)], y_pred_random_forest)
cm
library(caret)
confusionMatrix(table(test_set$is_canceled, y_pred_random_forest), mode = 'everything')
str(test_set)
#Plottingg ROC curve 
library(pROC)
library(ggplot2)
rocobj <- roc(as.numeric(test_set$is_canceled), as.numeric(y_pred_random_forest))
g <- ggroc(rocobj)
g  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve random forest") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")

#Applying logistic regression
classifier = glm(formula =  is_canceled ~ lead_time+hotel+is_repeated_guest+days_in_waiting_list+adr,
                 family = binomial,
                 data = training_set)
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-2])
y_pred = ifelse(prob_pred > 0.5, 1, 0)
y_pred
# Making the Confusion Matrix
cm = table(test_set$is_canceled, y_pred)
cm
library(caret)
confusionMatrix(table(test_set$is_canceled, y_pred),mode = 'everything')
#making ROC curve
library(pROC)
library(ggplot2)
rocobj1 <- roc(as.numeric(test_set$is_canceled), as.numeric(y_pred))
g <- ggroc(rocobj1)
g  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve logistic regression") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")
#comparision of ROC curve
g2 <- ggroc(list(y_pred_random_forest=rocobj, y_pred=rocobj1))
g2
g2 <- ggroc(list(random_forest=rocobj, logistic_regression=rocobj1 ),size=1.5)
g2  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve Comparison") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")
