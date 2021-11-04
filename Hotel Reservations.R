#Importing libraries
library(readxl)
library(dplyr)
library(randomForest)
library(ROCR)
library(car)
library(pROC)
library(caTools)
library(MASS)
library(brglm2)
library(vcd)
library(lmtest)
library(ggplot2)


#Importing data
setwd("C:/Users/tejksedopc/Desktop/Classes/Statistical Consulting")
hotel <- read_excel("reservations.xlsx")

### DATA CLEANING - PREPARATION - PREPROCESSING ###
summary(hotel) #4 NA values in children column.
hotel <- hotel[is.na(hotel$children)!=T,] #NA values are deleted. 4/119390 = negligibly small.

str(hotel) #Some character and variables needs to be changed into factors.
hotel$hotel <- as.factor(ifelse(hotel$hotel=="Resort Hotel", "Resort", "City"))
hotel$is_canceled <- as.factor(hotel$is_canceled)
hotel$arrival_date_year <- as.factor(hotel$arrival_date_year)
hotel$arrival_date_month <- factor(hotel$arrival_date_month,
                                   levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
hotel$meal <- as.factor(hotel$meal)
hotel$country <- as.factor(hotel$country)
hotel$market_segment <- as.factor(hotel$market_segment)
hotel$distribution_channel <- as.factor(hotel$distribution_channel)
hotel$is_repeated_guest <- as.factor(hotel$is_repeated_guest)
hotel$reserved_room_type <- as.factor(hotel$reserved_room_type)
hotel$assigned_room_type <- as.factor(hotel$assigned_room_type)
hotel$deposit_type <- as.factor(hotel$deposit_type)
hotel$customer_type <- as.factor(hotel$customer_type)
hotel$reservation_status <- as.factor(hotel$reservation_status)

#Too many NULL values. Are these variables even useful???
sum(hotel$agent=="NULL") # 16338/119386 = 13.69%
sum(hotel$company=="NULL") # 112589/119386 = 94.31%

#Issues with adr (daily rate) variable
nrow(hotel[hotel$adr<0,]) # 1 negative rate
nrow(hotel[hotel$adr==0,]) # 1959/119386 = 1.64% zero rate
hotel <- hotel[hotel$adr>0,] # They are deleted.
hotel <- hotel[hotel$adr!=5400,]

#Plots of variables
hist(hotel$lead_time)
plot(hotel$arrival_date_year)
plot(hotel$arrival_date_month) #Not alphabetically ordered
hist(hotel$arrival_date_week_number)
hist(hotel$stays_in_weekend_nights)
hist(hotel$stays_in_week_nights)
hist(hotel$adults); hist(hotel$children); hist(hotel$babies)
plot(hotel$meal)
plot(hotel$country)
plot(hotel$market_segment); plot(hotel$distribution_channel)
hist(hotel$previous_cancellations); hist(hotel$previous_bookings_not_canceled); hist(hotel$booking_changes)
plot(hotel$reserved_room_type); plot(hotel$assigned_room_type) #Small changes in between
hist(hotel$days_in_waiting_list)
plot(hotel$customer_type)
hist(hotel$adr); plot(hotel$adr) # One huge observation, it's deleted.


#Creation/deletion of new variables
hotel$is_weekend_included <- as.factor(ifelse(hotel$stays_in_weekend_nights==0, 0, 1))
hotel$stays_in_total_nights <- hotel$stays_in_week_nights+hotel$stays_in_weekend_nights
hotel$people <- hotel$adults+hotel$babies+hotel$children
hotel$reserved_assigned_same <- as.factor(ifelse(as.character(hotel$reserved_room_type)==as.character(hotel$assigned_room_type), 1, 0))
hotel <- hotel[, -31]
hotel$is_special_request <- as.factor(ifelse(hotel$total_of_special_requests==0, 0, 1))
hotel$total_cost <- hotel$stays_in_total_nights*hotel$adr

### POSSIBLE QUESTIONS ###
#When's the best period of the year to book a hotel? - OPTIMAL TRIP
#Optimal length of stay in order to get the best daily rate? - OPTIMAL TRIP
#Predict whether or not a hotel is gonna receive a disproportionately high number of special requests?
#Can we predict the possibility of a cancellation?
#Another questions...

### ANSWERS TO QUESTION: BEST PERIOD OF THE YEAR ###
#Time-wise

loc <- hotel %>% group_by(arrival_date_month,hotel) %>% summarize(price=mean(adr))
#Resort are more expensive in summer. In general, city hotels are more expensive.
ggplot(hotel) + geom_bar(aes(x=arrival_date_month, fill=4)) + 
        theme_bw()+ theme(legend.title=element_blank(),legend.text=element_blank(),legend.position="none",axis.title.x=element_blank()) + ylab("Number of reservations")

plot(~arrival_date_month, data=hotel, col=4, ylim=c(0,14000),ylab="Number of reservations", main="Monthly Reservations")

ggplot(hotel) + geom_bar(aes(x=arrival_date_week_number, fill=4)) + geom_vline(xintercept=seq(4,53,4.5),linetype=2) +
        theme_bw()+ theme(legend.title=element_blank(),legend.text=element_blank(),legend.position="none",axis.title.x=element_blank(), axis.text.x=element_blank()) + ylab("Number of reservations")
#Price-wise is similarly correlated.
adr_per_week <- hotel %>% group_by(arrival_date_week_number) %>% summarize(adr_week=round(mean(adr),2), adr_count=n())
cor(adr_per_week$adr_week, adr_per_week$adr_count) #0.82

### ANSWERS TO QUESTION: OPTIMAL TRIP ###

#Rates for each month
adr_per_month <- hotel %>% group_by(arrival_date_month) %>% summarize(adr_month=round(mean(adr),2))
adr_per_month <- data.frame(arrival_date_month=c(1:12),
                            adr_month=c(71.91,74.95,81.96,101.63,110.38,117.97,128.51,141.84,106.64,89.77,75.50,83.78))
barplot(adr_month~c(1:12), data=adr_per_month)

#Rates for each week
adr_per_week <- hotel %>% group_by(arrival_date_week_number) %>% summarize(adr_week=round(mean(adr),2), adr_count=n())
barplot(adr_month~arrival_date_week_number, data=adr_per_week)
#Summers are expensive, last week is expensive due to Christmas/Newyears

#Rates for each country
adr_per_country <- hotel %>% group_by(country) %>% summarize(adr_country=round(mean(adr),2)) %>% arrange(adr_country)
barplot(adr_country~c(1:178), data=adr_per_country)



#The more you stay, the better rate you have
plot(hotel$stays_in_week_nights, hotel$adr)
plot(hotel$stays_in_weekend_nights, hotel$adr)
plot(hotel$stays_in_total_nights, hotel$adr)
plot(hotel$stays_in_total_nights, hotel$total_cost)

#PLOTS ON THE REPORT
nightrates <- hotel %>% group_by(stays_in_total_nights) %>% summarize(daily_rate = mean(adr), total_rate=mean(sqrt(total_cost)))
plot(nightrates$stays_in_total_nights, nightrates$daily_rate, xlab="Length of stay (day)", ylab=" f (daily rate) ")
regLine(lm(nightrates$daily_rate~nightrates$stays_in_total_nights))
plot(nightrates$stays_in_total_nights,nightrates$total_rate, xlab="Length of stay(day)", ylab=" f (total cost) ")
regLine(lm(nightrates$total_rate~nightrates$stays_in_total_nights))

stay <- hotel %>% group_by(is_weekend_included) %>% summarize(lowest_rate=min(adr), lowest_cost=min(total_cost), lowest_days=min(stays_in_total_nights),
        average_rate=mean(adr), average_cost=mean(total_cost), average_days=mean(stays_in_total_nights),
        highest_rate=max(adr), highest_cost=max(total_cost), highest_days=max(stays_in_total_nights))

t.test(hotel[hotel$is_weekend_included==0,]$adr, hotel[hotel$is_weekend_included==1,]$adr)
t.test(hotel[hotel$is_weekend_included==0,]$total_cost, hotel[hotel$is_weekend_included==1,]$total_cost)
#Significant differences between weekends/noweekends

weekend <- hotel[hotel$is_weekend_included==1,]
mean(weekend[weekend$adr<105.3626+2*48.125 & weekend$adr>105.3626-2*48.125,]$adr)
mean(no_weekend[no_weekend$adr<101.0039+2*44.63667 & no_weekend$adr>101.0039-2*44.63667,]$adr)

### ANSWERS TO QUESTION: PREDICTION OF HIGH SPECIAL REQUESTS ###
unique(hotel$total_of_special_requests) # 0 1 2 3 4 5
nrow(hotel[hotel$total_of_special_requests==0,]) # 69139
nrow(hotel[hotel$total_of_special_requests==1,]) # 32724
nrow(hotel[hotel$total_of_special_requests==2,]) # 12762
nrow(hotel[hotel$total_of_special_requests==3,]) # 2436
nrow(hotel[hotel$total_of_special_requests==4,]) # 324
nrow(hotel[hotel$total_of_special_requests==5,]) # 40
percentages <- round(c(69139,32724,12762,2436,324,40)/117426*100,2)
#Plot
barplot(table(hotel$total_of_special_requests), ylim=c(0,70000),
        xlab="Number of Special Requests",ylab="Number of Customers", main="Special Requests of Customers", col=4)


#Dichotomization
plot(hotel$is_special_request)

#Logistic regression for special request
#Train-test division (80%, 20%)
set.seed(100)
sample <- sample.int(n=nrow(hotel), size=floor(0.8*nrow(hotel)))
train <- hotel[sample,]
test <- hotel[-sample,]
#Forward selection.
log_model_full <- glm(is_special_request~hotel+lead_time+arrival_date_month+arrival_date_week_number+arrival_date_day_of_month+stays_in_weekend_nights+stays_in_week_nights+adults+children+babies+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+is_weekend_included+stays_in_total_nights+people+total_cost, family=binomial(link="logit"), data=train)
summary(log_model_full)
log_model <- log_model_full %>% stepAIC(trace=F)
summary(log_model)
#Prediction
prob <- log_model %>% predict(newdata=test, type="response")
pred_class <- ifelse(prob > 0.5, 1,0)
table(pred_class, test$is_canceled)
roc(as.numeric(test$is_canceled),as.numeric(predict(log_model, newdata=test, type="response")), plot=T) #0.8533

#Bootstrap for balancing
nrow(hotel[hotel$is_special_request==0,]) #69140
nrow(hotel[hotel$is_special_request==1,]) #48286
hotel_special <- hotel[hotel$is_special_request==1,]
set.seed(100)
hotel_balanced <- rbind(hotel[hotel$is_special_request==0,],hotel_special[sample(nrow(hotel_special),69140,replace=T),])
nrow(hotel_balanced[hotel_balanced$is_special_request==0,]) #69140
nrow(hotel_balanced[hotel_balanced$is_special_request==1,]) #69140
#Train-test division (80%, 20%)
set.seed(100)
sample <- sample.int(n=nrow(hotel_balanced), size=floor(0.8*nrow(hotel_balanced)))
train <- hotel_balanced[sample,]
test <- hotel_balanced[-sample,]
rm(hotel_special)

#Random forest
#Finding optimal number of trees
trees <- c(seq(50,500,50))
auc_values <- c()
for (i in 1:length(trees)){
        rf_model <- randomForest(is_special_request~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_canceled+total_cost, data=train, ntree=trees[i])
        auc_values[i] <- roc(as.numeric(test$is_special_request),as.numeric(predict(rf_model, newdata=test)))$auc
}
#Optimal number of trees is 200
rf_model <- randomForest(is_special_request~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+is_weekend_included+stays_in_total_nights+total_cost, data=train, ntree=200)
roc(as.numeric(test$is_special_request),as.numeric(predict(rf_model, newdata=test)), plot=T) #0.8344
table(test$is_special_request,predict(rf_model, newdata=test)) #Find the accuracy here



#DO THE SAME THING, WITH DIVISION 01/2345, 012/345, 0123/45, 01234/5
hotel$is_special_request <- as.factor(ifelse(hotel$total_of_special_requests<=1, 0, 1))
nrow(hotel[hotel$is_special_request==0,]) #101864
nrow(hotel[hotel$is_special_request==1,]) #15562
hotel_special <- hotel[hotel$is_special_request==1,]
set.seed(100)
hotel_balanced <- rbind(hotel[hotel$is_special_request==0,],hotel_special[sample(nrow(hotel_special),101864,replace=T),])
nrow(hotel_balanced[hotel_balanced$is_special_request==0,]) #101864
nrow(hotel_balanced[hotel_balanced$is_special_request==1,]) #101864
#Train-test division (80%, 20%)
set.seed(100)
sample <- sample.int(n=nrow(hotel_balanced), size=floor(0.8*nrow(hotel_balanced)))
train <- hotel_balanced[sample,]
test <- hotel_balanced[-sample,]
rm(hotel_special)
#Finding optimal number of trees
trees <- c(seq(50,500,50))
auc_values <- c()
for (i in 1:length(trees)){
        rf_model <- randomForest(is_special_request~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_canceled+total_cost, data=train, ntree=trees[i])
        auc_values[i] <- roc(as.numeric(test$is_canceled),as.numeric(predict(rf_model, newdata=test)))$auc
}
#All auc are the same. Use 50 trees for simplicity. Optimal number of trees is 50
rf_model <- randomForest(is_special_request~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_canceled+total_cost, data=train, ntree=50)
roc(as.numeric(test$is_canceled),as.numeric(predict(rf_model, newdata=test)), plot=T) #0.6112
t <- table(test$is_canceled,predict(rf_model, newdata=test)) #Find the accuracy here

hotel$is_special_request <- as.factor(ifelse(hotel$total_of_special_requests<=2, 0, 1))
nrow(hotel[hotel$is_special_request==0,]) #114626
nrow(hotel[hotel$is_special_request==1,]) #2800
hotel_special <- hotel[hotel$is_special_request==1,]
set.seed(100)
hotel_balanced <- rbind(hotel[hotel$is_special_request==0,],hotel_special[sample(nrow(hotel_special),114626,replace=T),])
nrow(hotel_balanced[hotel_balanced$is_special_request==0,]) #114626
nrow(hotel_balanced[hotel_balanced$is_special_request==1,]) #114626
#Train-test division (80%, 20%)
set.seed(100)
sample <- sample.int(n=nrow(hotel_balanced), size=floor(0.8*nrow(hotel_balanced)))
train <- hotel_balanced[sample,]
test <- hotel_balanced[-sample,]
rm(hotel_special)
#Finding optimal number of trees
trees <- c(seq(50,500,50))
auc_values <- c()
for (i in 1:length(trees)){
        rf_model <- randomForest(is_special_request~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_canceled+total_cost, data=train, ntree=trees[i])
        auc_values[i] <- roc(as.numeric(test$is_canceled),as.numeric(predict(rf_model, newdata=test)))$auc
}
gc()
#All auc are the same. Use 50 trees for simplicity. Optimal number of trees is 50
rf_model <- randomForest(is_special_request~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_canceled+total_cost, data=train, ntree=50)
roc(as.numeric(test$is_canceled),as.numeric(predict(rf_model, newdata=test)), plot=T) #0.6328
table(test$is_canceled,predict(rf_model, newdata=test)) #Find the accuracy here

#No specific improvement.

#Feature selection for random forest
#Since ROC and accuracies are similar across different number of trees, forward selection is tested to achieve a good model.
rf_model <- randomForest(total_of_special_requests~market_segment+deposit_type+adults+
                                 customer_type+previous_bookings_not_canceled+
                                 babies+lead_time+required_car_parking_spaces+
                                 distribution_channel+reserved_assigned_same+
                                 is_repeated_guest+meal+booking_changes+
                                 days_in_waiting_list+previous_cancellations+adr+
                                 stays_in_total_nights+children+total_cost+is_weekend_included+
                                 hotel, data=train, ntree=100)
roc(as.numeric(test$is_canceled),as.numeric(predict(rf_model, newdata=test))) #0.6112
t <- table(test$is_canceled,predict(rf_model, newdata=test)); (t[1,1]+t[2,2])/sum(t)










### POISSON REGRESSION ###
#Goodness of fit test for poisson distribution.
special <- hotel$total_of_special_requests
observed <- table(special)
expected <- dpois(0:5, lambda=mean(special))*length(special)
chisq <- sum((observed-expected)**2/expected)
p <- pchisq(chisq, df=length(observed)-2)
#P-value > 0.05. It is poisson distribution.

#Poisson regression model with forward selection.
pois_model <- glm(total_of_special_requests~market_segment+deposit_type+adults+
                          customer_type+previous_bookings_not_canceled+
                          babies+lead_time+required_car_parking_spaces+
                          distribution_channel+reserved_assigned_same+
                          is_repeated_guest+meal+booking_changes+
                          days_in_waiting_list+previous_cancellations+adr+
                          stays_in_total_nights+children+total_cost+is_weekend_included+
                          hotel, family="poisson", data=hotel)
summary(pois_model)

#Deviance test
dev <- summary(pois_model)$deviance
df <- summary(pois_model)$df.residual
p_value <- 1-pchisq(dev,df) #The model is accurate

#Overdispersion
special <- hotel$total_of_special_requests
sd(special)**2 > mean(special) #True. Overdispersion exists.
anova(pois_model)

quasipois_model <- glm(total_of_special_requests~market_segment+deposit_type+adults+
                          customer_type+previous_bookings_not_canceled+
                          babies+lead_time+required_car_parking_spaces+
                          distribution_channel+reserved_assigned_same+
                          is_repeated_guest+meal+booking_changes+
                          days_in_waiting_list+previous_cancellations+adr+
                          stays_in_total_nights+children+total_cost+is_weekend_included+
                          hotel, family="quasipoisson", data=hotel)
summary(quasipois_model) #Standard errors are smaller

nb_model <- glm.nb(total_of_special_requests~market_segment+deposit_type+adults+
                           customer_type+previous_bookings_not_canceled+
                           babies+lead_time+required_car_parking_spaces+
                           distribution_channel+reserved_assigned_same+
                           is_repeated_guest+meal+booking_changes+
                           days_in_waiting_list+previous_cancellations+adr+
                           stays_in_total_nights+children+total_cost+is_weekend_included+
                           hotel, data=hotel)
summary(nb_model)



#Train-test-prediction
hsr0 <- hotel[hotel$total_of_special_requests==0,]; sample0 <- sample.int(n=nrow(hsr0), size=floor(0.8*nrow(hsr0))); train0 <- hsr0[sample0,]; test0 <- hsr0[-sample0,]
hsr1 <- hotel[hotel$total_of_special_requests==1,]; sample1 <- sample.int(n=nrow(hsr1), size=floor(0.8*nrow(hsr1))); train1 <- hsr1[sample1,]; test1 <- hsr1[-sample1,]
hsr2 <- hotel[hotel$total_of_special_requests==2,]; sample2 <- sample.int(n=nrow(hsr2), size=floor(0.8*nrow(hsr2))); train2 <- hsr2[sample2,]; test2 <- hsr2[-sample2,]
hsr3 <- hotel[hotel$total_of_special_requests==3,]; sample3 <- sample.int(n=nrow(hsr3), size=floor(0.8*nrow(hsr3))); train3 <- hsr3[sample3,]; test3 <- hsr3[-sample3,]
hsr4 <- hotel[hotel$total_of_special_requests==4,]; sample4 <- sample.int(n=nrow(hsr4), size=floor(0.8*nrow(hsr4))); train4 <- hsr4[sample4,]; test4 <- hsr4[-sample4,]
hsr5 <- hotel[hotel$total_of_special_requests==5,]; sample5 <- sample.int(n=nrow(hsr5), size=floor(0.8*nrow(hsr5))); train5 <- hsr5[sample5,]; test5 <- hsr5[-sample5,]
train <- rbind(train0,train1,train2,train3,train4,train5)
test <- rbind(test0,test1,test2,test3,test4,test5)
rm(hsr0,hsr1,hsr2,hsr3,hsr4,hsr5, sample0,sample1,sample2,sample3,sample4,sample5, train0,train1,train2,train3,train4,train5, test0,test1,test2,test3,test4,test5)
pois_model_train <- glm(total_of_special_requests~market_segment+deposit_type+adults+
                                customer_type+previous_bookings_not_canceled+
                                babies+lead_time+required_car_parking_spaces+
                                distribution_channel+reserved_assigned_same+
                                is_repeated_guest+meal+booking_changes+
                                days_in_waiting_list+previous_cancellations+adr+
                                stays_in_total_nights+children+total_cost+is_weekend_included+
                                hotel, family="poisson", data=train)
quasipois_model_train <- glm(total_of_special_requests~market_segment+deposit_type+adults+
                               customer_type+previous_bookings_not_canceled+
                               babies+lead_time+required_car_parking_spaces+
                               distribution_channel+reserved_assigned_same+
                               is_repeated_guest+meal+booking_changes+
                               days_in_waiting_list+previous_cancellations+adr+
                               stays_in_total_nights+children+total_cost+is_weekend_included+
                               hotel, family="quasipoisson", data=train)
nb_model_train <- glm.nb(total_of_special_requests~market_segment+deposit_type+adults+
                           customer_type+previous_bookings_not_canceled+
                           babies+lead_time+required_car_parking_spaces+
                           distribution_channel+reserved_assigned_same+
                           is_repeated_guest+meal+booking_changes+
                           days_in_waiting_list+previous_cancellations+adr+
                           stays_in_total_nights+children+total_cost+is_weekend_included+
                           hotel, data=train)
summary(quasipois_model_train) #Standard errors are smaller
summary(pois_model_train)
summary(nb_model_train)
predictions_quasi <- round(exp(predict(pois_model_train, newdata=test)),0)
predictions_pois <- round(exp(predict(quasipois_model_train, newdata=test)),0)
predictions_nb <- round(exp(predict(nb_model_train, newdata=test)),0)
error_quasi <- mean(abs(predictions_quasi-test$total_of_special_requests))
error_pois <- mean(abs(predictions_pois-test$total_of_special_requests))
error_nb <- mean(abs(predictions_nb-test$total_of_special_requests))
error_quasi_2 <- mean((predictions_quasi-test$total_of_special_requests)**2)
error_pois_2 <- mean((predictions_pois-test$total_of_special_requests)**2)
error_nb_2 <- mean((predictions_nb-test$total_of_special_requests)**2)
#Comparison of poisson-quasipoisson-negativebinomial
comparison <- round(data.frame(Poisson=coef(pois_model_train)[1:length(coef(pois_model_train))], Quasi=coef(quasipois_model_train), NegBin=coef(nb_model_train),
                               se.Poisson=summary(pois_model_train)$coefficients[,2], se.Quasi=summary(quasipois_model_train)$coefficients[,2], se.NegBin=summary(nb_model_train)$coefficients[,2]),4)

#Quasipoison has lower std.errors, it means it's more precise.
#Quasi is the best

### ANSWERS TO QUESTION: PREDICTION OF CANCELLATION ###
plot(hotel$is_canceled)
#Factors that may affect cancellation
#Classification methods: random forest, logistic regression

#Logistic regression
#Train-test division (80%, 20%)
set.seed(100)
sample <- sample.int(n=nrow(hotel), size=floor(0.8*nrow(hotel)))
train <- hotel[sample,]
test <- hotel[-sample,]
#Forward selection.
log_model_full <- glm(is_canceled~hotel+lead_time+arrival_date_month+arrival_date_week_number+arrival_date_day_of_month+stays_in_weekend_nights+stays_in_week_nights+adults+children+babies+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+people+is_special_request+total_cost, family=binomial(link="logit"), data=train)
summary(log_model_full)
log_model <- log_model_full %>% stepAIC(trace=F)
summary(log_model)
#Prediction
prob <- log_model %>% predict(newdata=test, type="response")
pred_class <- ifelse(prob > 0.5, 1,0)
table(pred_class, test$is_canceled)
roc(as.numeric(test$is_canceled),as.numeric(predict(log_model, newdata=test, type="response")), plot=T) #0.8533


#Bootstrap for balancing
nrow(hotel[hotel$is_canceled==0,]) #73419
nrow(hotel[hotel$is_canceled==1,]) #44007
hotel_cancel <- hotel[hotel$is_canceled==1,]
set.seed(100)
hotel_balanced <- rbind(hotel[hotel$is_canceled==0,],hotel_cancel[sample(nrow(hotel_cancel),73419,replace=T),])
nrow(hotel_balanced[hotel_balanced$is_canceled==0,]) #73419
nrow(hotel_balanced[hotel_balanced$is_canceled==1,]) #73419
#Train-test division (80%, 20%)
set.seed(100)
sample <- sample.int(n=nrow(hotel_balanced), size=floor(0.8*nrow(hotel_balanced)))
train <- hotel_balanced[sample,]
test <- hotel_balanced[-sample,]
rm(hotel_cancel)

#Random forest
#Finding optimal number of trees
trees <- c(seq(50,500,50))
auc_values <- c()
for (i in 1:length(trees)){
        rf_model <- randomForest(is_canceled~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_special_request+total_cost, data=train, ntree=trees[i])
        auc_values[i] <- roc(as.numeric(test$is_canceled),as.numeric(predict(rf_model, newdata=test)))$auc
}
#Optimal number of trees is 200
rf_model <- randomForest(is_canceled~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+days_in_waiting_list+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_special_request+total_cost, data=train, ntree=200)
roc(as.numeric(test$is_canceled),as.numeric(predict(rf_model, newdata=test, type="response")), plot=T) #0.8902
table(test$is_canceled,predict(rf_model, newdata=test, type="response")) #Find the accuracy here
#Backward selection for better model
importance <- rf_model$importance
rf_model_t <- randomForest(is_canceled~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+previous_cancellations+booking_changes+deposit_type+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_special_request+total_cost, data=train, ntree=200)
roc(as.numeric(test$is_canceled),as.numeric(predict(rf_model_t, newdata=test, type="response")), plot=T) #0.8902
table(test$is_canceled,predict(rf_model_t, newdata=test, type="response")) #Find the accuracy here


### ANSWERS TO QUESTION: OTHER... ###




#### MODEL COEFFICIENT IMPORTANCE
logistic <- glm(is_canceled~hotel+lead_time+arrival_date_month+arrival_date_week_number+arrival_date_day_of_month+stays_in_weekend_nights+stays_in_week_nights+adults+children+babies+meal+market_segment+distribution_channel+is_repeated_guest+previous_cancellations+previous_bookings_not_canceled+booking_changes+deposit_type+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_special_request+total_cost, family=binomial(link="logit"), data=train)
summary(logistic)
logcoef <- sort(logistic$coefficients)
random_forest <- randomForest(is_canceled~hotel+lead_time+arrival_date_week_number+arrival_date_month+stays_in_total_nights+people+meal+market_segment+distribution_channel+previous_cancellations+booking_changes+deposit_type+customer_type+adr+required_car_parking_spaces+total_of_special_requests+is_weekend_included+stays_in_total_nights+reserved_assigned_same+is_special_request+total_cost, data=train, ntree=200)
sort(random_forest$importance)
random_forest$terms
# Cancellation
        # Positive: Longer stays, more people,full_board (entitlement), previous cancellations, adr(totalcost)
        # Negative: repeated quests, previous_not_canceled, booking changes (deðiþiklik yapýyorsa iptal etmez), special request (özel isteði varsa planlýdýr, iptal etmez)


quasipoisson <- glm(total_of_special_requests~market_segment+deposit_type+adults+
                            customer_type+previous_bookings_not_canceled+
                            babies+lead_time+required_car_parking_spaces+
                            distribution_channel+reserved_assigned_same+
                            is_repeated_guest+meal+booking_changes+
                            days_in_waiting_list+previous_cancellations+adr+
                            stays_in_total_nights+children+total_cost+is_weekend_included+
                            hotel, family="quasipoisson", data=train)
exp(quasipoisson$coefficients)
# Special Request: Online (özel seçim kolay), repeated (müdavim, talebi bol), booking_changes, total night, weekend included

