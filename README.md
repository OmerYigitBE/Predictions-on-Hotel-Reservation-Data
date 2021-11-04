# Predictions on Hotel Reservation Data

The reservations dataset contains booking information of the customers from a city hotel and a resort hotel, from 2014 October to 2017 September, spanning across three years. The raw data has information about more than 100.000 reservations and 32 variables like date, number of days, daily rate, special requests (twin bed, high floor…) etc. The dataset is analyzed from two perspectives: Customer perspective and hotel management perspective. Naturally, customers and hotel management are interested in answering different questions. Using this dataset, those questions are tried to be answered and certain conclusions are reached.

The detailed analysis and results can be found in the report. Also, the datased and codes are provided. Below is the brief summary:

### Customer Perspective
The answer to these 2 questions are sought:
- When is the best period of the year to book a hotel?
- What is the optimal length of stay in order to get the best daily rate?

The methods of descriptive statistics were sufficient.The following conclusions are reached:
  - A month when the number of reservations is high also has higher daily rate.
  - City hotels are more expensive than resort hotels, except summer. Also, weekends are more
expensive than weekdays.
  - An off-season holiday at a resort is the cheapest option. Early September can also be a good
alternative for a late-summer holiday.
  - A 5 day-long holiday without a weekend is the cheapest option. A 12 day-long holiday
without the second weekend can also be a good alternative

### Hotel Management Perspective
The answer to these 2 questions are sought:
  - Can we predict whether a customer would cancel the reservation or not?
  - Can predict how many special requests a customer would have?

For predicting cancellations, a logistic regression and a random forest models are fitted to the data. Then, the results are compared. Random forest model is found to be better at predicting the cancellations with 90% success.

For predicting special requests, a poisson regression model is fitted. According to the model outputs, average prediction error in absolute values (mean absolute error – MAE) is found to be 0.45 and in squared values (mean square error – MSE) is found to be 1.64. Hence, this poisson regression model can successfully be used for future predictions for the number of special requests a customer would have.

In general, the following conclusions are reached from the hotel management perspective:
- Longer reservations, reservations with more people, and reservations made by one who has cancelled a reservation before are indicators of a possible cancellation. Also, loyal customers who have had reservations before would most likely not to cancel their reservations.
- Online reservations include more special requests, in general, due to easiness. Also, regular customers can be more demanding in terms of special requests.
