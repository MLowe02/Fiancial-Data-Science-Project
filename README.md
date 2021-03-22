# Fiancial-Data-Science-Project
This is a project I did as a test for a company. A detailed report is presented in Time Series Problem.html
The basic scenario is that we are given mock transaction and purchase history of individual customers for a six month period.
Each transaction is processed through a specific vendor, with each vendor taking a different cut of the revenue from the purchase. 

At the end of March, vendor 37 closes. This is one of the vendors that takes a high percentage of the revenue cut. 
The question, than, is how much is the revenue affected by the closure of vendor 37 specifically? Is there value to the company in reopening this vendor?

We start with doing some general EDA and data manipulation. Than, in the time series script, we fit a Linear Regression with residuals modeled as an ARIMA process.
This model is validated and used to forecast revenue. A comparison is made between situations where vendor 37 closed and where vendor 37 didn't close.
