#load packages
library(here)
library(tidyverse)

#import data as tibbles
ActiveUsers <- read_csv(here("m_mock_active_users_20190621.csv"))
summary(ActiveUsers)

Transactions <- read_csv(here("m_mock_transaction_20190621.csv"))
summary(Transactions)

#37 missing values for vendors here
VendorMap <- read_csv(here("m_vendor_map.csv"))

# Preliminary exploration -------------------------------------------------

##We run into an initial roadblock of only 6 vendors having a known % taken off the price
##lets look at how this is distributed
Transactions$vendor %>% table()#total of 729200 transactions over the months
Transactions$revenue %>% sum #total unadjusted revenue of 40,938,991

#The difference in scale is so large, ggplot has to represent the y axis in scientific notation!
ggplot(data = Transactions) + geom_bar(mapping = aes(x=vendor))+ scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


#with a total of 729200 transactions over 6 months, lets see how much percentage is accounted for by the vendors we know
Transactions_KnownVendor <- Transactions %>% filter(
  vendor == "vendor1" |
    vendor == "vendor11" |
    vendor == "vendor26" |
    vendor == "vendor33" |
    vendor == "vendor36" |
    vendor == "vendor37" |
    vendor == "vendor42"
) 

##so approximately 99.77% of all transactions in this 6 month period goes through a vendor that we know the percentage cut for
Transactions_KnownVendor$vendor %>% table() %>% sum/729200

#99.72% of unadjusted revenue comes through one of the vendors we know
Transactions_KnownVendor$revenue %>% sum/40938991

#Therefore, I am comfortable ommitting the vendors we dont know

# Adjust revenue by percent taken ------------------------------------------------

Transactions_KnownVendor <- Transactions_KnownVendor %>% 
  group_by(vendor) %>% #case_when essentially acts as a elegant ifelse statement
  mutate(AdjRev = case_when(vendor == "vendor1" ~ revenue-revenue*0.10,
                            vendor == "vendor11" ~ revenue-revenue*0.05,
                            vendor == "vendor26" ~ revenue-revenue*0.20,
                            vendor == "vendor33" ~ revenue-revenue*0.22,
                            vendor == "vendor36" ~ revenue-revenue*0.25,
                            vendor == "vendor37" ~ revenue-revenue*0.30,
                            vendor == "vendor42" ~ revenue-revenue*0.20,)) %>%
  ungroup

# Before closure - what did revenue look like for vendor37?
#.    Proportion of total revenue
#.    Revenue/transactions per day through vendor37
#.    Regional usage details

##Isolate data for months 1-3
MonthsBefore37Closure <- Transactions_KnownVendor %>% filter(month <= 3)

#What is the proportion of total adjusted revenue look like?
MonthsBefore37Closure %>% 
  group_by(vendor) %>%
  summarise(TotalRevenue = sum(AdjRev)) %>% #create a summary table with the total adjusted revenue of each vendor
  mutate(percent = prop.table(TotalRevenue)*100) %>% arrange(desc(percent)) #add a column to the table for the percent of each vendor
##We can see that vender 37 makes up only 0.62% of the adjusted revenue



#Revenue/transactions per day
Vend37 <- MonthsBefore37Closure %>% filter(vendor == "vendor37")
##As each row is a a single day, we can use simple row operations
#Find the mean per day of revenue
mean(Vend37$revenue)
#average number of transactions a day, 
#found by total number of transactions over the total days in the three months
nrow(Vend37)/(31+29+31)

#Countries affected by closure?
Vend37$country_code %>% table
#Only 6 countries have more than 100 transactions through vender 37
Vend37 %>% group_by(country_code) %>% filter(n() > 100) %>% .$country_code %>% table
#transactions from all other countires, combined
Vend37 %>% group_by(country_code) %>% filter(n() < 100) %>% .$country_code %>% table %>% sum


# Upper bound for loss ----------------------------------------------------

#Note that since there is 
#A simple t-test between the adjusted revenue in month 3 vs month 4 would suffice
Month3Revenue <- 
  Transactions_KnownVendor %>% 
  filter(month == 3) %>%
  select(AdjRev)

Month4Revenue <-
  Transactions_KnownVendor %>%
  filter(month == 4) %>%
  select(AdjRev)

RevenueTest <- t.test(Month3Revenue, Month4Revenue)
RevenueTest
#in the transition month, we have an upper bound of $2.53 of revenue lost per transaction
#a naive estimate of the revenue lost than would be the upper bound times the number of transactions
RevenueTest$conf.int[2] * nrow(Month4Revenue)
#so an estimated $280554 is lost following the closure

#We can also compare the average revenue in the three month period before 37 closure to after

#subset data for the 3 months after 37 closure
MonthsAfter37Closure <- Transactions_KnownVendor %>% filter(month > 3)

#We see a significant difference in the average revenue before the closure of vendor 37 and after
t.test(MonthsBefore37Closure$AdjRev, MonthsAfter37Closure$AdjRev)
#There is a similar upper bound of $2.77 lost per transaction in the months after closure


# Number of users affected by the closure ---------------------------------

#straightforward question, just take the count of unique users of vendor 37 before closure
MonthsBefore37Closure %>% 
  filter(vendor == "vendor37") %>% 
  select(account) %>% unique %>% nrow

#1215 users made purchases through vendor37 in the months before the closure




# Is there a measurable revenue change as a result of the closure  --------
# This question cannot be answered in the exploratory data analysis stage
# While we can see, from the earlier t-test, that there is a statistically 
# significant difference (t = 19.67, df = 240432, p < 0.0001) between revenue 
# before and after vendor 37 closed, this *does not* imply that the vendor37 closure 
# caused the decrease in revenue, or was even associated with it. It is possible that there is a confounding
# variable


#What about regionally?
#Well, we established before that there were only really 6 countries that had 100 transactions over the three months prior
#these are country16, country214, country217,  country45,  country51, and country66

##First, lets write a function that takes a list of country codes and applies the same operations to all of them
countries <- list("country16","country45","country51", "country66", "country214", "country217")
lapply(countries, function(x){ 
  Month3rev <- Transactions_KnownVendor %>% 
    filter(month == 3 & country_code == x) %>%
    .$AdjRev
  Month4rev <- Transactions_KnownVendor %>% 
    filter(month == 4 & country_code == x) %>%
    .$AdjRev
  t.test(Month3rev, Month4rev)
})


# Comparing ARPU of vendor37 users to other users ---------------
#lets get a list of all users of vendor 37
Vendor37Users <- Transactions_KnownVendor %>%
  filter(vendor == "vendor37") %>%
  .$account %>%
  unique()

#Now, lets find all the former vend37 users
MonthsAfter37Closure %>% 
  filter(account %in% Vendor37Users)
#Uh oh. We can immediately see from this table that there are only 205 transactions from former vendor37 users
MonthsAfter37Closure %>%
  filter(account %in% Vendor37Users) %>%
  .$AdjRev %>%
  sum(.)/
MonthsAfter37Closure %>%
  filter(account %in% Vendor37Users) %>%
  .$account %>% 
  unique %>% 
  length

    
MonthsAfter37Closure %>% 
  filter(!(account %in% Vendor37Users)) %>% 
  .$AdjRev %>% 
  sum()/
MonthsAfter37Closure %>%
  filter(!(account %in% Vendor37Users)) %>%
  .$account %>% 
  unique %>% 
  length

#So we can see that the ARPU of the former vendor 37 users is on average higher than users of all other vendors during the 3 month period following 
#the closure of Vendor 37. Notably, however, only 98 of the 1215 users have migrated to other vendors

#taking a closer look at where former vendor37 units have moved to


MonthsAfter37Closure %>% filter(account %in% Vendor37Users) %>% 
  .$vendor%>% table()
#So we can clearly see that most of the transactions being made by former 37 users are being made through vendor37 now
  
#Personally, I doubt that the closure of vendor 37 caused a significant decrease in profits, unless this closure was associated with a substantially bad 
#event that soured community opinion. This is due to the very small ammount of adjusted revenue that vendor37 accounted for before the closure. Remember, vendor 37
#only made up 0.68% of the total revenue in the months before its closure
  
  #hol up a minute why tf is there only 100 unique sku????
Transactions_KnownVendor %>% select(sku) %>% unique()
#73 sku bought
MonthsBefore37Closure %>% select(sku) %>% unique
#85 sku bought
MonthsAfter37Closure %>% select(sku) %>% unique

#list of sku sold in the months before closure
wut <- MonthsBefore37Closure %>% .$sku %>% unique
#sku that were sold after closure
#58
MonthsAfter37Closure %>% filter(sku %in% wut) %>% select(sku) %>% unique

#So, that means that 15 sku stopped being sold (or bought) after 37 closure, and 27 new sku were sold
#what if the new sku didnt do as well as the 15 that stopped being sold?
#lets look at proportion of revenue by sku 
MonthsBefore37Closure %>% 
  group_by(sku) %>%
  summarise(TotalRevenue = sum(AdjRev)) %>% #create a summary table with the total adjusted revenue of each vendor
  mutate(percent = prop.table(TotalRevenue)*100) %>% arrange(desc(percent)) #add a column to the table for the percent of each vendor

#so we can see a couple of heavy hitters here, with the first three sku accounting for 67.3% of the revenue
#Lets check months after
MonthsAfter37Closure %>% 
  group_by(sku) %>%
  summarise(TotalRevenue = sum(AdjRev)) %>% #create a summary table with the total adjusted revenue of each vendor
  mutate(percent = prop.table(TotalRevenue)*100) %>% arrange(desc(percent)) #add a column to the table for the percent of each vendor

#ah well, that sinks that hypothesis. The top six most profitable sku are unchanged before and after the closure



#Total users decreased? Not just vend 37?
MonthsBefore37Closure %>% .$account %>% unique() %>% length
MonthsAfter37Closure %>% .$account %>% unique %>% length
#ah-ha! So in general, 46189 less users made purchases in the months following vendor37 closure
#So without doing any model, I am pretty comfortable hypothesizing that the closure of vendor37 would not have 
#meaningfully changed the revenue stream