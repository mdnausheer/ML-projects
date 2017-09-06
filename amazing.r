library(dplyr)


a <- inner_join(x=amazingOrder, y=amazingOrderList, by = Order ID)


getwd()
setwd("C:/Praxis/Term 2/RETA")

library(readxl)
amazingOrder <- read_excel("AmazingMartEU2.xlsx", sheet = "OrderBreakdown")
amazingOrderList <- read_excel("AmazingMartEU2.xlsx", sheet = "ListOfOrders")
amaziingSummary <- read_excel("Amazing.xlsx", sheet = "a1")


names(amazingOrder)
names(amazingOrderList)

am2 <- amazingOrder
am1 <- amazingOrderList


#merge the data

am <- merge(am1,am2, all.x = T)

dim(am)



#PREPARING THE DATA FOR ANALYSIS

#CONVERTING THE DATE VARIABLE FROM FACTOR TO DATE
am$`Order Date` <- as.character(am$`Order Date`)
am$`Order Date` <- as.Date(am$`Order Date`)
am$Year_of_Purchase <- as.numeric(format(am$`Order Date`,"%Y"))

class(am$`Order Date`)

#ASIDE 1:
#cALCULATING TIME DIFFERENCE
time1 = "2017-05-01"
time2 = "2016-12-12"
class(time1)
mode(time1)
format(as.Date(time1),"%Y")

difftime(time1,time2,units = "days")
as.numeric(difftime(time1,time2,units = "days"))

am <- read.csv("am.csv")

am <- am[,-1]
#CREATING AN ADDITIONAL COLUMN THAT WILL HLD THE INFORMATION OF DAYS SINCE LAST PURCHASE

am$daysSince <- round(as.numeric((difftime("2015-01-01",am$OrderDate,units = "days"))))
View(am)

#Activating sqldf

require(sqldf)


#CALCULATING RECENCY,FREQUENCY AND MONETARY (RFM) VALUES FOR EACH UNIQUE CUSTOMERS

customer = sqldf("SELECT CustomerName, MIN(DaysSince) as Recency, 
                 count(DISTINCT(OrderID)) AS Frequency,sum(Sales) as Amount,
                 Max(Dayssince) as FirstPurchase FROM am GROUP BY 1")



#Initial segmentation

customer$segment <- NA
customer$segment[which(customer$Recency > 365*3)] = "inactive"
customer$segment[which(customer$Recency <= 365*3 & customer$Recency>365*2)] = "cold"
customer$segment[which(customer$Recency <= 365*2 & customer$Recency>365)] = "warm"
customer$segment[which(customer$Recency <= 365)]  = "active"
View(customer)

#Further segmentation
customer$segment[which(customer$segment == "active" & customer$FirstPurchase <365)] = "new active"
customer$segment[which(customer$segment == "active" & customer$Amount>200)] = "active high value"
customer$segment[which(customer$segment == "active" & customer$Amount<200)] = "active low value"
customer$segment[which(customer$segment == "warm" & customer$FirstPurchase < 365*2)] = "new warm"
customer$segment[which(customer$segment == "warm" & customer$Amount >200)] = "warm high value"
customer$segment[which(customer$segment == "warm" & customer$Amount <200)] = "warm low value"

                         
#2014

am$daysSince <- round(as.numeric((difftime("2014-01-01",am$OrderDate,units = "days"))))
View(am)







                         
#######################################                  
#Arjuns code for RFM [the other way to do RFM using dplyr]
summary_data <- am %>% group_by(CustomerName) %>%
  summarise(FirstPurchase = max(daysSince), Recency = min(daysSince), Amount = sum(Sales), Frequency = n_distinct(OrderID))

summary_data <- mutate(summary_data, Segmentation = ifelse(Recency>365*3, "Inactive",
                                                           ifelse((Recency<365*3 & Recency>365*2), "Cold",
                                                                  ifelse((Recency<365*2 & Recency>365), "Warm", "Active"))))
###########################################
#Studying the segments

#Number of customers

length(customer$CustomerName)

#Frequency distribution of segments
table(customer$segment)

#Reording the levels
customer$segment <- factor(x=customer$segment,levels = c("inactive","cold","warm high vaue","warm low value",
                                                         "newwarm","active high","active low value", "new active"))

table(customer$segment)

#aggregating the data
aggregate(customer[,2:5], by= list(customer$segment), FUN = mean)

View(am)

write.csv(am,"C:/Praxis/Term 2/am2.csv")


# Revenue generated in 2014 (Per customer)

revenue_2014 <- sqldf("SELECT CustomerName,sum(Sales) as TOTSales
                      FROM am 
                      WHERE Year_of_Purchase ==2014
                      GROUP BY 1")

actual <- merge(customer,revenue_2014)

View(actual)

revenue <- sqldf("SELECT segment,SUM(TOTSales) as Revenue FROM actual GROUP BY 1")
revenue

revenue$segment <- factor(x=revenue$segment,levels = c("inactive","cold","warm high vaue","warm low value",
                                                        "newwarm","active high","active low value", "new active"))

#Note that these are the list on Y people who purchase in 2014

#Merge the 2014 Customer and 2015 Revenue
