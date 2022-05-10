##### Installing necessary packages
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")

##### Loading installed packages
library(dplyr)
library(lubridate)
library(ggplot2)

##### Reading in Data
SanFranciscoWQ <- read.csv("/cloud/project/SanFranciscoWQ.csv")
LosAngelesWQ <- read.csv("/cloud/project/LAWQ.csv")
CentralValleyWQ <- read.csv("/cloud/project/CentralValleyWQ.csv")
SantaAnaWQ <- read.csv("/cloud/project/SantaAnaWQ.csv")


##### Parsing dates with year,month,day and hours:minutes:seconds
SanFranciscoWQ$dateF <- ymd_hms(SanFranciscoWQ$SAMPLE_DATE) 
LosAngelesWQ$dateF <- ymd_hms(LosAngelesWQ$SAMPLE_DATE)
CentralValleyWQ$dateF <- ymd_hms(CentralValleyWQ$SAMPLE_DATE)
SantaAnaWQ$dateF <- ymd_hms(SantaAnaWQ$SAMPLE_DATE)
# $dateF indicates that a new column in the data set should be created with the 
# command to the right of the arrow

##### Extracting the month and year from each date and creating new columns
SanFranciscoWQ$Month <- month(SanFranciscoWQ$dateF)
SanFranciscoWQ$Year <- year(SanFranciscoWQ$dateF)

LosAngelesWQ$Month <- month(LosAngelesWQ$dateF)
LosAngelesWQ$Year <- year(LosAngelesWQ$dateF)

CentralValleyWQ$Month <- month(CentralValleyWQ$dateF)
CentralValleyWQ$Year <- year(CentralValleyWQ$dateF)

SantaAnaWQ$Month <- month(SantaAnaWQ$dateF)
SantaAnaWQ$Year <- year(SantaAnaWQ$dateF)


##### Subsetting data frames so that data relevant to the passing of legislation
##### (2000s) is highlighted
SanFranciscoWQTime <- SanFranciscoWQ %>%
  filter(Year >= "1990", Year <= "2022")

LAWQTime <- LosAngelesWQ %>%
  filter(Year >= "1989", Year <= "2021")

CentralValleyWQTime <- CentralValleyWQ %>%
  filter(Year >= "1991", Year <= "2020")

SantaAnaWQTime <- SantaAnaWQ %>%
  filter(Year >= "1990", Year <= "2021")

##### Creating a time series for each region to measure seasonality and trends

### San Francisco Nitrate Content Time Series

# Subsetting the San Francisco data so that the only pollutant measured is nitrogen
SanFranciscoWQTimeN <- SanFranciscoWQTime %>%
  filter(PARAMETER == "Dissolved Nitrate")

# Averaging data for each month of each year so that there are equal intervals
# between each nitrogen measurement
SanFranciscoWQTimeN.TS <- SanFranciscoWQTimeN %>%
  group_by(Year, Month) %>%
  summarise(Nitrogen.avg = mean(RESULT, na.rm=TRUE))

# Creating the time series with data that has consistent intervals of one month
SFnitrate_ts <- ts(SanFranciscoWQTimeN.TS$Nitrogen.avg, # data
                 start = c(2001,1),#start year 2001, month 1
                 end = c(2020,1), #end year 2020, month 1
                 frequency= 12) #number of observations within the unit of time

# Decomposing the dataset in order to identify trends (changes in values over time), 
# seasonality (cyclical changes), and random error 
SFnitrate_decompose <- decompose(SFnitrate_ts)
plot(SFnitrate_decompose)

# Extracting and plotting the trend data from the decomposition 

SFtimedata <- decompose(SFnitrate_ts)

#trend
SFtimedata$trend

SFtimedatadf <- data.frame(trend = as.vector(SFtimedata$trend))

plot(SFtimedatadf$trend, 
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     col = "sienna3",
     ylab = "Nitrate Content Overtime") 


# Making an initial plot to see if there are any trends
ggplot(data = SanFranciscoWQTimeN.TS, # data for plot
       aes(x = DateF, y=Nitrogen.avg ) )+ # aes, x and y
  geom_point()+ # make points at data point
  labs(x="Year", y="Nitrate Content") # make axis labels
#since the resulting graph had no clear trends, I think it would be more useful
#if there were fewer data points, so I think an annual average nitrate measurement
#should be used rather than a monthly average nitrate measurement

### Making a plot of average annual nitrate content over time
#Calculating Annual Average Nitrate Content
SanFranciscoAnnualAvg <- SanFranciscoWQTimeN.TS %>%
  group_by(Year) %>%
  summarise(Annual.avg = mean(Nitrogen.avg))

#Graphing the Annual Average Nitrate Content Data
ggplot(data = SanFranciscoAnnualAvg, aes(x = Year, y = Annual.avg)) +
  geom_point(color = "sienna3") +
  geom_line(color = "sienna3") +
  labs(x="Year", y="Nitrate Content (mg/L)", title = "Annual Average Nitrate Content in San Francisco Surface Water from 1997-2022") +
  theme_classic()

#Adding annotation to the graph
b <- ggplot(data = SanFranciscoAnnualAvg, aes(x = Year, y = Annual.avg)) +
  geom_point(color = "sienna3") +
  geom_line(color = "sienna3") +
  labs(x="Year", y="Nitrate Content (mg/L)", title = "Annual Average Nitrate Content in San Francisco Surface Water from 1997-2022") +
  theme_classic()

b + annotate("segment", # line label
             x=2018, # start x coordinate
             y=1.612721, # start y coordinate
             xend=2018, # end x coordinate
             yend=2.2) + # end y coordinate
  annotate("text", # add text label
           x=2018, # center of label x coordinate
           y= 2.2, # center of label y coordinate
           label="ILRP Implemented (2018)") # label to add


### Los Angeles Nitrate Content Time Series

# Subsetting the Los Angeles data so that the only pollutant measured is nitrogen
LAWQTimeN <- LAWQTime %>%
  filter(PARAMETER == "Dissolved Nitrate")

# Averaging data for each month of each year so that there are equal intervals
# between each nitrogen measurement
LAWQTimeN.TS <- LAWQTimeN %>%
  group_by(Year, Month) %>%
  summarise(Nitrogen.avg = mean(RESULT, na.rm=TRUE))

# Creating the time series with data that has consistent intervals of one month
LAnitrate_ts <- ts(LAWQTimeN.TS$Nitrogen.avg, # data
                   start = c(2001,2),
                   end = c(2019,2),
                   frequency= 4) 

# Decomposing the dataset in order to identify trends (changes in values over time), 
# seasonality (cyclical changes), and random error 
LAnitrate_decompose <- decompose(LAnitrate_ts)
plot(LAnitrate_decompose)

# Extracting and plotting the trend data from the decomposition 
LAtimedata <- decompose(LAnitrate_ts)
LAtimedata$trend

LAtimedatadf <- data.frame(trend = as.vector(LAtimedata$trend))

plot(LAtimedatadf$trend, # x data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     col = "darkgoldenrod1",
     ylab = "Nitrate Content Over time") #x axis label

### Making a plot of average annual nitrate content over time
#Calculating Annual Average Nitrate Content
LosAngelesAnnualAvg <- LAWQTimeN.TS %>%
  group_by(Year) %>%
  summarise(Annual.avg = mean(Nitrogen.avg))

#Graphing the Annual Average Nitrate Content Data
ggplot(data = LosAngelesAnnualAvg, aes(x = Year, y = Annual.avg)) +
  geom_point(color = "darkgoldenrod1") +
  geom_line(color = "darkgoldenrod1") +
  labs(x="Year", y="Nitrate Content (mg/L)", title = "Annual Average Nitrate Content in San Francisco Surface Water from 1997-2022") +
  theme_classic()

#Adding annotation to the graph
b <- ggplot(data = LosAngelesAnnualAvg, aes(x = Year, y = Annual.avg)) +
  geom_point(color = "darkgoldenrod1") +
  geom_line(color = "darkgoldenrod1") +
  labs(x="Year", y="Nitrate Content (mg/L)", title = "Annual Average Nitrate Content in Los Angeles Surface Water from 1989-2022") +
  theme_classic()

b + annotate("segment", # line label
             x=2005, # start x coordinate
             y=1.7625000, # start y coordinate
             xend=2005, # end x coordinate
             yend=2.4) + # end y coordinate
  annotate("text", # add text label
           x=2005, # center of label x coordinate
           y= 2.6, # center of label y coordinate
           label="ILRP Implemented (2005)") # label to add

### Central Valley Nitrate Content Time Series

# Subsetting the Central Valley data so that the only pollutant measured is nitrogen
CentralValleyWQN <- CentralValleyWQ %>%
  filter(PARAMETER == "Dissolved Nitrate")

# Averaging data for each month of each year so that there are equal intervals
# between each nitrogen measurement
CentralValleyWQN.TS <- CentralValleyWQN %>%
  group_by(Year, Month) %>%
  summarise(Nitrogen.avg = mean(RESULT, na.rm=TRUE))

# Creating the time series with data that has consistent intervals of one month
CentralValleynitrate_ts <- ts(CentralValleyWQN.TS$Nitrogen.avg, # data
                   start = c(2001,1),
                   end = c(2022,1),#start year 2016, month 1
                   #first number is unit of time and second is observations within a unit
                   frequency= 12) 

# Decomposing the dataset in order to identify trends (changes in values over time), 
# seasonality (cyclical changes), and random error 
CentralValleynitrate_decompose <- decompose(CentralValleynitrate_ts)
plot(CentralValleynitrate_decompose)

# Extracting and plotting the trend data from the decomposition 
CentralValleytimedata <- decompose(CentralValleynitrate_ts)
CentralValleytimedata$trend

CentralValleytimedatadf <- data.frame(trend = as.vector(CentralValleytimedata$trend))

plot(CentralValleytimedatadf$trend, # x data
     type = "b", #b = points and lines
     col = "springgreen2",
     pch = 19, # symbol shape
     ylab = "Nitrate Content Over time") #x axis label

### Making a plot of average annual nitrate content over time
#Calculating Annual Average Nitrate Content
CentralValleyAnnualAvg <- CentralValleyWQN.TS %>%
  group_by(Year) %>%
  summarise(Annual.avg = mean(Nitrogen.avg))

CentralValleyAnnualAvgT <- CentralValleyWQTimeN.TS %>%
  group_by(Year) %>%
  summarise(Annual.avg = mean(Nitrogen.avg))

#Graphing the Annual Average Nitrate Content Data
ggplot(data = CentralValleyAnnualAvgT, aes(x = Year, y = Annual.avg)) +
  geom_point(color = "springgreen2") +
  geom_line(color = "springgreen2") +
  labs(x="Year", y="Nitrate Content (mg/L)", title = "Annual Average Nitrate Content in Central Valley Surface Water from 1995-2021") +
  theme_classic()

b <- ggplot(data = CentralValleyAnnualAvgT, aes(x = Year, y = Annual.avg)) +
  geom_point(color = "springgreen2") +
  geom_line(color = "springgreen2") +
  labs(x="Year", y="Nitrate Content (mg/L)", title = "Annual Average Nitrate Content in Central Valley Surface Water from 1995-2021") +
  theme_classic()

b + annotate("segment", # line label
             x=2003, # start x coordinate
             y=3.295073, # start y coordinate
             xend=2003, # end x coordinate
             yend=3.5) + # end y coordinate
  annotate("text", # add text label
           x=2003, # center of label x coordinate
           y= 3.6, # center of label y coordinate
           label="ILRP Implemented (2003)") # label to add


