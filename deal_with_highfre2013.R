##### import some package #####
library(stringr)
library(tidyverse)
library(dplyr)
library(dummies)
library(xts)
library(lubridate)
library(highfrequency)
library(tidyr)
library(Nippon) #search holidays in Japan 
library(timeDate) #search holidays in LONDON & NYSE
library(plyr)

filename <- list.files("/Users/xubodun/Desktop/RA/ebs2013/allfile")
col_name <- c("Time", "Currency", "D_P", "Bid", "Ask","Bid_Volume","Ask_Volume")
currency <- c("EUR/USD", "USD/JPY")
DP <- c("P", "D")

# find the holidays in 2013 
Londonholiday <- holidayLONDON(2013) %>% as.POSIXct()
NYSEholiday <- holidayNYSE(2013) %>% as.POSIXct()
Japanholiday <- jholiday(2013) %>% as.POSIXct()
holiday <- unique(c(Londonholiday, NYSEholiday, Japanholiday)) %>% as.Date() %>% as.character()

# find the weekends in 2013 
weekends <- seq(as.Date("2013-01-01"), as.Date("2013-12-31"), by="days") %>% 
  as.data.frame() %>% 
  mutate(daysofweek = format(. ,format = "%u")) %>% 
  filter(daysofweek == 6 | daysofweek == 7) %>% 
  .[,-2] %>% 
  as.character()

holiweekend <- unique(c(holiday, weekends)) %>% 
  str_replace_all(., "-","")

# omit the holiday and weekends in filename
filename <- as.data.frame(filename) %>%
  filter(!(str_sub(filename, 9, 16) %in% holiweekend)) %>% 
  .[1:nrow(.), ] %>% 
  as.character()

  
datain2013 <- NULL
for (a in filename) {
  
  TXTname <- paste0("/Users/xubodun/Desktop/RA/ebs2013/allfile/", a)
  data1 <- read.csv(TXTname, header = F)
  
  # transfer the time into POSIXct
  data2 <- data1 %>% 
    mutate(time = str_c(data1$V1, data1$V2, sep = " ")) %>% 
    .[,-c(1,2)] %>% 
    select(time, everything())
  colnames(data2)[1:ncol(data2)] <- col_name 
  data2$Time <- mdy_hms(data2$Time)
  
  # order the data by Time 
  data2 <- data2[order(as.Date(data2$Time, format="%Y/%m/%d")),]
  
  ## drop the data which in holidays or weekends ##
  # create a column called "days of week"
  data3 <- data2 %>% 
    mutate(daysofweek = format(Time, format = "%u"))
  
  # get a new data with every five minutes #
  # need to seperate the currency before split the data with five minutes
  # split the data with five minutes #
  
  for (d in currency) {
    
    # filter the currency
    data4 <- filter(data3, Currency == d)
    if(nrow(data4) == 0) next 
    
    for (p in DP) {
      
      data5 <- filter(data4, D_P == p)
      if(nrow(data4) == 0) next 
      data6 <- split(data5, cut(data5$Time, "5 mins"))
      everyfivemin <- names(data6) %>% length() %>% as.numeric()
      
      for (f in 1:everyfivemin) {
        
        data7 <- data6[f] %>% as.data.frame()
        if(nrow(data7) == 0) next 
        colnames(data7)[1:8] <- 
          c("Time", "Currency", "D_P", "Bid", "Ask", "Bid_Volume", "Ask_Volume", "daysofweek")
        data8 <-  data7 %>% 
          as.data.frame() %>% 
          last() %>% 
          mutate(
            DP_Amount = count(data7, vars = "D_P")$freq,
            Bid_highest =  max(data7$Bid, na.rm = T),
            Ask_lowest = min(data7$Ask, na.rm = T),
            Bid_totalvolume = sum(as.numeric(data7$Bid_Volume), na.rm = T),
            Ask_totalvolume = sum(as.numeric(data7$Ask_Volume), na.rm = T),
            count = paste0(str_sub(a, 9,16),"_",d,"_",p,"_",f) 
          )
        datain2013 <- rbind(datain2013, data8)
        
      }  
    }
  }
  print("coooooooooooooooooooooooooooooooooooooooooooooool")
}




