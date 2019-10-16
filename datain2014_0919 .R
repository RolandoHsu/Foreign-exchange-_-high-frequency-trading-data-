setwd("/Users/xubodun/Desktop/RA_foreign2014")

##### import some package #####
library(stringr)
library(tidyverse)
library(dplyr)
library(dummies)
library(xts)
library(lubridate)
library(highfrequency)
library(tidyr)
#library(Nippon) #search holidays in Japan 
library(timeDate) #search holidays in LONDON & NYSE
library(plyr)

# find the holidays in 2014 
Londonholiday <- holidayLONDON(2014) %>% as.POSIXct()
NYSEholiday <- holidayNYSE(2014) %>% as.POSIXct()
Japanholiday <- c("2014-01-01", "2014-01-02", "2014-01-03", 
                  "2014-01-13", "2014-02-11", "2014-02-14",
                  "2014-03-03", "2014-03-21", "2014-04-29",
                  "2014-05-03", "2014-05-04", "2014-05-05",
                  "2014-05-06", "2014-06-21", "2014-07-07",
                  "2014-07-21", "2014-08-06", "2014-08-09",
                  "2014-09-15", "2014-09-23", "2014-10-13",
                  "2014-11-03", "2014-11-15", "2014-11-23",
                  "2014-11-24", "2014-12-22", "2014-12-23",
                  "2014-12-25", "2014-12-31") %>% as.POSIXct()
Holiday <- unique(c(Londonholiday, NYSEholiday, Japanholiday)) %>% 
  as.Date() %>% 
  as.character()

# find the weekends in 2014 
Weekends <- seq(as.Date("2014-01-01"), as.Date("2014-12-31"), by="days") %>% 
  as.data.frame() %>% 
  mutate(daysofweek = format(. ,format = "%u")) %>% 
  filter(daysofweek == 6 | daysofweek == 7) %>% 
  .[,-2] %>% 
  as.character()

HolidayAndWeekend <- unique(c(Holiday, Weekends)) %>% 
  str_replace_all(., "-","")

FileName <- list.files("File")

# omit the holiday and weekends in filename
DailyFileNameList <- as.data.frame(FileName) %>%
  filter(!(str_sub(FileName, 9, 16) %in% HolidayAndWeekend)) %>% 
  .[1:nrow(.), ] %>% 
  as.character()

# ##### transfer the data into R.data #####
# for (i in DailyFileNameList) {
#   TXTname <- paste0("/Users/xubodun/Desktop/RA_foreign2014/File/", i)
#   dataTXT <- read.csv(TXTname, header = F)
#   Rdatasign <- paste0(str_sub(i,1,16), ".RData")
#   savename <- paste0("/Users/xubodun/Desktop/RA_foreign2014/R.Data/", Rdatasign)
#   save(dataTXT, file = savename)
#   cheer <- paste0("cheer!!!!!", Rdatasign)
#   print(cheer)
# }

##### get the new filename #####
RDataName <- list.files("/Users/xubodun/Desktop/RA_foreign2014/R.Data")

col_name <- c("Time", "Currency", "D_P", "Bid", "Ask","Bid_Volume","Ask_Volume")
currency <- c("EUR/USD", "USD/JPY")
DP <- c("P", "D")

decidethecurrency_fun <- function(cur, mins1or5){
  datain2014 <- NULL
  for (a_1 in RDataName) {
    
    Rdata_name <- paste0("/Users/xubodun/Desktop/RA_foreign2014/R.Data/", a_1)
    load(Rdata_name)
    # transfer the time into POSIXct
    data2 <- dataTXT %>% 
      mutate(time = str_c(dataTXT$V1, dataTXT$V2, sep = " ")) %>% 
      .[,-c(1,2)] %>% 
      select(time, everything())
    colnames(data2)[1:ncol(data2)] <- col_name 
    data2$Time <- mdy_hms(data2$Time)
    
    # order the data by Time 
    data2 <- data2[order(as.Date(data2$Time, format="%Y/%m/%d")),]
    
    # omit the data is D plus price is na 
    data3 <- data2 %>% 
      mutate(daysofweek = format(Time, format = "%u"))%>% 
      subset(!(D_P == "D" & !(is.na(Bid)) & !(is.na(Ask))))
    
    for (p in DP) {
      
      # filter the data (loop in DP) #
      data4 <- data3 %>% subset(Currency == cur & D_P == p)
      if(nrow(data4) == 0) next 
      data4[is.na(data4)] <- 0
      
      # transform the data into xts 
      data5 <- as.xts(data4[,2:ncol(data4)], data4[,1])
      
      # get the new data speperate from the minutes 
      data6_mins <- to.period(data5, period = "minutes", k = mins1or5, OHLC = F)
      
      # Bid_High, Ask_Low #
      Bid_High <- as.xts(data4[,4] ,data4[,1]) %>% 
        to.period(., period = "minutes", k = mins1or5, OHLC = T) %>% 
        .[,2]
      
      Ask_Low <- as.xts(data4[,5] ,data4[,1]) %>% 
        to.period(., period = "minutes", k = mins1or5, OHLC = T) %>% 
        .[,3] 
      
      # Bid and Ask value #
      data4_BA_Value <- as.xts(data4[,c(6,7)] ,data4[,1])
      Bid_TValue <- period.sum(data4_BA_Value$Bid_Volume, endpoints(data4_BA_Value, on= "mins", k = mins1or5))
      Ask_TValue <- period.sum(data4_BA_Value$Ask_Volume, endpoints(data4_BA_Value, on= "mins", k = mins1or5))
      
      # count the P and D
      data4 <- data4 %>% 
        mutate(DP_1 = 1) 
      DP_count <- as.xts(data4[,9] ,data4[,1]) %>% 
        period.sum(., endpoints(., on= "mins", k = mins1or5)) 
      
      # count the numbers of the 5 mins in one day
      time <- time(data6_mins) %>% as.data.frame()
      
      if (mins1or5 == 5) {
        countmins <- time %>% 
          mutate(
            sign = case_when(
              between(minute(time$.), 0, 4) ~ 1,
              between(minute(time$.), 5, 9) ~ 2,
              between(minute(time$.), 10, 14) ~ 3,
              between(minute(time$.), 15, 19) ~ 4,
              between(minute(time$.), 20, 24) ~ 5,
              between(minute(time$.), 25, 29) ~ 6,
              between(minute(time$.), 30, 34) ~ 7,
              between(minute(time$.), 35, 39) ~ 8,
              between(minute(time$.), 40, 44) ~ 9,
              between(minute(time$.), 45, 49) ~ 10,
              between(minute(time$.), 50, 54) ~ 11,
              between(minute(time$.), 55, 59) ~ 12))
        countmins <- countmins %>% mutate(hour = hour(.))
        dayinyear <- yday(time(data6_mins)[1])+1
        
        if ((dayinyear < yday(ymd("2014/03/11")) | dayinyear > yday(ymd("2014/11/04")))) {
          countmins <- countmins %>% mutate(
            hour_sign = case_when(
              hour == 22 ~ 1, 
              hour == 23 ~ 2,
              TRUE ~ hour +3))
        }else{
          countmins <- countmins %>% mutate(
            hour_sign = case_when(
              hour == 21 ~ 1, 
              hour == 22 ~ 2,
              hour == 23 ~ 3, 
              TRUE ~ hour +4))
        }
        countmins <- countmins %>% 
          mutate(
            countmins = sign + (hour_sign-1)*12) %>% 
          .[,ncol(.)]
        
      }else{
        
        countmins <- time %>%
          mutate(
            sign = minute(time$.) + 1)
        countmins <- countmins %>% mutate(hour = hour(.))
        dayinyear <- yday(time(data6_mins)[1])+1
        
        if ((dayinyear < yday(ymd("2014/03/11")) | dayinyear > yday(ymd("2014/11/04")))) {
          countmins <- countmins %>% mutate(
            hour_sign = case_when(
              hour == 22 ~ 1, 
              hour == 23 ~ 2,
              TRUE ~ hour +3))
        }else{
          countmins <- countmins %>% mutate(
            hour_sign = case_when(
              hour == 21 ~ 1, 
              hour == 22 ~ 2,
              hour == 23 ~ 3, 
              TRUE ~ hour +4))
        }
        countmins <- countmins %>% 
          mutate(
            countmins = sign + (hour_sign-1)*60) %>% 
          .[,ncol(.)]
      }
      
      # get the time variable 
      septime <- time(data6_mins) %>% 
        as.data.frame() %>% 
        mutate(
          Year = year(data6_mins),
          Month = month(data6_mins),
          day = day(data6_mins),
          Hour = hour(data6_mins),
          Minute = minute(data6_mins),
          Second = second(data6_mins)
        )
      
      # merge all things into finaldata 
      data7 <- merge(data6_mins[,3:7], Bid_High, Ask_Low, Bid_TValue, Ask_TValue, join="inner") %>% 
        as.data.frame() %>% 
        cbind(., as.data.frame(data6_mins[,1:2])) %>% 
        mutate(
          countmins = countmins,
          DP_count =  DP_count
        )
      
      colnames(data7)[6:7] <- c("Bid_High", "Ask_Low")
      finaldata <- data.frame(septime, data7)
      colnames(finaldata)[1] <- "Time"
      
      datain2014 <- rbind(datain2014, finaldata)
      cheer <- paste0("cheers_", a_1)
      print(cheer)
      
    }
  }
  name <- paste0("/Users/xubodun/Desktop/RA_foreign2014/RdataforeachDPorCur/datain2014_", mins1or5, "_", str_sub(cur,5,7), ".RData")
  save(datain2014, file = name)
}

cur = "EUR/USD"
mins1or5 = 5
decidethecurrency_fun(cur, mins1or5)

cur = "USD/JPY"
mins1or5 = 5
decidethecurrency_fun(cur, mins1or5)

cur = "EUR/USD"
mins1or5 = 1
decidethecurrency_fun(cur, mins1or5)

cur = "USD/JPY"
mins1or5 = 1
decidethecurrency_fun(cur, mins1or5)

##### final adjust #####
setwd("/Users/xubodun/Desktop/RA_foreign2014/RdataforeachDPorCur")

adjustRDatatofinal <- function(Rdatafile){
  load(Rdatafile)
  
  datain2014 <- datain2014 %>% 
    dummy.data.frame(names = "D_P") %>% 
    within(., D_PD <- ifelse((Ask == 0 & D_PD == 1),-1, D_PD))
  
  D_BA <- datain2014 %>% 
    select(Bid, Ask, D_PD) %>% 
    mutate(D_price = Bid + Ask) %>% 
    within(., D_price <- ifelse(D_PD == 0 , NA, D_price))
  
  P_Bidprice <- datain2014 %>% 
    select(Bid, D_PP) %>% 
    within(., Bid <- ifelse(D_PP == 0, NA, Bid))
  
  P_Askprice <- datain2014 %>% 
    select(Ask, D_PP) %>% 
    within(., Ask <- ifelse(D_PP == 0, NA, Ask)) 
  
  datain2014$Bid[datain2014$Bid == 0] <- NA
  datain2014$Ask[datain2014$Ask == 0] <- NA
  
  
  datain2014 <- datain2014 %>% 
    mutate(
      P_Bidprice = P_Bidprice$Bid,
      P_Askprice = P_Askprice$Ask,
      D_price = D_BA$D_price
    )
  
  name_2 <- paste0("/Users/xubodun/Desktop/RA_foreign2014/NewRdataforeachDPorCur/", Rdatafile)
  save(datain2014, file = name_2)
}

RdataSignBeforeadjust <- list.files()

for (i in RdataSignBeforeadjust) {
  adjustRDatatofinal(i)
}



