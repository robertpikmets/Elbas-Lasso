library(tidyverse); library(lubridate); library(zoo)

#source for load_data function: https://stackoverflow.com/questions/23190280/
load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read_csv)
  bind_rows(tables)
}

##transform into tibble, change column names, filter to hourly products only, filter to buy or sell areas under consideration only and omit irrelevant columns
#for data after  13th of June 2018
prepare_data_post <- function(df, areas, columns) {
  new_df <- as_tibble(df)
  colnames(new_df) <- columns
  
  new_df <- new_df %>% 
    filter((buyer %in% areas | seller %in% areas) & type %in% c("P60MIN", "PH")) %>% 
    select(-c(currency, cancelled, id, type))
  
  new_df <- new_df %>% 
    mutate(timestamp = ymd_hms(new_df$trade_time, tz="UTC"), 
           delivery_day = ymd(substring(new_df$power_hour, 4, 11)),
           delivery_hour = as.numeric(substring(new_df$power_hour, 13, 14))
    ) %>% 
    select(-c(trade_time, power_hour)) %>% 
    relocate(timestamp, delivery_day, delivery_hour)
  
  return(new_df)
}

#for data before 13th of June 2018
prepare_data_pre <- function(df, areas, columns) {
  new_df <- as_tibble(df)
  colnames(new_df) <- columns
  
  new_df <- new_df %>% 
    filter((buyer %in% areas | seller %in% areas), cancelled == 0) %>% 
    select(-c(currency, cancelled))
  
  new_df <- new_df %>% 
    mutate(timestamp = ymd_hms(new_df$trade_time, tz="UTC"), 
           delivery_day = ymd(substring(new_df$type, 4, 11)),
           delivery_hour = as.numeric(substring(new_df$type, 13, 14)),
           product = substring(new_df$type, 1,2)
    ) %>% 
    filter(product == "PH") %>% 
    select(-c(trade_time, type, product)) %>% 
    relocate(timestamp, delivery_day, delivery_hour)
  
  return(new_df)
}

#calculate volume-weighted average price
calculate_vwap <- function(data) {
  agg_data <- data %>% 
    group_by(delivery_day, delivery_hour) %>% 
    summarise(volume = sum(qty), vwap = round(sum(price*qty)/volume, 2))
  return(agg_data)
}

far_vwap <- function(df, hours) {
  
  new_df <- df %>% 
    mutate(end_time = ymd_h(paste(delivery_day, delivery_hour)),
           interval = floor(difftime(end_time, timestamp, units = "hours"))) %>% 
    filter(interval > hours)
  
  new_df <- calculate_vwap(new_df)
  return(new_df) 
}

near_vwap <- function(df, hours) {
  
  new_df <- df %>% 
    mutate(end_time = ymd_h(paste(delivery_day, delivery_hour)),
           interval = floor(difftime(end_time, timestamp, units = "hours"))) %>% 
    filter(interval <= hours)
  
  new_df <- calculate_vwap(new_df)
  return(new_df) 
}

exact_vwap <- function(df, hours) {
  
  new_df <- df %>% 
    mutate(end_time = ymd_h(paste(delivery_day, delivery_hour)),
           interval = floor(difftime(end_time, timestamp, units = "hours"))) %>% 
    filter(interval == hours)
  
  new_df <- calculate_vwap(new_df)
  return(new_df) 
}

#calculates the latest-vwap variable
latest_vwap_fn <- function(df) {
  latest_price_5 <- exact_vwap(df, 5)
  latest_price_6 <- exact_vwap(df, 6)
  latest_price_7 <- exact_vwap(df, 7)
  latest_price_8 <- exact_vwap(df, 8)
  latest_price_9 <- exact_vwap(df, 9)
  
  join_latest <- list(latest_price_5, latest_price_6, latest_price_7, 
                      latest_price_8, latest_price_9) %>% 
    reduce(full_join, by = c("delivery_day", "delivery_hour"))
  
  #more recent price info takes priority
  join_latest$vwap.x[is.na(join_latest$vwap.x)] <- join_latest$vwap.y[is.na(join_latest$vwap.x)]
  join_latest$vwap.x[is.na(join_latest$vwap.x)] <- join_latest$vwap.x.x[is.na(join_latest$vwap.x)]
  join_latest$vwap.x[is.na(join_latest$vwap.x)] <- join_latest$vwap.y.y[is.na(join_latest$vwap.x)]
  join_latest$vwap.x[is.na(join_latest$vwap.x)] <- join_latest$vwap[is.na(join_latest$vwap.x)]
  
  join_latest <- join_latest[,c(1,2,4)]
  names(join_latest)[3] <- "latest_vwap"
  return(join_latest)
}

create_vwap_df <- function(data, hours) {
  near_vwap_data <- near_vwap(data, hours)
  
  far_vwap_data <- far_vwap(data, hours)
  
  vwap_data <- full_join(far_vwap_data, near_vwap_data, by = c("delivery_day", "delivery_hour"))
  names(vwap_data)[3:length(vwap_data)] <- c("far_vol","far_vwap", "near_vol", "near_vwap")
  
  vwap_data$delivery_hour <- as.factor(vwap_data$delivery_hour)
  
  vwap_data <- vwap_data %>% 
    ungroup() %>% 
    mutate(h5_near_vwap = lag(near_vwap, 5),
           h5_near_vwap_vol = lag(near_vol, 5),
           d1_near_vwap = lag(near_vwap, 24), 
           d7_near_vwap = lag(near_vwap, 168)) %>% 
    select(-near_vol) %>% 
    filter(year(delivery_day)>2015)
  return(vwap_data)
}

prepare_elbas <- function(data_2015, data_2016, data_2017, data_2018, data_2019, data_2020, price_areas, names1, names2){
  
  data_2015 <- prepare_data_pre(data_2015, price_areas, names1)
  data_2016 <- prepare_data_pre(data_2016, price_areas, names1)
  data_2017 <- prepare_data_pre(data_2017, price_areas, names1)
  data_2019 <- prepare_data_post(data_2019, price_areas, names2)
  data_2020 <- prepare_data_post(data_2020, price_areas, names2)
  
  #2018 first half data
  data_2018a <-  data_2018[1:which(is.na(data_2018), arr.ind=TRUE)[1]-1,]
  data_2018a <- data_2018a[, colSums(is.na(data_2018a)) == 0]
  data_2018a <- prepare_data_pre(data_2018a, price_areas, names1)
  
  #2018 second half data
  data_2018b <- data_2018[which(is.na(data_2018), arr.ind=TRUE)[1]:nrow(data_2018),]
  data_2018b <- data_2018b[, colSums(is.na(data_2018b)) == 0]
  names_2018b <- c("power_hour", "currency", "price","qty", "buyer", 
                   "seller", "cancelled", "trade_time", "type", "id") 
  data_2018b <- prepare_data_post(data_2018b, price_areas, names_2018b)
  
  #merge 2018
  data_2018 <- bind_rows(data_2018a, data_2018b)
  
  elbas_data <- bind_rows(data_2015, data_2016, data_2017, data_2018, data_2019, data_2020)
  return(elbas_data)
}

##### Elbas data #####
setwd("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/")

#before 13th of June 2018
names1 <- c("trade_time", "type", "currency", "price",
            "qty", "buyer", "seller", "cancelled") 

#after  13th of June 2018
names2 <- c("trade_time", "type", "id", "power_hour", "currency", "price",
            "qty", "buyer", "seller", "cancelled") 

#restrict areas to EE,FI,SE,DK, LV, LT
price_areas <- c("EE", "FI", "SE1", "SE2", "SE3", "SE4", "DK1", "DK2", "LV", "LT")


#2016-2020 data, assumes working directory has been set correctly
data_2015 <- load_data("2015")
data_2016 <- load_data("2016")
data_2017 <- load_data("2017")
data_2018 <- load_data("2018")
data_2019 <- load_data("2019")
data_2020 <- load_data("2020")

elbas_data <- prepare_elbas(data_2015, data_2016, data_2017, data_2018, data_2019, data_2020, price_areas, names1, names2)


###### VWAP variables ######
vwap_data <- create_vwap_df(elbas_data, 4)

join_latest <- latest_vwap_fn(elbas_data)
join_latest$delivery_hour <- as.factor(join_latest$delivery_hour)


####### Operating data #######
setwd("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/operating")

sdv_fun <- function(filename, skip) {
  op_data <- read.csv(filename, skip = skip, header = FALSE,
                      as.is = TRUE, sep = ";", na.strings = "")
  op_data <- op_data %>% 
    filter(V1 != "AL", V2 != "U") %>% 
    mutate(delivery_day = dmy(V6)) %>% 
    select(-c(V1, V3, V4, V6, V11, V33)) %>% 
    relocate(delivery_day) %>% 
    filter(year(delivery_day) %in% c(2015:2020))
  return(op_data)
}

load_data_sdv <- function(path, skip) { 
  files <- dir(path, pattern = '\\.sdv', full.names = TRUE)
  tables <- lapply(files, sdv_fun, skip=skip)
  bind_rows(tables)
}

op_data_fun <- function(path, skip, flag = FALSE) {
  #specify folder
  setwd(path)
  #initiate dataframe with first year
  for (year in c(2016:2020)){
    if (flag == FALSE) {
      flag = TRUE
      first_df <- load_data_sdv(as.character(year), skip)
      next
    }
    new_df <- load_data_sdv(as.character(year), skip)
    first_df <- bind_rows(first_df, new_df)
  }
  return(first_df)
}

#for areas with all operating data available
tidy_sdv_wind <- function(op_data) {
  names(op_data)[5:length(op_data)] <- c(1:24) #delivery hours
  names(op_data)[2:4] <- c("code", "weekday", "area") #meaningful names
  
  #change data types
  op_data$weekday <- factor(op_data$weekday)
  op_data[5:length(op_data)] <- lapply(op_data[5:length(op_data)], as.numeric)
  
  #turn 24 delivery hour columns into one column
  op_data <- op_data %>% 
    pivot_longer(cols = -c(1:4), names_to = "delivery_hour", values_to = "value") %>% 
    group_by(code, area, delivery_day) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = code, values_from = value) %>% 
    select(-row) %>% 
    ungroup()
  
  op_data$delivery_hour <- as.factor(op_data$delivery_hour)
  
  #create lagged error columns
  #PE - Day-ahead production prognosis, P - Total production
  #WE - Day-ahead wind production prognosis, WS - Settled wind production
  #F - Total consumption, E - Day-ahead consumption prognosis
  op_data <- op_data %>% 
    mutate(load_error = lag(E - `F`, 5), prod_error = lag(PE - P, 5), wind_error = lag(WE - WS, 5)) %>% 
    select(-c(`F`, P, WS)) %>% 
    filter(year(delivery_day) > 2015) %>% 
    pivot_wider(names_from = area, values_from = c("PE", "WE", "E", "load_error", "prod_error", "wind_error"), names_sep = "_")
  
  return(op_data)
}

#for areas that do not have wind data available
tidy_sdv_nowind <- function(op_data, columns) {
  names(op_data)[5:length(op_data)] <- c(1:24) #delivery hours
  names(op_data)[2:4] <- c("code", "weekday", "area") #meaningful names
  
  #change data types
  op_data$weekday <- factor(op_data$weekday)
  op_data[5:length(op_data)] <- lapply(op_data[5:length(op_data)], as.numeric)
  
  #turn 24 delivery hour columns into one column
  op_data <- op_data %>% 
    pivot_longer(cols = -c(1:4), names_to = "delivery_hour", values_to = "value") %>% 
    group_by(code, area, delivery_day) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = code, values_from = value) %>% 
    select(-row) %>% 
    ungroup()
  
  op_data$delivery_hour <- as.factor(op_data$delivery_hour)
  
  #create lagged error columns, no wind error
  op_data <- op_data %>% 
    mutate(load_error = lag(E - `F`, 5), prod_error = lag(PE - P, 5)) %>% 
    select(-c(`F`, P)) %>% 
    filter(year(delivery_day) > 2015) %>% 
    pivot_wider(names_from = area, values_from = columns, names_sep = "_")
  
  return(op_data)
}

#interpolate missing values linearly
missing_val <- function(df){
  idx <- colSums(is.na(df)) != 0
  df[, idx] <- na.approx(df[, idx])
  new_df <- df %>% 
    mutate(across(where(is.numeric), round, 2))
  return(new_df)
}

#read in all data
op_data_ee <- op_data_fun("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/operating/estonia", 12)
op_data_fi <- op_data_fun("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/operating/finland", 15)
op_data_dk <- op_data_fun("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/operating/denmark", 19)
op_data_lv <- op_data_fun("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/operating/latvia", 12)
op_data_lt <- op_data_fun("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/operating/lt", 14)
op_data_se <- op_data_fun("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/operating/sweden", 33)

##### EE #####
#24.12.2019 PE and WE are missing, assume prognosis equals realised values and derive PE and WE as such
where_christmas <- op_data_ee[op_data_ee$delivery_day == "2019-12-24",][3:4,]
where_christmas[,2] <- c("PE", "WE")
op_data_ee <- rbind(op_data_ee, where_christmas)

tidy_ee <- op_data_ee %>% 
  tidy_sdv_wind() %>% 
  missing_val()

##### FI #####
tidy_fi <- op_data_fi %>% 
  filter(V2 %in% c("E", "F", "PE", "P")) %>%  # no wind variables
  tidy_sdv_nowind(c("PE", "E", "load_error", "prod_error")) %>% 
  missing_val()

##### DK #####
tidy_dk <- op_data_dk %>% 
  filter(V2 %in% c("E", "F", "PE", "P", "WS", "WE"), V7 != "DK")# omit regulating data for now

tidy_dk$V7[tidy_dk$V7 == "JY"] <- "DK1"
tidy_dk$V7[tidy_dk$V7 == "SJ"] <- "DK2"

tidy_dk <- tidy_dk %>% 
  tidy_sdv_wind() %>% 
  missing_val()

##### SE #####
tidy_se <- op_data_se %>% 
  filter(V2 %in% c("E", "F", "PE", "P", "WE"), V7 != "SE") %>% 
  tidy_sdv_nowind(c("PE", "E", "WE", "load_error", "prod_error")) %>% 
  missing_val()

##### LV #####
tidy_lv <- op_data_lv %>% 
  tidy_sdv_wind() %>% 
  missing_val()

##### LT #####
tidy_lt <- op_data_lt %>% 
  filter(V2 %in% c("E", "F", "PE", "P")) %>% 
  tidy_sdv_nowind(c("PE", "E", "load_error", "prod_error")) %>% 
  missing_val()


#merge tidy_ee, tidy_lv, tidy_lt, tidy_fi, tidy_dk, tidy_se
op_merge <- list(tidy_ee, tidy_lv, tidy_lt, tidy_fi, tidy_dk, tidy_se) %>% 
  reduce(left_join, by = c("delivery_day", "weekday", "delivery_hour"))


###### Day-ahead price data ######
sdv_spot_all <- function(filename, skip) {
  spot_data <- read.csv(filename, skip = skip, header = FALSE,
                        as.is = TRUE, sep = ";", na.strings = "")
  spot_data <- spot_data %>% 
    mutate(delivery_day = dmy(V6)) %>% 
    relocate(delivery_day) %>% 
    select(-c(V1,V2,V3,V4,V6,V12,V34)) %>% 
    filter(year(delivery_day) %in% c(2015:2020), V8 == "EUR") %>% 
    select(-V8)
  return(spot_data)
}

load_spot_sdv <- function(path, skip) { 
  files <- dir(path, pattern = '\\.sdv', full.names = TRUE)
  tables <- lapply(files, sdv_spot_all, skip=skip)
  bind_rows(tables)
}

spot_data_fun <- function(path, skip, flag=FALSE) {
  #specify folder
  setwd(path)
  #initiate dataframe with first year
  for (year in c(2016:2020)){
    if (flag==FALSE) {
      flag = TRUE
      first_df <- load_spot_sdv(as.character(year), skip)
      next
    }
    new_df <- load_spot_sdv(as.character(year), skip)
    first_df <- bind_rows(first_df, new_df)
  }
  return(first_df)
}

tidy_sdv_spot <- function(spot_data) {
  names(spot_data)[4:length(spot_data)] <- c(1:24) #delivery hours
  names(spot_data)[2:3] <- c("weekday", "area") #meaningful names
  
  #change data types
  spot_data$weekday <- factor(spot_data$weekday)
  spot_data[4:length(spot_data)] <- lapply(spot_data[4:length(spot_data)], gsub, pattern = ",", replacement = ".")
  spot_data[4:length(spot_data)] <- lapply(spot_data[4:length(spot_data)], as.numeric)
  
  #turn 24 delivery hour columns into one column
  spot_data <- spot_data %>% 
    filter((area %in% c("NO1", "NO2", "NO3", "NO4", "NO5", "FRE")) == FALSE) %>% 
    pivot_longer(cols = -c(1:3), names_to = "delivery_hour", values_to = "value") %>% 
    filter(year(delivery_day) > 2015) %>% 
    pivot_wider(names_from = area, values_from = value, names_glue = "{area}_spot")
  
  spot_data$delivery_hour <- as.factor(spot_data$delivery_hour) 
  return(spot_data)
}

spot_data <- spot_data_fun("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/elspot", 25)

tidy_spot <- spot_data %>% 
  tidy_sdv_spot() %>% 
  missing_val()


###### Regulating data #######
regulating <- op_data_fun("/Users/Roobu/Documents/University of Tartu/Lõputöö/data/regulating", skip = 22)

tidy_regulating <- function(regu_data) {
  names(regu_data)[5:length(regu_data)] <- c(1:24) #delivery hours
  names(regu_data)[2:4] <- c("code", "weekday", "area") #meaningful names
  
  regu_data <- regu_data %>% 
    filter(area %in% c("DK1", "DK2", "FI", "SE1", "SE2", "SE3", "SE4"))
  
  #change data types
  regu_data$weekday <- factor(regu_data$weekday)
  regu_data[5:length(regu_data)] <- lapply(regu_data[5:length(regu_data)], gsub, pattern = ",", replacement = ".")
  regu_data[5:length(regu_data)] <- lapply(regu_data[5:length(regu_data)], as.numeric)
  
  #turn 24 delivery hour columns into one column
  regu_data <- regu_data %>% 
    pivot_longer(cols = -c(1:4), names_to = "delivery_hour", values_to = "value") %>% 
    group_by(code, area, delivery_day) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = code, values_from = value) %>% 
    select(-row) %>% 
    ungroup()
  
  regu_data$delivery_hour <- as.factor(regu_data$delivery_hour)
  regu_data$DD <- as.factor(regu_data$DD)
  
  regu_data <- regu_data %>% 
    pivot_wider(names_from = area, values_from = c("RO", "RN", "RC", "RP", "RS", "DD"), names_sep = "_") %>% 
    ungroup() %>% 
    mutate(across(4:45, lag, n=5)) %>% 
    filter(year(delivery_day)>2015)
  
  return(regu_data)
}

regu_data <- regulating %>% 
  tidy_regulating() %>% 
  missing_val()

#due to na.approx approximating DD factors as well, deal with one 0,5 value manually
regu_data[regu_data$DD_DK2 == "-0.5",]$DD_DK2 <- 0 #neutral value
regu_data <- regu_data %>% mutate(across(39:45, factor))









