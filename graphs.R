library(ggplot2)

#Division into initial calibration window, validation set and test set
ggplot(merge_all3, aes(delivery_day, near_vwap))+
  geom_line(color = "dodgerblue2") +
  labs(x = "Date", y = "near-VWAP") +
  scale_x_date(breaks = as.Date(c("2016-01-01", "2016-12-30", "2017-03-31", "2020-12-31")), 
               guide = guide_axis(n.dodge=2))+
  geom_vline(xintercept = as.numeric(as.Date("2016-12-30")), linetype=2)+
  geom_vline(xintercept = as.numeric(as.Date("2017-03-31")), linetype=2)+
  scale_y_continuous(breaks = seq(0, 600, by = 50)) +
  annotate("text", x=as.Date("2016-06-01"), y=400, label= "Initial calibration window") + 
  annotate("text", x=as.Date("2017-03-01"), y=400, label= "Lambda validation") + 
  annotate("text", x=as.Date("2018-07-01"), y=400, label= "Out-of-sample test period") + 
  theme_classic()


#An example of Elbas trades for a given delivery hour: 08.09.2019, hour 22
elbas_example <- elbas_data %>% filter(delivery_day == "2019-09-08", delivery_hour == 22)

library(scales)
ggplot(elbas_example, aes(x=timestamp, y=price))+
  geom_line(color="dodgerblue2")+
  scale_x_datetime(labels = date_format("%H:00"), date_breaks = "2 hours") +
  labs(x="Time of trade", y="Trade price (EUR/MWh)") +
  theme_classic()


#An example of two weeks of near-vwap
example_near_vwap <- merge_all3 %>% 
  filter(delivery_day >= "2019-09-02", delivery_day <= "2019-09-08") %>% 
  select(delivery_day, delivery_hour, near_vwap)

example_near_vwap$time <- ymd_h(paste(example_near_vwap$delivery_day, 
                                 as.character(example_near_vwap$delivery_hour), 
                                 sep = ","))

ggplot(example_near_vwap, aes(x=time, y=near_vwap))+
  geom_line(color = "dodgerblue2")+
  scale_x_datetime(labels = date_format("%H:00"), date_breaks = "6 hours") +
  labs(y = "Near-VWAP (EUR/MWh)", x="Time of delivery hour")+
  theme_classic()

#Near-VWAP average for each of the 24 delivery hours
near_vwap_per_hour <- merge_all3 %>% 
  group_by(delivery_hour = factor(delivery_hour, levels = c(1:24))) %>% 
  summarise(avg_near_vwap = mean(near_vwap))

ggplot(near_vwap_per_hour, aes(x=delivery_hour, y=avg_near_vwap))+
  geom_col(fill="dodgerblue2") +
  labs(x="Delivery hour", y="Near-VWAP (EUR/MWh)") +
  theme_classic()

#Total volume per buyer and seller bidding area
price_areas <- c("EE", "FI", "SE1", "SE2", "SE3", "SE4", "DK1", "DK2", "LV", "LT")
total_vol_buyer <- elbas_data %>% 
  filter(year(delivery_day) > 2015, buyer %in% price_areas) %>% 
  group_by(buyer) %>%
  summarise(total_volume = round(sum(qty), 0))
total_vol_buyer$buyer <- as.factor(total_vol_buyer$buyer)

total_vol_seller <- elbas_data %>% 
  filter(year(delivery_day) > 2015, seller %in% price_areas) %>% 
  group_by(seller) %>%
  summarise(total_volume = sum(qty))
total_vol_seller$seller <- as.factor(total_vol_seller$seller)

library(gridExtra)
buyer_plot <- ggplot(total_vol_buyer, aes(x=reorder(buyer, total_volume), y=total_volume)) +
  geom_col(fill="dodgerblue2")+
  coord_flip() +
  labs(y="Total volume (MWh)", x="Buyer area") +
  scale_y_continuous(labels = number) +
  theme_classic()

seller_plot <- ggplot(total_vol_seller, aes(x=reorder(seller, total_volume), y=total_volume)) +
  geom_col(fill="dodgerblue2")+
  coord_flip() +
  labs(y="Total volume (MWh)", x="Seller area") +
  scale_y_continuous(labels = number) +
  theme_classic()

grid.arrange(buyer_plot, seller_plot, ncol=2)


#Yearly total volume and no of trades
overall_volume <- elbas_data %>% 
  filter(year(delivery_day) > 2015) %>% 
  group_by(year = year(delivery_day)) %>% 
  summarise(total_volume = sum(qty), trades = n())

yearly_vol_graph <- ggplot(data = overall_volume, aes(x=year, y = trades)) +
  geom_bar(stat = "identity", fill="dodgerblue2") +
  labs(x="Year", y = "Total number of trades") +
  scale_y_continuous(labels = number) +
  theme_classic()

yearly_trade_graph <- ggplot(data = overall_volume, aes(x=year, y = total_volume)) +
  geom_bar(stat = "identity", fill="dodgerblue2") +
  labs(x="Year", y = "Total volume (MWh)") +
  scale_y_continuous(labels = number) +
  theme_classic()

grid.arrange(yearly_vol_graph, yearly_trade_graph, ncol=2)


#No of trades across delivery hours
hour_trades <- elbas_data %>% 
  filter(year(delivery_day) > 2015) %>% 
  group_by(delivery_hour) %>% 
  summarise(trades = n())
  
ggplot(data = hour_trades, aes(x=delivery_hour, y = trades)) +
  geom_bar(stat = "identity", fill="dodgerblue2") +
  labs(x="Delivery hour", y="No of trades") +
  scale_x_continuous(breaks = seq(1, 24, by = 1)) +
  scale_y_continuous(labels = number) +
  theme_classic()
  

#Mean elbas trade price per buyer area
area_prices <- elbas_data %>% 
  filter(year(delivery_day) > 2015, buyer %in% price_areas) %>% 
  group_by(buyer) %>%
  summarise(mean_price = mean(price))

ggplot(area_prices, aes(x=reorder(buyer, mean_price), y = mean_price)) +
  geom_col(fill="dodgerblue2") +
  coord_flip() +
  labs(x="Bidding area", y="Average trade price (EUR/MWh)") +
  theme_classic()


##### Summary statistics for near-VWAP
#on data that has not yet had its missing values imputed
training_set <- vwap_data %>% filter(delivery_day < "2016-12-30") %>% arrange(delivery_day, delivery_hour)
validation_set <- vwap_data %>% filter(delivery_day >= "2016-12-30", delivery_day < "2017-03-31") %>% arrange(delivery_day, delivery_hour)
test_set <- vwap_data %>% filter(delivery_day >= "2017-03-31") %>% arrange(delivery_day, delivery_hour)

summary(training_set$near_vwap)
summary(validation_set$near_vwap)
summary(test_set$near_vwap)
rbind(sd(training_set$near_vwap, na.rm = TRUE), 
      sd(validation_set$near_vwap), 
      sd(test_set$near_vwap, na.rm = TRUE))



#MAE/RMSE per delivery hour compared for best 1) benchmark 2) univariate and 3) multivariate
#baseline
baseline_matrix <- matrix(baseline1_errors_test, ncol = 24, byrow = TRUE)
#univariate
uni_matrix <- matrix(uni_test_set_errors, ncol = 24, byrow = TRUE)

hour_wise_metrics <- function(error_matrix){
  mae <- colSums(abs(error_matrix))/nrow(error_matrix)
  rmse <- (colSums(abs(error_matrix)**2)/nrow(error_matrix))**(1/2)
  return(cbind(mae,rmse))
}

rmse_df <- data.frame("delivery_hour" = c(1:24),
                      "baseline" = hour_wise_metrics(baseline_matrix)[,2],
                      "Univariate" = hour_wise_metrics(uni_matrix)[,2],
                      "Multivariate" = hour_wise_metrics(mv_test_results_small[[1]])[,2])

mae_df <- data.frame("delivery_hour" = c(1:24),
                     "baseline" = hour_wise_metrics(baseline_matrix)[,1],
                     "Univariate" = hour_wise_metrics(uni_matrix)[,1],
                     "Multivariate" = hour_wise_metrics(mv_test_results_small[[1]])[,1])

rmse_graph <- ggplot(rmse_df, aes(delivery_hour))+
  geom_line(aes(y=baseline, colour="Far-VWAP baseline"))+
  geom_line(aes(y=Univariate, colour="Univariate LASSO"))+
  geom_line(aes(y=Multivariate, colour="Multivariate LASSO"))+
  labs(x="Delivery hour", y="RMSE (EUR/MWh)", colour="Model")+
  scale_x_continuous(breaks = seq(1, 24, by = 1))+
  scale_y_continuous(breaks = seq(1, 16, by = 2))+
  theme_classic()+
  theme(legend.position = "bottom")

mae_graph <- ggplot(mae_df, aes(delivery_hour))+
  geom_line(aes(y=baseline, colour="Far-VWAP baseline"))+
  geom_line(aes(y=Univariate, colour="Univariate LASSO"))+
  geom_line(aes(y=Multivariate, colour="Multivariate LASSO"))+
  labs(x="Delivery hour", y="MAE (EUR/MWh)", colour="Model")+
  scale_x_continuous(breaks = seq(1, 24, by = 1))+
  scale_y_continuous(breaks = seq(1, 6, by = 1))+
  theme_classic()+
  theme(legend.position = "bottom")

grid.arrange(mae_graph, rmse_graph, ncol=2)
