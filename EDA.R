#----- Libraries and data imports -------

library(tidyverse) 
library(ggthemes)
library(Metrics) 
library(hydroGOF)
library(mgsub) 
library(plotly)


load("full_data_saved.RData")

data_2013 = full_data |> filter(Year == 2013)



#---- Time Series and extra data prep -----

data_2013 = data_2013 |> 
  mutate(Month_number = month(DateTime),
         Seasons = if_else(Month_number %in% c(3,4,5), "Spring",
                           if_else(Month_number %in% c(6,7,8), "Summer",
                                   if_else(Month_number %in% c(9,10,11), "Fall",
                                           if_else(Month_number %in% c(12,1,2), "Winter", NA)))),
         data_2013 = data_2013 |> 
           mutate(Price = if_else(Tariff == "Low", 3.99,
                                  if_else(Tariff == "Normal", 11.76,
                                          if_else(Tariff == "High", 67.20, NA)))))

save(data_2013, file = "data_2013.RData")



#----- Energy Consumption in a day in 2013 -------

std_data_daily = data_2013 |>
  group_by(Hour, Status) |>
  summarise(daily_consumption = mean(Consumption))

daily_plt = ggplot(std_data_daily, mapping = aes(x = Hour, y = daily_consumption, 
                                                 colour = Status)) +
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette="Set1") +
  ggtitle("DAILY ENERGY CONSUMPTION: STD VS ToU") +
  xlab("HOURS") + ylab("CONSUMPTION  in  kWh") + labs(colour = "Status") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()



#----- Energy Consumption in the year -------

yearly = data_2013 |>
  group_by(Month, Status) |>
  summarise(Consumption = mean(Consumption))

yearly_plt = ggplot(yearly, mapping = aes(x = Month, y = Consumption, 
                                          colour = Status)) + 
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette="Set1") +
  ggtitle("") +
  xlab("Month") + ylab("CONSUMPTION  in  kWh") + labs(colour = "Status") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()



#----- Average price of electricity per day when on ToU  -------

daily_price_tou = data_2013 |>
  filter(Status == "ToU") |>
  group_by(Hour, Seasons) |>
  summarise(Price = mean(Price))

daily_price_tou_plt = ggplot(daily_price_tou, mapping = aes(x = Hour, y = Price, 
                                                            colour = Seasons)) + 
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette="Set1") +
  ggtitle("") +
  xlab("Hour") + ylab("Price  in  Pence/kWh") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()



#----- Distribution of the Annual bills of households -----

bills_dist = data_2013 |> 
  mutate(Bills = if_else(Status == "ToU", Price * Consumption, Consumption * 14.228)) |> 
  group_by(LCLid, Status) |> 
  summarise(Bills =  sum(Bills))

bills_dist_plt = ggplot(bills_dist, aes(x=Bills, fill=Status)) + 
  geom_histogram(alpha=.5, colour="black") +
  scale_x_continuous(breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000)) +
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()



#----- Energy consumption of dToU Households before 2013 vs after ----

before_after = full_data |>
  filter(Year %in% c(2012, 2013, 2014)) |>
  group_by(Month, Status) |> 
  summarise(Consumption = mean(Consumption)) |> 
  pivot_wider(names_from = Status, values_from = Consumption) |> 
  mutate(Difference = Std - ToU)

before_after_plt = ggplot(before_after, mapping = aes(x = Month, y = Difference)) + 
  geom_line(linewidth = 1.2) +#
  scale_color_brewer(palette="Set1") +
  ggtitle("") +
  xlab("Month") + ylab("CONSUMPTION  in  kWh") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()
