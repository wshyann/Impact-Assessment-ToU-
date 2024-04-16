#----- Libraries and data imports -------

library(tidyverse) 
library(ggthemes)
library(Metrics) 
library(hydroGOF)
library(mgsub) 
library(plotly)
library(ggeasy)


load("full_data_saved.RData")

data_2013 = full_data |> filter(Year == 2013)



#---- Time Series and extra data prep -----

data_2013 = data_2013 |> 
  mutate(Month_number = month(Month),
         Seasons = if_else(Month_number %in% c(3,4,5), "Spring",
                           if_else(Month_number %in% c(6,7,8), "Summer",
                                   if_else(Month_number %in% c(9,10,11), "Fall",
                                           if_else(Month_number %in% c(12,1,2), "Winter", NA)))),
         Price = if_else(Tariff == "Low", 0.039,
                                  if_else(Tariff == "Normal", 0.1176,
                                          if_else(Tariff == "High", 0.6720, NA)))) |>
  select(-Month_number)

save(data_2013, file = "data_2013.RData")



#----- Energy Consumption throughout the research period -------

yearly = full_data |>
  group_by(Month, Status) |>
  summarise(Consumption = mean(Consumption))

yearly_plt = ggplot(yearly, mapping = aes(x = Month, y = Consumption, 
                                          colour = Status)) + 
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette="Set1") +
  ggtitle("STD VS dTou") +
  xlab("Month") + ylab("CONSUMPTION  in  kWh") + labs(colour = "Status") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()



#----- Energy consumption difference between STD and dToU Households ----

before_after = full_data |>
  filter(Year %in% c(2012, 2013, 2014)) |>
  group_by(Month, Status) |> 
  summarise(Consumption = mean(Consumption)) |> 
  pivot_wider(names_from = Status, values_from = Consumption) |> 
  mutate(Difference = Std - ToU)

before_after_plt = ggplot(before_after, mapping = aes(x = Month, y = Difference)) + 
  geom_line(linewidth = 1.2) +
  scale_color_brewer(palette="Set1") +
  ggtitle("CONSUMPTION DIFFERENCE") +
  xlab("Month") + ylab("CONSUMPTION  in  kWh") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()



#----- Response to dToU ----- 

response_to_price1 = full_data |> 
  filter(Year == 2012, Seasons=="Winter") |>
  filter(Status == "ToU") |>
  group_by(Hour) |>
  summarise(Before_ToU = mean(Consumption))

response_to_price2 = data_2013 |> 
  filter(Year == 2013, Seasons=="Winter") |>
  filter(Status == "ToU") |>
  group_by(Hour) |>
  summarise(After_ToU = mean(Consumption))

response_to_price = left_join(response_to_price1, response_to_price2, by="Hour") |>
  pivot_longer(cols = c(Before_ToU, After_ToU), names_to = "Variable", values_to = "Value")

ggplot(response_to_price, mapping = aes(x = Hour, y = Value, colour = Variable)) + 
  geom_line(linewidth = 1.2)  +
  scale_color_brewer(palette="Set1") +
  ggtitle("WINTER 2012 VS WINTER 2013") +
  xlab("Hour") + ylab("CONSUMPTION  in  kWh") + 
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
  ggtitle("dToU Price PLOT PER SEASON") +
  xlab("Hour") + ylab("Price  in  Pence/kWh") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()



#----- Distribution of the Annual bills of households -----

bills1 = full_data |> 
  filter(Year == 2012, Seasons=="Winter") |>
  filter(Status == "ToU") |>
  mutate(Bills = Price * Consumption) |> 
  group_by(LCLid) |> 
  summarise(Before_ToU = sum(Bills)) |> 
  ungroup()

bills2 = full_data |> 
  filter(Year == 2013, Seasons=="Winter") |>
  filter(Status == "ToU") |>
  mutate(Bills = Price * Consumption) |> 
  group_by(LCLid) |> 
  summarise(After_ToU = sum(Bills)) |> 
  ungroup()

bills = left_join(bills1, bills2, by="LCLid") |>
  pivot_longer(cols = c(Before_ToU, After_ToU), names_to = "Variable", values_to = "Value") |> 
  filter(is.na(Value) == FALSE)

bills_dist_plt = ggplot(bills, aes(x=Value, fill=Variable)) + 
  geom_histogram(alpha=.5, colour="black") +
  facet_wrap(~Variable, nrow = 2) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 300, 500, 1000)) +
  scale_color_brewer(palette="Set1") +
  ggtitle("BILLS DISTRIBUTION: Before dToU VS After dToU") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  easy_center_title()

bills |> group_by(Variable) |> summarise(Value = median(Value))
bills |> group_by(Variable) |> summarise(Value = mean(Value))


  