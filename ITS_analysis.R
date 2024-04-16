#----- Libraries and data imports -------

library(tidyverse) 
library(plotly)
library(stats)
library(tseries)
library(ggeasy)
library(nlme)
library(tsibble)
library(astsa)
library(forecast)
library(lmtest)

load("full_data_saved.RData")



#---- Uncontrolled, 2 interventions data ----

# Preparing the Dataframe 

### Description of the dataframe:

# - Time: This variable represent the number of weeks that passed since 1970

# - Intervention: This variable is 0 before the first week of 2013 and 1 for the first week and everything after 

# - Intervention_2: This variable is 0 before the first week of 2014 and 1 for the first week and everything after 

# - Post_intervention: This variable is 0 before the first week of 2013 and increases by one for 
#                      each week that passes after

# - Post_intervention_2: This variable is 0 before the first week of 2014 and increases by one for 
#                      each week that passes after

its_df = full_data |> 
  arrange(DateTime) |> 
  mutate(Week = yearweek(DateTime)) |>
  filter(Status == "ToU", Year %in% c(2012,2013,2014)) |> 
  group_by(Week) |>
  summarise(Consumption = mean(Consumption)) |> 
  mutate(Time = as.integer(difftime(Week, 
                                    as.POSIXct("1970-01-01"), 
                                    units = "weeks")), 
         Intervention = if_else(Week >= yearweek(dmy("01/01/2013")), 1, 0),
         Post_intervention = if_else(Week >= yearweek(dmy("01/01/2013")), Time - 2242, 0),
         Intervention_2 = if_else(Week >= yearweek(dmy("01/01/2014")), 1, 0), 
         Post_intervention_2 = if_else(Week >= yearweek(dmy("01/01/2014")), Time - 2294, 0)) |> 
  select(Week, Time, Intervention, Post_intervention, Intervention_2, Post_intervention_2, Consumption)

save(its_df, file = "its_df_saved.RData")

## Ploting Consumption 
consumption = select(its_df, Consumption) 
consumption = ts(consumption, frequency = 52)
plot(consumption)

## ACF and PACF of the raw consumption data 
ggtsdisplay(consumption)
adf.test(consumption)


# - The plot of the raw consumption data, both the ACF and PACF charts show, and 
#   the Dickey Fuller test results show that we do not have a stationary time series 
#
# - The Nonstationary behaviour is indicated by the obvious trend in the data, slow 
#   decay in the ACF, and the fact that the PACF at the first lag is nearly 1 (Shumway and Stoffer, 2011).



## Dealing with the Nonstationarity 
adf.test(diff(consumption))
ggtsdisplay(diff(consumption))

# The result of the Dickey Fuller test on the first difference of thet data (p-value = 0.03)
# shows that the first difference is enough to make the data stationary. 

## Model

its_df_matrix = its_df |> ungroup() |> mutate(Time = Time - 2189) |>
  select(-Week, -Consumption) |> as.matrix()

uncontrolled_model = auto.arima(consumption, seasonal=TRUE, xreg=its_df_matrix, d=1, max.D=1, 
                                max.p = 5, max.q = 5, stepwise=FALSE, trace=TRUE)

summary(uncontrolled_model)
checkresiduals(uncontrolled_model)

## Plot



#---- Controlled, 1 intervention data ----

its_df_2 = full_data |> 
  arrange(DateTime) |> 
  mutate(Week = yearweek(DateTime)) |>
  filter(Year %in% c(2012,2013,2014)) |> 
  group_by(Week, Status) |>
  summarise(Consumption = mean(Consumption)) |> 
  mutate(Time = as.integer(difftime(Week, 
                                    as.POSIXct("1970-01-01"), 
                                    units = "weeks")), 
         Intervention = if_else(Week >= yearweek(dmy("01/01/2013")), 1, 0),
         Post_intervention = if_else(Week >= yearweek(dmy("01/01/2013")), Time - 2242, 0),
         Time = Time - 2189,
         x = if_else(Status == "ToU", 1, 0), 
         x.Time = x * Time,
         x.Intervention = x*Intervention,
         x.Post_intervention = x * Post_intervention) |> 
  select(x, Time, x.Time, Intervention, x.Intervention, Post_intervention, x.Post_intervention, Consumption)

save(its_df_2, file = "its_df2_saved.RData")


## ACF and PACF of the raw consumption data

consumption_2 = its_df_2 |> ungroup() |> select(Consumption) 
consumption_2 = ts(consumption_2, frequency = 52)
plot(consumption_2)

## ACF and PACF of the raw consumption data 
ggtsdisplay(consumption_2)
adf.test(consumption_2)

## Dealing with the Nonstationarity 
adf.test(diff(consumption_2))
ggtsdisplay(diff(consumption_2))


consumption_2 = its_df_2 |> ungroup() |> select(Consumption) 
its_df_2_matrix = its_df_2 |> ungroup() |> select(-Week, -Consumption) |> as.matrix()

controlled_model = auto.arima(consumption_2, seasonal=TRUE, xreg=its_df_2, max.d=1, max.D=1, 
                              stepwise=FALSE, trace=TRUE)

summary(controlled_model)
checkresiduals(controlled_model)

