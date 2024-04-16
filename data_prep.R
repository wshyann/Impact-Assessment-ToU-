#----- Libraries imports -------

library(tidyverse) 
library(vroom)
library(zoo)
library(ggthemes)
library(Metrics) 
library(hydroGOF)
library(mgsub) 
library(plotly)
library(hms)

# Creating a function that changes the path of the data

get_path = function(i){
  path = sprintf("partitioned data/LCL-June2015v2_%s.csv", i)
  return(path)
}

# For each path, we will read the data, concatenate it to a group of data and save it

#----- First sub data set -----

data1 = tibble()

for (id in seq(0, 35)) {
  path = get_path(id)
  df = vroom(path) 
  data1 = bind_rows(data1, df)
  print(paste(id, "/35", sep = ""))
}
save(data1, file = "data1_saved.RData")

#----- Second sub data set -----

data2 = tibble()

for (id in seq(36, 70)) {
  path = get_path(id)
  df = vroom(path) 
  data2 = bind_rows(data2, df)
  print(paste(id-35, "/35", sep = ""))
}
save(data2, file = "data2_saved.RData")


#----- Third sub data set -----

data3 = tibble()

for (id in seq(71, 105)) {
  path = get_path(id)
  df = vroom(path) 
  data3 = bind_rows(data3, df)
  print(paste(id-70, "/35", sep = ""))
}
save(data3, file = "data3_saved.RData")


#----- Fourth sub data set -----

data4 = tibble()

for (id in seq(106, 140)) {
  path = get_path(id)
  df = vroom(path) 
  data4 = bind_rows(data4, df)
  print(paste(id-105, "/35", sep = ""))
}
save(data4, file = "data4_saved.RData")


#----- Fifth sub data set -----

data5 = tibble()

for (id in seq(141, 167)) {
  path = get_path(id)
  df = vroom(path)
  if (id == 153 | 167){
    df$`KWH/hh (per half hour)` = as.numeric(df$`KWH/hh (per half hour)`)
  }
  data5 = bind_rows(data5, df)
  print(paste(id-140, "/27", sep = ""))
}

save(data5, file = "data5_saved.RData")


#----- Concatenating all the subsets -----

full_data = bind_rows(data1, data2, data3, data4, data5)

# Creating Month, Date, and Day columns 
# Renaming "stdorToU" to "Status"
# Renaming "KWH/hh (per half hour)" to "Consumption"

full_data = full_data |> 
  mutate(Month = as.yearmon(DateTime),
         Date = as_date(DateTime),
         Day = day(DateTime),
         Hour = as_hms(DateTime),
         Year = year(DateTime)) |> 
  rename(Status = stdorToU, Consumption = 'KWH/hh (per half hour)')

#----- Tariffs data merger -----

# Importing Tariffs data
tariffs = read_csv("Tariffs.csv")
tariffs = tariffs |>
  rename(DateTime = TariffDateTime) |> 
  mutate(DateTime = dmy_hm(DateTime))

# Merger
full_data = left_join(full_data, tariffs, by="DateTime") |>
  mutate(Tariff = if_else(is.na(Tariff), "Std", Tariff))

# Dealing with NA values
full_data = full_data |> filter(is.na(Consumption) == FALSE)



#---- Seasons and Prices ----

full_data = full_data |> 
  mutate(Month_number = month(Month),
         Seasons = if_else(Month_number %in% c(3,4,5), "Spring",
                           if_else(Month_number %in% c(6,7,8), "Summer",
                                   if_else(Month_number %in% c(9,10,11), "Fall",
                                           if_else(Month_number %in% c(12,1,2), "Winter", NA)))),
         Price = if_else(Tariff == "Low", 0.039,
                         if_else(Tariff == "Normal", 0.1176,
                                 if_else(Tariff == "High", 0.6720, 0.14228)))) |>
  select(-Month_number)

#----- Saving the full dataset -----

save(full_data, file = "full_data_saved.RData")

# Loading the data 
# full_data_saved = load("full_data_saved.RData")


