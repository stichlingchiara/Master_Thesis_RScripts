#Import data and establish data frames 

#install packages 
#install.packages("tidyverse")
#install.packages('dtplyr')
library(tidyverse)
#install.packages("ggplot2")  
library(ggplot2)
install.packages("writexl")
library(writexl)

#importing my data sets 
# data set without compaction, conversion Salmo
install.packages("readxl")
library(readxl)

# data set conversion Salmo
Salmo_nc <- read_excel("~/Documents/Master/Thesis/Data_Analysis/Thesis_try/dataset_1.xlsx", 
                       +     sheet = "no_compaction")
View(Salmo_nc)                   

#dataset Salmo with compaction
Salmo_wc <- read_excel("~/Documents/Master/Thesis/Data_Analysis/Thesis_try/dataset_1.xlsx", 
                       +     sheet = "with_compaction")
View(Salmo_wc)                   

# data set conversion Palau
Palau_nc <- read_excel("~/Documents/Master/Thesis/Data_Analysis/Thesis_try/dataset_1.xlsx", 
                       +     sheet = "no_compaction_Palau")
View(Palau_nc) 

# data set conversion Van Bemmelen

Van_Bemmelen_nc <- read_excel("~/Documents/Master/Thesis/Data_Analysis/Thesis_try/dataset_1.xlsx", 
                              +     sheet = "no_compaction_Van_Bemmelen")
View(Van_Bemmelen_nc)          

#create data frames
# dataframe Salmo
df_salmo <- Salmo_nc %>% 
  select(Location, Station, Plot, Level, Compaction, `% org carbon`, calc_carbon_cm, calc_carbon, `SOC density`,
         DBD, Years_passed
  ) %>%
  mutate(conversion = "Salmo",
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", 
                                                "Mangguihay", "Baganga")))  %>%
  filter(!(Location == "Mangguihay" &
             Station == "2" &
             Plot == "3")) 

View(df_salmo)

df_salmo_wc <- Salmo_wc %>% 
  select(Location, Station, Plot, Level, `% org carbon`, calc_carbon_cm, calc_carbon, `SOC density`,
         DBD, Years_passed
  ) %>%
  mutate(conversion = "Salmo",
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", 
                                                "Mangguihay", "Baganga")))  %>%
  filter(!(Location == "Mangguihay" &
             Station == "2" &
             Plot == "3"))
view(df_salmo_wc)
# data frame palau
df_palau <- Palau_nc %>% 
  select(Location, Station, Plot, Level, `% org carbon`, calc_carbon_cm
  ) %>%
  mutate(conversion = "Palau") %>%
  filter(!(Location == "Mangguihay" &
             Station == "2" &
             Plot == "3"))
view(df_palau)

#data frame Van Bemmelen 
df_bemmelen <- Van_Bemmelen_nc %>% 
  select(Location, Station, Plot, Level, `% org carbon`, calc_carbon_cm
  ) %>%
  mutate(conversion = "Van Bemmelen") %>%
  filter(!(Location == "Mangguihay" &
             Station == "2" &
             Plot == "3"))
view(df_bemmelen)

#checking how many datapoints I have
sum(!is.na(df_salmo$`% org carbon`))
sum(complete.cases(df_salmo))

#table with an oversight of all means 
station_summary <- df_salmo %>%
  group_by(Location, Station) %>%
  summarise(
    mean_OC = mean(`% org carbon`, na.rm = TRUE),
    sd_OC = sd(`% org carbon`, na.rm = TRUE),
    mean_DBD = mean(DBD, na.rm = TRUE),
     sd_DBD = sd(DBD, na.rm = TRUE),
    mean_SOC_density = mean(`SOC density`, na.rm = TRUE),
    sd_SOC_density = sd(`SOC density`, na.rm = TRUE),
    .groups = "drop"
  )

#round for clean reporting
station_summary <- station_summary %>%
  mutate(
    mean_OC = round(mean_OC, 2),
    mean_DBD = round(mean_DBD, 3),
    mean_SOC_density = round(mean_SOC_density, 3),
    sd_OC = round(sd_OC, 2),
    sd_DBD = round(sd_DBD, 3),
    sd_SOC_density = round(sd_SOC_density, 3)
  )

#add the stock
station_stock <- df_plot_stock %>%
  group_by(Location, Station) %>%
  summarise(
    mean_stock_50cm = mean(stock_50cm, na.rm = TRUE),
    sd_stock_50cm   = sd(stock_50cm, na.rm = TRUE),
    .groups = "drop"
  )

#round 
station_stock <- station_stock %>%
  mutate(
    mean_stock_50cm = round(mean_stock_50cm, 3),
    sd_stock_50cm = round(sd_stock_50cm, 3),
    .groups = "drop"
)

#convert to Mg C / ha 
station_summary_full <- station_summary_full %>%
  mutate(stock_Mg_ha = mean_stock_50cm * 100,
         stock_Mg_ha_sd = sd_stock_50cm * 100)
#stations as numeric 
station_stock <- station_stock %>%
  mutate(Station = gsub("S", "", Station))

#as character
station_summary <- station_summary %>%
  mutate(Station = as.character(Station))

station_stock <- station_stock %>%
  mutate(Station = as.character(Station))

#add to the whole df
station_summary_full <- station_summary %>%
  left_join(station_stock, by = c("Location", "Station"))

#make it an excel file 
write_xlsx(station_summary_full,
           "Station_Summary_Table2.xlsx")
