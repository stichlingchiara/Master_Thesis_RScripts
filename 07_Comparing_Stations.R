#ANOVA different stations 
# Fishpond

Fishpond <- df_salmo %>% 
  filter(Location == "Fishpond")
Fishpond$Station <- as.factor(Fishpond$Station)
anova_fp <- aov(`% org carbon` ~ Station, data = Fishpond)
summary(anova_fp) 
#Df Sum Sq Mean Sq F value Pr(>F)   SIGNIFICANT
#Station      2  32.29  16.145   5.282 0.0144 *
#Residuals   20  61.13   3.057     

TukeyHSD(anova_fp)

#fishpond carbon stock 
Fishpond2 <- df_plot_stock %>% 
  filter(Location == "Fishpond")
Fishpond2$Station <- as.factor(Fishpond2$Station)
anova_fp2 <- aov(stock_50cm ~ Station, data = Fishpond2)
summary(anova_fp2) 

#compare data from same location, different stations
Mangguihay <- df_salmo %>% 
  filter(Location == "Mangguihay") 
Mangguihay$Station <- as.factor(Mangguihay$Station)
anova_mang <- aov(`% org carbon` ~ Station, data = Mangguihay)
summary(anova_mang)

#ANOVA different stations Tamisan
#compare data from same location, different stations
Tamisan <- df_salmo %>% 
  filter(Location == "Tamisan") 

anova_Tamisan <- aov(`% org carbon` ~ Station, data = Tamisan)
summary(anova_Tamisan)

#checking normality
plot(anova_Tamisan, which = 2)
# data is normally distributed

# check homogenity
plot(anova_Tamisan, which = 1)

#check difference between station 1 and 3
Tamisan_2 <- df_salmo%>% 
  filter(Location == "Tamisan",
         Station %in% c("1", "3"))

anova_Tamisan_2 <- aov(`% org carbon` ~ Station, data = Tamisan_2)
summary(anova_Tamisan_2)

#Anova Badas
Badas <- df_salmo %>% 
  filter(Location == "Badas") 

anova_badas <- aov(`% org carbon` ~ Station, data = Badas)
summary(anova_badas)
levels(Badas$Station)
Badas$Station <- as.factor(Badas$Station)
TukeyHSD(anova_badas)

Badas2 %>%
  group_by(Station) %>%
  summarise(mean_stock = mean(stock_50cm, na.rm = TRUE,),
            sd_stock = sd(stock_50cm, na.rm = TRUE))

#fishpond carbon stock 
Badas2 <- df_plot_stock %>% 
  filter(Location == "Badas")
Badas2$Station <- as.factor(Badas2$Station)
anova_badas2 <- aov(stock_50cm ~ Station, data = Badas2)
summary(anova_badas2)
TukeyHSD(anova_badas2)

#comparing stations properly
library(dplyr)

tam <- df_salmo %>%
  filter(Location == "Tamisan") %>%
  mutate(
    Age = case_when(
      Station %in% c(1,2) ~ "2005",   
      Station %in% c(3,4) ~ "2023",
      TRUE ~ NA_character_
    ),
    Age = factor(Age)
  )

#summarize to plot means
tam_plot <- tam %>%
  group_by(Age, Station, Plot) %>%
  summarise(mean_carbon = mean(calc_carbon_cm, na.rm = TRUE), .groups = "drop")
tam_lm <- lm(mean_carbon ~ Age, data = tam_plot)
summary(tam_lm)

#anova of different ages
anova_tam <- aov(mean_carbon ~ Age, data = tam_plot)
summary(anova_tam)

#look at % org carbon
tam_plot2 <- tam %>%
  group_by(Age, Station, Plot) %>%
  summarise(mean_carbon = mean(`% org carbon`, na.rm = TRUE), .groups = "drop")
tam_lm2 <- lm(mean_carbon ~ Age, data = tam_plot2)
summary(tam_lm2)

#look at all different ages and locations
age_diff <- lmer(calc_carbon_cm ~ Years_passed + (1|Location/Station/Plot), data = Palau)
summary(age_diff)

#checking if age differs with location for carbon per cm
age_loc <- lmer(calc_carbon_cm ~ Years_passed * Location +
       (1|Station/Plot), data = Palau)
summary(age_loc)

#checking the same thing for % organic carbon
age_loc_orgcarb <- lmer(`% org carbon` ~ Years_passed * Location +
                  (1|Station/Plot), data = df_salmo)
summary(age_loc_orgcarb)

#comparing age differences again 
station_age_df <- df_salmo %>%
  group_by(Location, Station, Years_passed) %>%   # or Age if categorical
  summarise(
    mean_OC = mean(`% org carbon`, na.rm = TRUE),
    sd_OC   = sd(`% org carbon`, na.rm = TRUE),
    n_OC    = sum(!is.na(`% org carbon`)),
    .groups = "drop"
  )

#comparing age differences again - stocks
station_age_df2 <- df_salmo %>%
  group_by(Location, Station, Years_passed) %>%   # or Age if categorical
  summarise(
    mean_OC = mean(calc_carbon, na.rm = TRUE),
    sd_OC   = sd(calc_carbon, na.rm = TRUE),
    n_OC    = sum(!is.na(calc_carbon)),
    .groups = "drop"
  )
View(station_age_df2)
#plot this 
ggplot(grouped_ages, aes(x = Years_passed, y = mean_stock100, color = Location)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "Years since establishment", y = "SOC Stock to 1 m")

#test for significant difference
m_age_station <- lmer(mean_OC ~ Years_passed + (1|Location), data = station_age_df)
summary(m_age_station)

#looking at carbon stocks 
df_plot_stock2 <- df_salmo %>%
  group_by(Location, Station, Plot, Years_passed) %>%
  summarise(
    mean_per_cm = mean(calc_carbon_cm, na.rm = TRUE),
    stock_50cm  = mean_per_cm * 50,
    mean_soc = mean(`% org carbon`, na.rm=TRUE)
  )%>% 
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 
View(df_plot_stock2)

#extrapolate stocks to 100 cm 
df_plot_stock2 <- df_plot_stock2 %>%
  group_by(Location, Station, Years_passed) %>%
  mutate(stock_100cm = stock_50cm * 2) %>%
  mutate(stock_Mgha = stock_100cm * 100) %>%
           filter(Location != "Baganga")

#group by stations to compare different ages
grouped_ages <- df_plot_stock2 %>%
  group_by(Location, Station, Years_passed) %>%   # or Age if categorical
  summarise(
    mean_stock100 = mean(stock_Mgha, na.rm = TRUE),
    sd_stock_100   = sd(stock_Mgha, na.rm = TRUE),
    n_stock_100    = sum(!is.na(stock_Mgha)),
    .groups = "drop"
  )
View(grouped_ages)

#looking at carbon stocks, with compaction
df_plot_stock_wc <- df_salmo_wc %>%
  group_by(Location, Station, Plot, Years_passed) %>%
  summarise(
    mean_per_cm = mean(calc_carbon_cm, na.rm = TRUE),
    stock_50cm  = mean_per_cm * 50,
    mean_soc = mean(`% org carbon`, na.rm=TRUE)
  )%>% 
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 
View(df_plot_stock_wc)

#test for significant difference
m_age_stocks <- lmer(stock_50cm ~ Years_passed + (1|Location/Station), data = df_plot_stock2)
summary(m_age_stocks)

#even cleaner? 
df_station_stock <- df_plot_stock2 %>%
  group_by(Location, Station, Years_passed) %>%
  summarise(mean_stock = mean(stock_50cm), .groups = "drop")

#test
m_age_station2 <- lmer(mean_stock ~ Years_passed + (1|Location),
                      data = df_station_stock)
summary(m_age_stocks)

#plot this 
ggplot(df_station_stock, aes(x = Years_passed, y = mean_stock, color = Location)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "Years since establishment", y = 
         expression("Carbon Stock per Plot (" * g ~ cm^{-2} * ")"))

#boxplot of stations
#create data frame without Baganga
df_plot_stock_noBG <- df_plot_stock %>%
  filter(Location != "Baganga")

df_plot_stock_noBG %>% 
  ggplot(mapping = aes(x = Station, y = mean_soc)) +
  geom_boxplot() +
  facet_wrap(vars(Location), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(x = "", y = "% Organic Carbon", title = "Comparing % org carbon across sampling sites") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )
