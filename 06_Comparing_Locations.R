#check difference across different locations
#removing the outlier of Mangguihay S2, P3 first
res <- residuals(model2)
which.max(abs(res))
Palau[which.max(abs(res)), ]

#find if outliers are significant
install.packages("influence.ME")
library(influence.ME)
plot(cooks.distance(model2))

#remove outliers
#Palau <- Palau %>%
#  filter(!(Location == "Mangguihay" &
#             Station == "2" &
#             Plot == "3"))

#choose the calculated carbon per cm
df_calc_carbon_cm <- df_salmo %>% 
  select(Location, Station, Plot, Level, calc_carbon_cm
  ) %>%
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 

#choose the calculated carbon over all
df_calc_carbon <- df_salmo %>% 
  select(Location, Station, Plot, Level, calc_carbon
  ) %>%
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 

view(df_calc_carbon2)

#plot differences across stations and locations
df_calc_carbon_cm %>% ggplot(mapping = aes(x = Plot, y = calc_carbon_cm, colour = Level )) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(title = "Comparing the amount of org carbon per cm") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#plot differences across stations and locations carbon overall
df_calc_carbon %>% ggplot(mapping = aes(x = Plot, y = calc_carbon, colour = Level )) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(title = "Comparing the amount of org carbon") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#plot % org carbon across stations
df_org_carbon <- df_salmo %>% 
  select(Location, Station, Plot, Level, `% org carbon`
  ) %>%
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 

df_org_carbon %>% ggplot(mapping = aes(x = Plot, y = `% org carbon`, colour = Level )) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(title = "Comparing % org carbon across sampling sites") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#map stocks without level differences 
df_plot_stock %>% ggplot(mapping = aes(x = Plot, y = mean_soc )) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(title = "Comparing % org carbon across sampling sites", y = "% Organic Carbon") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )


#looking at mean and sd of the different locations
loc_summary <- df_salmo %>%
  group_by(Location) %>%
  summarise(
    mean_OC = mean(`% org carbon`, na.rm = TRUE),
    sd_OC   = sd(`% org carbon`, na.rm = TRUE),
    n       = n()
  )

loc_summary

#doing the same for calc_carbon_cm
loc_summary2 <- df_salmo %>%
  group_by(Location) %>%
  summarise(
    mean_cc = mean(calc_carbon, na.rm = TRUE),
    sd_cc   = sd(calc_carbon, na.rm = TRUE),
    n       = n()
  )

loc_summary2

#doing the same for carbon stocks
loc_summary_plots <- df_plot_stock %>%
  group_by(Location) %>%
  summarise(
    mean_cc = mean(stock_50cm, na.rm = TRUE),
    sd_cc   = sd(stock_50cm, na.rm = TRUE),
    n       = n()
  )

loc_summary_plots
#comparing locations amount of carbon
model_loc <- lmer(calc_carbon ~ Location + (1|Station), data = df_salmo)
summary(model_loc)
#we could see that the stations did not have a significant effect, so now we check with an ANOVA
anova_model_plots <- aov(stock_50cm ~ Location, data = df_plot_stock)
summary(anova_model_plots)
#pariwise comparison 
TukeyHSD(anova_model_plots)

#with compaction!!!!
#we could see that the stations did not have a significant effect, so now we check with an ANOVA
anova_model_plots_wc <- aov(stock_50cm ~ Location, data = df_plot_stock_wc)
summary(anova_model_plots_wc)
#pariwise comparison 
TukeyHSD(anova_model_plots_wc)

#comparing locations with % organic carbon
anova_model2 <- aov(`% org carbon` ~ Location, data = Palau_noPlot)
summary(anova_model2)
#pairwise comparison 
TukeyHSD(anova_model2)

#correct total carbon, standardized by depth 
df_plot_stock <- df_salmo %>%
  group_by(Location, Station, Plot) %>%
  summarise(
    mean_per_cm = mean(calc_carbon_cm, na.rm = TRUE),
    stock_50cm  = mean_per_cm * 50,
    mean_soc = mean(`% org carbon`, na.rm=TRUE)
    )%>% 
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 

#plot the %org carbon means and sd 
ggplot(df_plot_stock, aes(x = Location, y = mean_soc)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Location", y = expression("% Organic Carbon ")
  ) +
  labs(title = "Comparing %OC across Locations") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )
