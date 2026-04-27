#import dataset 
library(readxl)
data_polished <- read_excel("~/Documents/Master/Thesis/Data_Analysis/data_polished.xlsx")
View(data_polished)

#get rid of levels 
df_data <- data_polished %>%
  group_by(Location, Station, Plot, Year_planted, Years_passed) %>%   
  summarise(
    DBD_nc = mean(DBD_nc, na.rm = TRUE),
    DBD_wc   = mean(DBD_wc, na.rm = TRUE),
    DBD_mean  = mean(DBD_mean, na.rm = TRUE),
    DBD_no_b  = mean(DBD_no_b, na.rm = TRUE),
    m_perc_LOI = mean(perc_LOI, na.rm = TRUE),
    sd_LOI = sd(perc_LOI, na.rm = TRUE),
    m_perc_oc = mean(perc_oc, na.rm = TRUE),
    sd_oc = sd(perc_oc, na.rm = TRUE),
    .groups = "drop"
  )

#Locations right Order
df_data <- df_data %>%
  mutate(Location = trimws(Location))
df_data <- df_data %>%
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
        Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", 
                                                            "Mangguihay", "Baganga")))
  arrange(Location)

#calculate SOC density 
df_data <- df_data %>%
  mutate(
    SOC_dens_nc = DBD_nc * (m_perc_oc / 100),
    SOC_dens_wc = DBD_wc * (m_perc_oc / 100),
    SOC_dens_mean = DBD_mean * (m_perc_oc / 100),
    SOC_dens_no_b = DBD_no_b * (m_perc_oc / 100)
  )  %>%
  mutate(
    stock_50_m = (SOC_dens_mean * 50),
    stock_100_m = (SOC_dens_mean * 100)
  )%>%
  mutate(
    stock50_Mg = (stock_50_m * 100)
  )

#percentage difference 
df_data <- df_data %>%
  mutate(diff_percent = ((SOC_dens_nc - SOC_dens_wc) / SOC_dens_wc) * 100)
mean(df_data$diff_percent, na.rm = TRUE)
sd(df_data$diff_percent, na.rm = TRUE)

#make a graph comparing soc density with different dbds
#long format 
df_soc_long <- df_data %>%
  pivot_longer(
    cols = c(SOC_dens_nc, SOC_dens_wc, SOC_dens_mean, SOC_dens_no_b),
    names_to = "SOC_type",
    values_to = "SOC_density"
  ) %>%
  mutate(SOC_type = recode(SOC_type,
                           "SOC_dens_mean" = "Mean DBD",
                           "SOC_dens_nc" = "DBD w/o compaction",
                           "SOC_dens_wc" = "Measured DBD",
                           "SOC_dens_no_b" = "Mean w/o Badas"
  )) %>%
  mutate(SOC_type = factor(SOC_type,
                           levels = c( "Measured DBD", "DBD w/o compaction",
                                      "Mean DBD", "Mean w/o Badas")
  ))

#test significance 
model_soc <- aov(SOC_density ~ SOC_type + Plot, data = df_soc_long)
shapiro.test(residuals(model_soc))
qqnorm(residuals(model_soc))
qqline(residuals(model_soc))
#not normally distributed, so we do a friedmann test
#we need plot IDs for that 
df_soc_test <- df_soc_long %>%
  mutate(plot_id = paste(Location, Station, Plot, sep = "_"))

friedman.test(SOC_density ~ SOC_type | plot_id, data = df_soc_test)
#Friedman rank sum test
#data:  SOC_density and SOC_type and plot_id
# Friedman chi-squared = 84.124, df = 3, p-value < 2.2e-16

#wilcox test
pairwise.wilcox.test(df_soc_test$SOC_density,
                     df_soc_test$SOC_type,
                     paired = TRUE,
                     p.adjust.method = "BH")

#boxplot
ggplot(df_soc_long, aes(x = SOC_type, y = SOC_density)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Comparing SOC Density using different DBDs",
       x = "DBD method",
       y = expression("SOC Density (" * g ~ cm^{-3} * ")"))

#plot
df_soc_long %>% ggplot(mapping = aes(x = Plot, y = SOC_density, colour = SOC_type )) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(strip.placement = "outside") +
  labs(title = "Comparing SOC Density using different DBDs",
       y = expression("SOC Density (" * g ~ cm^{-3} * ")"),
       fill = NULL, color = NULL) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )


#make a graph comparing the DBDs 
df_dbd_long <- df_data %>%
  pivot_longer(
    cols = c(DBD_nc, DBD_wc, DBD_mean, DBD_no_b),
    names_to = "dbd_type",
    values_to = "DBD"
  )

#plot points 
df_data_long %>% ggplot(mapping = aes(x = Location, y = DBD, colour = dbd_type)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(title = "Comparing Dry Bulk Density") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#boxplot
ggplot(df_data_long, aes(x = dbd_type, y = DBD)) +
  geom_boxplot() +
  theme_minimal()

#bar plot
df_data_long %>%
  ggplot(aes(x = Location, y = DBD, fill = dbd_type)) +
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.7), alpha = 0.7) +
  geom_point(aes(color = dbd_type),
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.7),
             alpha = 0.6) +
  theme_minimal()

#table including soc density, carbon stock and stock in 1 m 
df_data_stations <- df_data %>%
  group_by(Location, Station, Year_planted, Years_passed) %>%   
  summarise(
    perc_oc = mean(m_perc_oc, na.rm = TRUE),
    sd_oc = sd(m_perc_oc, na.rm = TRUE),
    SOC_dens = mean(SOC_dens_mean, na.rm = TRUE),
    sd_SOC_dens =sd(SOC_dens_mean, na.rm = TRUE),
    stock50 = mean(stock_50_m, na.rm = TRUE),
    sd_stock50 =sd(stock_50_m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stock_50_MG = (stock50 * 100),
    sd_stock_50_MG = (sd_stock50 * 100)
  ) %>%
  mutate(
    stock100 = (stock_50_MG * 2),
    sd_stock100 = (sd_stock_50_MG * 2)
  )

View(df_data_stations)

#make an excel for the table 
library(writexl)

write_xlsx(df_data_stations, "df_data_stations.xlsx")

#comparing stations over the years
ggplot(df_station_stock, aes(x = Years_passed, y = mean_stock, color = Location)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "Years since establishment", y = 
         expression("Carbon Stock per Plot (" * g ~ cm^{-2} * ")"))

#stations as bar 
df_data_stations <- df_data_stations %>%
  mutate(Years_passed = factor(Years_passed))

#stations ohne Baganga
df_data_stations_nb <- df_data_stations %>%
  filter(Location != "Baganga")

df_data_stations_nb %>%
  ggplot(aes(x = Years_passed, y = stock_50_MG, fill = Years_passed)) +
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.7), alpha = 0.7) +
  scale_fill_manual(values = c("grey30", "grey70","grey30", "grey70","grey30", "grey70",
                               "grey30", "grey70")) +
  scale_color_manual(values = c("grey30", "grey70", "grey30", "grey70","grey30", "grey70",
                                "grey30", "grey70"))+
  theme(legend.position = "none") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(title = "Influence of Age on Carbon Stocks",
       x = "Age of Mangrove Stand (years)",
       y= expression("Carbon Stock 50 cm (" * MgC ~ ha^{-1} * ")"))+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#comparing locations on plot level 
ggplot(df_data, aes(x = Location, y = stock50_Mg)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Location", y = expression("Carbon Stock 50 cm (" * MgC ~ ha^{-1} * ")")
  ) +
  labs(title = "Comparing SOC stocks across Locations") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#with a linear test 
#comparing locations amount of carbon
library(lme4)
model_loc_new <- lmer(stock50_Mg ~ Location + (1|Station), data = df_data)
summary(model_loc_new)
#other test with p-values
library(lmerTest)
model5 <- lmer(stock50_Mg ~ Location + (1 | Station), data = df_data)
summary(model5)
#compare all locations across each other 
install.packages("emmeans")
install.packages("multcomp")
install.packages("multcompView")

library(emmeans)
library(multcomp)
library(multcompView)

emm <- emmeans(model_loc_new, ~ Location)

summary(emm, infer = TRUE)
pairs(emm, adjust = "tukey")

#we could see that the stations did not have a significant effect, so now we check with an ANOVA
anova_model_plots <- aov(stock_50cm ~ Location, data = df_plot_stock)
summary(anova_model_plots)
#pariwise comparison 
TukeyHSD(anova_model_plots)