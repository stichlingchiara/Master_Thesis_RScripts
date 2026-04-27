#create data frames

#dataframe with compaction
df_with <- Salmo_wc %>% 
  select(Location, Station, Plot, Level, `SOC density`, `corrected DBD`
  ) %>%
  mutate(compaction = "with") %>%
  filter(!(Location == "Mangguihay" &
             Station == "2" &
             Plot == "3")) %>%
  rename(DBD = `corrected DBD`) 
view(df_with)

# data frame without compaction
df_without <- Salmo_nc %>% 
  select(Location, Station, Plot, Level, `SOC density`, DBD
  ) %>%
  mutate(compaction = "without") %>%
  filter(!(Location == "Mangguihay" &
             Station == "2" &
             Plot == "3"))
view(df_without)

#combine data frames to make a combined plot
#when we want to change the order of something, we always do it by the level
#with the function of levels = c("cnjsa", "nfdj")

df_combined <- bind_rows(df_with, df_without) %>% 
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 

view(df_combined)

# paired t.test between places with and without compaction 
str(df_combined)

#put data in the right format to compare
df_wide <- df_combined %>%
  pivot_wider(names_from = compaction,
              values_from = `SOC density`)
View(df_wide)
#remove NAs
df_wide <- df_wide %>%
  filter(!is.na(with) & !is.na(without))
view(df_wide)
#run a paired test
test_model <- t.test(df_wide$with,
       df_wide$without,
       paired = TRUE)
test_model

#make boxplot of comparison
ggplot(df_combined, aes(x = compaction, y = `SOC density`)) +
  geom_boxplot() +
  facet_wrap(~ Level, nrow = 1) +
  theme_minimal() +
  labs(title = "Effect of compaction correction across sediment levels") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

# making a plot with all Locations, Stations, Plots
df_combined %>% ggplot(mapping = aes(x = Plot, y = `SOC density`, colour = compaction, shape = Level)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(title = "Comparing SOC Density with/without compaction") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#compare compaction with mean compaction value
#create column with plot id 
df_salmo$Compaction[105] <- NA

df_compaction <- df_salmo %>%
  mutate(Location = factor(Location,
                           levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) %>%
  group_by(Location, Station, Plot) %>%
  summarise(
    compaction_mean = mean(Compaction, na.rm = TRUE),
    dbd_mean = mean(DBD, na.rm = TRUE),
    n = n()
  ) 

df_compaction <- df_compaction %>%
  mutate(dbd_corrected = compaction_mean * dbd_mean)

View(df_compaction)

#make a plot
df_compaction_long <- df_compaction %>%
  pivot_longer(cols = c(dbd_mean, dbd_corrected),
               names_to = "type",
               values_to = "dbd")

df_compaction_long %>% 
  ggplot(mapping = aes(x = Plot, y = dbd, colour = type)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside", legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Compaction", y = expression("DBD ("* g ~ cm^{-3} *")")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )
