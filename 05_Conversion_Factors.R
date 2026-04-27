#use Script "import data" first to import data, make data frames and install packages

#combine data frames to make a combined plot
#when we want to change the order of something, we always do it by the level
#with the function of levels = c("cnjsa", "nfdj")

df_conversions <- bind_rows(df_palau, df_salmo, df_bemmelen) %>% 
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 

view(df_conversions)

# making a plot 
 
#best so far approach
df_plot_conversion %>% ggplot(mapping = aes(x = Plot, y = carbon_plot, colour = conversion)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside", legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Comparing Conversion Factors", y = "% Organic Carbon") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )
#grouping, to get rid of the levels 
df_plot_conversion <- df_conversions %>%
  group_by(Location, Station, Plot, conversion) %>%
  summarise(
    carbon_plot = mean(`% org carbon`, na.rm = TRUE),
    .groups = "drop"
  )

#statistical tests on influence of conversion factor
# paired t.test between Locations using different conversion factors
str(df_conversions)

#put data in the right format to compare
df_wide_conversion <- df_conversions %>%
  select(Location, Station, Plot, Level, conversion, `% org carbon`) %>%
  pivot_wider(
    id_cols = c(Location, Station, Plot, Level),
    names_from = conversion,
    values_from = `% org carbon`
  )

#get sample IDs to run repeated ANOVA
df_conversions <- df_conversions %>%
  mutate(SampleID = paste(Location, Station, Plot, Level, sep = "_"))
length(unique(df_conversions$SampleID)) #check if it worked

#run repeated anova
anova_conv <- aov(`% org carbon` ~ conversion + Error(SampleID/conversion),
                  data = df_conversions)

summary(anova_conv)

#now use paired t.test to compare the three factors
pairwise.t.test(
  df_conversions$`% org carbon`,
  df_conversions$conversion,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
#checking the direction of difference 
df_conversions %>%
  group_by(conversion) %>%
  summarise(mean_carbon = mean(`% org carbon`, na.rm = TRUE),
            sd_carbon   = sd(`% org carbon`, na.rm = TRUE),
            n           = sum(!is.na(`% org carbon`)))

#plotting the difference 
ggplot(df_conversions, aes(x = conversion, y = `% org carbon`)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Comparing the effect of different Conversion Factors on % org carbon") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )
