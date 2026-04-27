#data frame focusing on the levels

df_levels <- df_salmo %>% 
  select(Location, Station, Plot, Level, `% org carbon`, calc_carbon_cm, calc_carbon
  ) %>%
  mutate(Plot = str_c("P", Plot), 
         Station = str_c("S" , Station), 
         Location = factor(Location, levels = c("Fishpond", "Badas", "Tamisan", "Mangguihay", "Baganga"))) 

view(df_levels)

#plot difference % org carbon
df_levels %>% ggplot(mapping = aes(x = Plot, y = `% org carbon`, colour = Level )) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(title = "Comparing % org carbon across Sediment layers") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#plot difference % org carbon
df_levels %>% ggplot(mapping = aes(x = Plot, y = calc_carbon_cm, colour = Level )) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.6)) +
  facet_wrap(vars(Location, Station), scales = "free_x", nrow = 1, strip.position = "bottom") +
  theme_minimal() +
  theme(strip.placement = "outside") +
  labs(y = "OC per cm (" * g ~ cm^{-2} * ")", title = "Comparing Carbon Stock per cm across Sediment Layers") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#Anova Levels
#anova_levels <- aov(`% LOI` ~ Level, data = Palau)
#summary(anova_levels)
#not possible because not independent 

#linear mixed model instead
install.packages("lme4")
library(lme4)

#check with a linear mixed model 
#model without the whole plot 3 at Station 2
#add p-values 
install.packages("lmerTest")
library(lmerTest)

Palau <- Palau %>%
  filter(!(Location == "Mangguihay" &
             Station == "2" &
             Plot == "3"))
# make top level baseline 
levels(df_levels$Level)
df_levels$Level <- as.factor(df_levels$Level)
df_levels$Level <- relevel(df_levels$Level, ref = "top")

#run linear model
model_levels <- lmer(calc_carbon ~ Level + (1|Location),
                     data = df_levels)

summary(model_levels)

#run linear model with % org carbon instead of calculated carbon per cm 
level_orgcarb <- lmer(`% org carbon` ~ Level + (1|Location),
                     data = df_levels)

summary(level_orgcarb)

#look at mean and sd of levels 
level_summary <- df_levels %>%
  group_by(Level) %>%
  rm(NA)
  summarise(
    mean_OC = mean(`% org carbon`, na.rm = TRUE),
    sd_OC   = sd(`% org carbon`, na.rm = TRUE),
    n       = n()
  )
level_summary

#take Location into consideration
level_loc_summary <- df_levels %>%
  group_by(Location, Level) %>%
  summarise(
    mean_OC = mean(`% org carbon`, na.rm = TRUE),
    sd_OC   = sd(`% org carbon`, na.rm = TRUE),
    n       = n(),
    .groups = "drop"
  )
level_loc_summary
