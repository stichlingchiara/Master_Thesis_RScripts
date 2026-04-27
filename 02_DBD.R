#looking at DBD

#boxplot
ggplot(df_salmo, aes(x = Level, y = DBD)) +
  geom_boxplot() +
  facet_wrap(~ Location, nrow = 1) +
  theme_minimal() +
  labs(x = "Sediment level",
       y = "Dry bulk density (g cm⁻³)",
       title = "Dry Bulk Density across Sediment Levels by Location") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

#make boxplot of compaction comparison
ggplot(df_combined, aes(x = compaction, y = DBD)) +
  geom_boxplot() +
  facet_wrap(~ Level, nrow = 1) +
  theme_minimal() +
  labs(x = "Compaction",
       y = "Dry bulk density (" * g ~ cm^{-3} * ")",title = "Effect of Compaction Correction on DBD across Sediment Levels") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

#testing the difference, looking at the means 
#wide layout
df_wide_dbd <- df_combined %>%
  pivot_wider(
    id_cols = c(Location, Station, Plot, Level),
    names_from = compaction,
    values_from = DBD
  )

dbd_level_summary <- df_wide_dbd %>%
  group_by(Level) %>%
  summarise(
    mean_with = mean(with, na.rm = TRUE),
    sd_with   = sd(with, na.rm = TRUE),
    mean_without = mean(without, na.rm = TRUE),
    sd_without   = sd(without, na.rm = TRUE),
    n = sum(!is.na(with)),
    .groups = "drop"
  )
View(dbd_level_summary)
