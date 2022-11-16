########################################################################################################################

############# Summarise dependent variable at national level both cross-sectionally and longitudinally #############

# UK 4 countries trends over time plot
# Summary table of 4 countries data
# Summary table of changes over time for each country

########################################################################################################################

############# Collect national data #############

# Calculate confidence limits - this calculation will need to change for rates

obesity_uk <- read_csv("data/Obesity_UK.csv") %>%
  filter(year_num <= 2018) %>%
  filter(school_year == "rcp") %>%
  mutate(se = sqrt((value*(100-value))/denominator)
         , lcl = value - (1.96*se)
         , ucl = value + (1.96*se))

########################################################################################################################

############# UK 4 countries trends over time plot #############

ggplot(obesity_uk, aes(x = year_num, y = value, group = country, colour = country)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.1, size = 0.1) +
  labs(y = "Prevalence (%)"
       , x = "Year"
       , title = "Prevalence of overweight and obese reception children in UK\ncountries") +
  scale_x_continuous(breaks = seq(2010, 2018, 1)
                     , labels = c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19")) +
  scale_y_continuous(limits = c(10, 30)
                     , breaks = seq(10, 30, 2)
                     , expand = c(0, 0)) +
  scale_colour_manual("Country"
                      , labels = c("E" = "England", "NI" = "Northern Ireland", "S" = "Scotland", "W" = "Wales")
                      , values = c("E" = NT_colour("bright purple"), "NI" = NT_colour("bright green"), "S" = NT_colour("bright blue"), "W" = NT_colour("bright red"))) +
  NT_style() +
  theme(legend.justification = c(-0.25, 0))
ggsave("output/descriptives_dependent/plots/Trend_obesity_rcp_uk.png", width = 13.7, height = 10, units = "cm", dpi = 600)

########################################################################################################################

############# Save summary table of 4 countries data #############

write_csv(obesity_uk, "output/descriptives_dependent/Summary_obesity_rcp_uk.csv")

########################################################################################################################

############# Summary table of change over time #############

# Compare proportion overweight or obese between earliest and latest year

trends_uk <- obesity_uk %>%
  filter(!is.na(value)) %>%
  group_by(country) %>%
  filter(year_num == min(year_num)) %>%
  dplyr::select(country, year_num, year_label, value, numerator, denominator) %>%
  rename(start_year_num = year_num
         , start_year_label = year_label
         , start_value = value
         , start_numerator = numerator
         , start_denominator = denominator) %>%
  left_join(obesity_uk %>% 
              filter(!is.na(value)) %>%
              group_by(country) %>%
              filter(year_num == max(year_num)) %>%
              dplyr::select(country, year_num, year_label, value, numerator, denominator)
            , by = "country") %>%
  rename(end_year_num = year_num
         , end_year_label = year_label
         , end_value = value
         , end_numerator = numerator
         , end_denominator = denominator) %>%
  mutate(
    diff         = end_value-start_value
    , perc_diff  = (diff/start_value)*100
    , p_value    = prop.test(x = c(start_numerator, end_numerator)
                             , n = c(start_denominator, end_denominator)
                             , conf.level = 0.95
                             , correct = TRUE)$p.value
    , trend_char = case_when(diff > 0 & p_value < 0.05 ~ "Higher"
                             , p_value >= 0.05 ~ "Stable"
                             , diff < 0 &p_value < 0.05 ~ "Lower")
  ) %>%
  dplyr::select(country, start_year_label, start_value, end_year_label, end_value, diff, perc_diff, p_value, trend_char)

write_csv(trends_uk, "output/descriptives_dependent/Trends_obesity_rcp_uk.csv")

########################################################################################################################
