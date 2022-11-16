########################################################################################################################

############## Summarise dependent variables at local authority level ##############

# Local authority distribution by year
# Local authority estimates with confidence intervals and national value line in key year
# Categorise trends over time for each local authority
# Descriptive statistics of local authority values
# Identify missing values
# Detect outliers
# Detect variability in longitudinal data: method 1 and 2
# Distribution of dependent variable = normal?
# Map dependent variable

########################################################################################################################

############## Local authority distribution by year ##############

# Distribution all obesity variables

ggplot(obesity_outcome_long %>%
         group_by(variable_new, year_num) %>%
         mutate(q25               = quantile(value, na.rm = TRUE, probs = 0.25)
                , q75             = quantile(value, na.rm = TRUE, probs = 0.75)
                , iqr             = IQR(value, na.rm = TRUE)
                , outlier         = case_when((value <= (q25-(1.5*iqr)) & value > (q25-(3*iqr))) 
                                              | (value >= (q75+(1.5*iqr)) & value < (q75+(3*iqr))) ~ value)
                , extreme_outlier = case_when(value <= (q25-(3*iqr)) | value >= (q75+(3*iqr)) ~ value))
       , aes(x = as.factor(year_num), y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.5, coef = 1.5, na.rm = TRUE, colour = NT_colour("NT ink")) +
  geom_boxplot(outlier.colour = NA, na.rm = TRUE, colour = NT_colour("NT ink")) +
  geom_point(aes(x = as.factor(year_num), y = outlier), colour = NT_colour("bright red"), shape = 16, na.rm = TRUE) +
  geom_point(aes(x = as.factor(year_num), y = extreme_outlier), colour = NT_colour("bright blue"), shape = 4, na.rm = TRUE) +
  facet_wrap(~ factor(variable_new
                      , levels=c("rcp_ovw_", "rcp_ob_", "rcp_ovw_ob_")
                      , labels = c("Overweight", "Obese","Overweight and obese"))) +
  scale_x_discrete(breaks = c(2010, 2012, 2014, 2016, 2018)
                   , labels = c("2010/11", "2012/13", "2014/15", "2016/17", "2018/19")) +
  scale_y_continuous(limits = c(0, 35)
                     , breaks = seq(0, 35, 5)
                     , expand = c(0, 0)) +
  labs(y = "Prevalence (%)"
       , x = "Year"
       , title = "Local authority distribution of overweight and obese reception\nchildren prevalence by year"
       , caption = "Red dot = outlier (1.5*IQR)\nBlue cross = extreme outlier (3*IQR)") +
  NT_style() +
  theme(panel.spacing = unit(1,"lines")
        , axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("output/descriptives_dependent/plots/Dist_all_obesity_rcp_utla.png", width = 13.7, height = 10, units = "cm", dpi = 600)

# Distribution just overweight and obese

ggplot(obesity_outcome_long %>%
         filter(variable_new == "rcp_ovw_ob_") %>%
         group_by(variable_new, year_num) %>%
         mutate(q25               = quantile(value, na.rm = TRUE, probs = 0.25)
                , q75             = quantile(value, na.rm = TRUE, probs = 0.75)
                , iqr             = IQR(value, na.rm = TRUE)
                , outlier         = case_when((value <= (q25-(1.5*iqr)) & value > (q25-(3*iqr))) 
                                              | (value >= (q75+(1.5*iqr)) & value < (q75+(3*iqr))) ~ value)
                , extreme_outlier = case_when(value <= (q25-(3*iqr)) | value >= (q75+(3*iqr)) ~ value))
       , aes(x = as.factor(year_num), y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.5, coef = 1.5, na.rm = TRUE, colour = NT_colour("NT ink")) +
  geom_boxplot(outlier.colour = NA, na.rm = TRUE, colour = NT_colour("NT ink")) +
  geom_point(aes(x = as.factor(year_num), y = outlier), colour = NT_colour("bright red"), shape = 16, na.rm = TRUE) +
  geom_point(aes(x = as.factor(year_num), y = extreme_outlier), colour = NT_colour("bright blue"), shape = 4, na.rm = TRUE) +
  scale_x_discrete(breaks = seq(2010, 2018, 1)
                   , labels = c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19")) +
  scale_y_continuous(limits = c(0, 40)
                     , expand = c(0, 0)) +
  labs(y = "Prevalence (%)"
       , x = "Year"
       , title = "Local authority distribution of overweight and obese reception\nchildren prevalence by year"
       , caption = "Red dot = outlier (1.5*IQR)\nBlue cross = extreme outlier (3*IQR)") +
  NT_style()
ggsave("output/descriptives_dependent/plots/Dist_obesity_rcp_utla.png", width = 13.7, height = 10, units = "cm", dpi = 600)

#########################################################################################################################

############## Local authority estimates with confidence intervals and national value line ##############

# Confidence intervals

obesity_count_data <- read_csv("data/Obesity_Counts_UTLA.csv") %>%
  filter(school_year == "rcp") %>%
  mutate(value = (numerator/denominator)*100
         , se = sqrt((value*(100-value))/denominator)
         , lcl = value - (1.96*se)
         , ucl = value + (1.96*se))

obesity_E_1819 <- read_csv("output/descriptives_dependent/Summary_obesity_rcp_uk.csv") %>% 
  filter(country == "E" & year_num == 2018)

ggplot(obesity_count_data %>%
         filter(year_num == 2018)
       , aes(x = reorder(utla17cd, value), y = value)) +
  geom_point(size = 0.5, colour = NT_colour("NT ink")) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), size = 0.2, colour = NT_colour("NT ink")) +
  geom_hline(aes(yintercept = obesity_E_1819$value), color = NT_colour("bright purple"), size = 0.2) +
  geom_rect(aes( ymin = obesity_E_1819$lcl, ymax = obesity_E_1819$ucl, xmin = 0, xmax = Inf)
            , alpha = 1/ 150, color = NA, fill = NT_colour("bright purple")) +
  labs(y = "Prevalence (%)"
       , x = "Upper tier local authority (n = 150)"
       , title = "Prevalence of overweight and obese reception children by local\nauthority in 2018/19"
       , caption = "Isles of Scilly combined with Cornwall\nCity of London combined with Hackney") +
  scale_y_continuous(limits = c(0, 40)
                     , expand = c(0,0)) +
  NT_style() +
  theme(axis.text.x=element_blank())
ggsave("output/descriptives_dependent/plots/Dist_la_eng_obesity_rcp_utla_1819.png", width = 13.7, height = 10, units = "cm", dpi = 600)

########################################################################################################################

############## Categorise trends over time for each local authority ##############

# Change between earliest and latest data point for each LA - higher, lower, stable

trends_la <- obesity_count_data %>%
  group_by(utla17cd) %>%
  filter(year_num == min(year_num)) %>%
  dplyr::select(utla17cd, year_num, year_label, value,  numerator, denominator) %>%
  rename(start_year_num = year_num
         , start_year_label = year_label
         , start_value = value
         , start_numerator = numerator
         , start_denominator = denominator) %>%
  left_join(obesity_count_data %>% 
              group_by(utla17cd) %>%
              filter(year_num == max(year_num)) %>%
              dplyr::select(utla17cd, year_num, year_label, value,  numerator, denominator)
            , by = "utla17cd") %>%
  rename(end_year_num = year_num
         , end_year_label = year_label
         , end_value = value
         , end_numerator = numerator
         , end_denominator = denominator) %>%
  mutate(
    diff         = end_value-start_value
    , perc_diff  = (diff/start_value)*100
    , p_value    = prop.test(x = c(start_numerator, end_numerator)
                             , n = c(start_denominator, end_denominator))$p.value
    , trend_char = case_when(diff > 0 & p_value < 0.05 ~ "Higher"
                             , p_value >= 0.05 ~ "Stable"
                             , diff < 0 &p_value < 0.05 ~ "Lower")
  )

write_csv(trends_la,"output/descriptives_dependent/Trends_la_obesity_rcp_utla.csv")

# Create a summary table of trends across all LAs

trends <- trends_la %>%
  group_by(trend_char) %>%
  summarise(n = n()
            , avg_diff = mean(diff)
            , min_diff = min(diff)
            , max_diff = max(diff)
            , avg_perc_diff = mean(perc_diff)
            , min_perc_diff = min(perc_diff)
            , max_perc_diff = max(perc_diff))

write_csv(trends,"output/descriptives_dependent/Trends_obesity_rcp_utla.csv")

########################################################################################################################

############## Descriptive statistics of local authority values ##############

obesity_desc <- as_tibble(describeBy(obesity_outcome_long$value, group = list(obesity_outcome_long$year_num
                                                                              , obesity_outcome_long$variable_new)
                                     , mat = TRUE, IQR = TRUE, quant = c(.25, .75))) %>%
  rename("variable_new" = "group2"
         , "year_char" = "group1")

write_csv(obesity_desc,"output/descriptives_dependent/Summary_obesity_rcp_utla.csv")

########################################################################################################################

############## Identify missing values ##############

obesity_missing <- obesity_outcome_long %>%
  group_by(variable_new, year_num) %>%
  summarise(missing = sum(is.na(value))) %>%
  filter(missing > 0)

write_csv(obesity_missing, "output/descriptives_dependent/Missing_obesity_rcp_utla.csv")

obesity_missing_la <- obesity_outcome_long %>%
  filter(is.na(value)) %>%
  dplyr::select(utla17cd, variable_new, year_num, year_label)

write_csv(obesity_missing_la, "output/descriptives_dependent/Missing_la_obesity_rcp_utla.csv")

########################################################################################################################

############## Detect outliers ##############

# outliers = mean +/- (3 * sd) or Q1/3 +/- (1.5* IQR)
# extreme outliers = Q1/3 +/- (3 * IQR)

obesity_desc_limits <- obesity_desc %>%
  dplyr::select(variable_new, year_char, mean, sd, Q0.25, Q0.75, IQR) %>%
  mutate(loweriqr_1.5 = Q0.25 - (IQR * 1.5)
         , upperiqr_1.5 = Q0.75 + (IQR * 1.5)
         , lowersd_3 = mean - (sd * 3)
         , uppersd_3 = mean + (sd * 3)
         , loweriqr_3 = Q0.25 - (IQR * 3)
         , upperiqr_3 = Q0.75 + (IQR * 3)
  )

obesity_outcome_long_stat <- obesity_outcome_long %>%
  left_join(obesity_desc_limits, by = c("variable_new", "year_char")) %>%
  mutate(iqr_outlier_tot_1.5 = case_when(value > upperiqr_1.5 | value < loweriqr_1.5 ~ 1,
                                         TRUE ~ 0)
         , iqr_outlier_high_1.5 = case_when(value > upperiqr_1.5 ~ 1,
                                            TRUE ~ 0)
         , iqr_outlier_low_1.5 = case_when(value < loweriqr_1.5 ~ 1,
                                           TRUE ~ 0)
         , sd_outlier_tot_3 = case_when(value > uppersd_3 | value < lowersd_3 ~ 1,
                                        TRUE ~ 0)
         , sd_outlier_high_3 = case_when(value > uppersd_3 ~ 1,
                                         TRUE ~ 0)
         , sd_outlier_low_3 = case_when(value < lowersd_3 ~ 1,
                                        TRUE ~ 0)
         , iqr_outlier_tot_3 = case_when(value > upperiqr_3 | value < loweriqr_3 ~ 1,
                                         TRUE ~ 0)
         , iqr_outlier_high_3 = case_when(value > upperiqr_3 ~ 1,
                                          TRUE ~ 0)
         , iqr_outlier_low_3 = case_when(value < loweriqr_3 ~ 1,
                                         TRUE ~ 0)  
  )

obesity_outliers <- obesity_outcome_long_stat %>%
  group_by(variable_new, year_num) %>%
  summarise(countiqr_tot_1.5 = sum(iqr_outlier_tot_1.5, na.rm = TRUE)
            , countiqr_high_1.5 = sum(iqr_outlier_high_1.5, na.rm = TRUE)
            , countiqr_low_1.5 = sum(iqr_outlier_low_1.5, na.rm = TRUE)
            , countsd_tot_3 = sum(sd_outlier_tot_3, na.rm = TRUE)
            , countsd_low_3 = sum(sd_outlier_low_3, na.rm = TRUE)
            , countsd_high_3 = sum(sd_outlier_high_3, na.rm = TRUE)
            , countiqr_tot_3 = sum(iqr_outlier_tot_3, na.rm = TRUE)
            , countiqr_high_3 = sum(iqr_outlier_high_3, na.rm = TRUE)
            , countiqr_low_3 = sum(iqr_outlier_low_3, na.rm = TRUE)
  )

write_csv(obesity_outliers,"output/descriptives_dependent/Outliers_obesity_rcp_utla.csv")

obesity_outliers_la <- obesity_outcome_long_stat %>%
  filter(sd_outlier_tot_3 == 1 | iqr_outlier_tot_1.5 == 1) %>%
  dplyr::select(year_num, utla17cd, variable
                , iqr_outlier_tot_1.5
                , sd_outlier_tot_3, iqr_outlier_tot_3
  )

write_csv(obesity_outliers_la,"output/descriptives_dependent/Outliers_la_obesity_rcp_utla.csv")

########################################################################################################################

############## Detect variability in longitudinal data ##############

## Method 1
#	At each time point, compare the average value across all local authorities to each iindividual local authority value

m1_obesity_se_avg <- obesity_outcome_long %>%
  group_by(variable_new, year_num) %>%
  summarise(avg_value = mean(value, na.rm = TRUE))

m1_obesity_outcome_long_avg <- obesity_outcome_long %>%
  left_join(m1_obesity_se_avg
            , by = c("variable_new", "year_num")) %>%
  mutate(diff = avg_value - value) 

m1_obesity_outcome_avg_diff <- m1_obesity_outcome_long_avg %>%
  group_by(variable_new, utla17cd) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE)
            , avg_diff = mean(diff, na.rm = TRUE)
            , sd_diff = sd(diff, na.rm = TRUE)) %>%
  mutate(abs_total_diff = abs(total_diff)
         , abs_avg_diff = abs(avg_diff)) %>%
  filter(!is.na(sd_diff))
# If sd is NA then not enough time points so excluded

m1_variability_limits <- m1_obesity_outcome_avg_diff %>%
  group_by(variable_new) %>%
  summarise(q25 = quantile(sd_diff, 0.25, na.rm = TRUE)
            , q75 = quantile(sd_diff, 0.75, na.rm = TRUE)
            , iqr = q75-q25
            , low1.5 = q25 - (1.5 * iqr)
            , low3 = q25 - (3 * iqr)
            , high1.5 = q75 + (1.5 * iqr)
            , high3 = q75 + (3 * iqr)
  )

ggplot(m1_obesity_outcome_avg_diff, aes(x = reorder_within(utla17cd,sd_diff,variable_new), y = sd_diff)) +
  geom_point(size = 0.2, colour = NT_colour("NT ink")) +
  geom_hline(data = m1_variability_limits, mapping = aes(yintercept = high1.5), colour = NT_colour("bright blue")) +
  geom_hline(data = m1_variability_limits, mapping = aes(yintercept = high3), colour = NT_colour("bright red")) +
  geom_hline(data = m1_variability_limits, mapping = aes(yintercept = low1.5), colour = NT_colour("bright blue")) +
  geom_hline(data = m1_variability_limits, mapping = aes(yintercept = low3), colour = NT_colour("bright red")) +
  facet_wrap(~ factor(variable_new
                      , levels=c("rcp_ovw_", "rcp_ob_", "rcp_ovw_ob_")
                      , labels = c("Overweight", "Obese","Overweight and obese"))
             , scales = "fixed") +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Local authority"
       , y = "Standard deviation of difference"
       , title = "Standard deviation of difference to all local authorities yearly means"
       , caption = "Blue line = 1.5 x IQR\nRed line = 3 x IQR") +
  NT_style() +
  theme(panel.spacing = unit(1,"lines")
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank())
ggsave("output/descriptives_dependent/plots/Variability_m1_sd_diff_obesity_rcp_utla.png", width = 13.7, height = 10, units = "cm", dpi = 600)

ggplot(m1_obesity_outcome_avg_diff, aes(y = sd_diff)) +
  stat_boxplot(geom = "errorbar", width = 0.2, colour = NT_colour("NT ink")) +
  geom_boxplot(outlier.colour = NT_colour("bright red"), outlier.shape = 8, colour = NT_colour("NT ink")) +
  facet_wrap(~ factor(variable_new
                      , levels=c("rcp_ovw_", "rcp_ob_", "rcp_ovw_ob_")
                      , labels = c("Overweight", "Obese","Overweight and obese"))
             , scales = "fixed") +
  labs(y = "Standard deviation of difference"
       , title = "Boxplot of standard deviation of difference to all local authorities yearly\nmeans"
       , caption = "Red outliers = greater than 1.5 x IQR") +
  scale_y_continuous(limits = c(0, NA)) +
  NT_style() +
  theme(panel.spacing = unit(1,"lines")
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank()
        , axis.title.x = element_blank())
ggsave("output/descriptives_dependent/plots/Variability_m1_box_obesity_rcp_utla.png", width = 13.7, height = 10, units = "cm", dpi = 600)

m1_variability_outliers_summary <- m1_obesity_outcome_avg_diff %>%
  left_join(m1_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(variable_new) %>%
  summarise(outliers_1.5 = sum(outlier_1.5)
            , outliers_3 = sum(outlier_3)) 

write_csv(m1_variability_outliers_summary, "output/descriptives_dependent/Variability_outliers_m1_obesity_rcp_utla.csv")

m1_variability_la_outliers_summary <- m1_obesity_outcome_avg_diff %>%
  left_join(m1_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  filter(outlier_1.5 == 1) %>%
  dplyr::select(variable_new, utla17cd, outlier_1.5, outlier_3)

write_csv(m1_variability_la_outliers_summary, "output/descriptives_dependent/Variability_outliers_m1_la_obesity_rcp_utla.csv")

ggplot(m1_obesity_outcome_long_avg %>%
         left_join(m1_variability_outliers_summary, by = "variable_new") %>%
         inner_join(m1_variability_la_outliers_summary, by = c("utla17cd", "variable_new")), aes(group = utla17cd, colour = utla17cd)) +
  geom_point(aes(x = factor(year_num), y = value), size = 1) +
  geom_line(aes(x = factor(year_num), y = value)) +
  facet_wrap(~ factor(variable_new
                      , levels=c("rcp_ovw_", "rcp_ob_", "rcp_ovw_ob_")
                      , labels = c("Overweight", "Obese","Overweight and obese"))
             , scales = "fixed") +
  labs(x = "Year"
       , y = "Prevalence"
       , title = "Local authorities with more than 1.5*IQR in difference to all local\nauthorities yearly means") +
  scale_x_discrete(breaks = c(2010, 2012, 2014, 2016, 2018)
                   , labels = c("2010/11", "2012/13", "2014/15", "2016/17", "2018/19")) +
  scale_y_continuous(limits = c(0, NA)
                     , expand = c(0, 0)) +
  scale_colour_NT("Local authority", palette = NT_palette()) +
  NT_style() +
  theme(panel.spacing = unit(1, "lines")
        , axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1 ))
ggsave("output/descriptives_dependent/plots/Variability_outliers_m1_la_obesity_rcp_utla.png", width = 13.7, height = 10, units = "cm", dpi = 600)

#######################################################

## Method 2
# At each time point, use linear regression to estimate LA value and compare to actual value

m2_fitted_models <- obesity_outcome_long %>%
  group_by(variable_new, utla17cd) %>%
  do(model = lm(value ~ year_num, data = .)) %>%
  mutate(a = summary(model)$coefficients[1]
         , b = summary(model)$coefficients[2]) %>%
  dplyr::select(-model)

m2_obesity_outcome_long_avg <- obesity_outcome_long %>%
  left_join(m2_fitted_models
            , by = c("variable_new", "utla17cd")) %>%
  mutate(fit = a + (b * year_num)
         , diff = value - fit) 

m2_obesity_outcome_avg_diff <- m2_obesity_outcome_long_avg %>%
  group_by(variable_new, utla17cd) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE)
            , avg_diff = mean(diff, na.rm = TRUE)
            , sd_diff = sd(diff, na.rm = TRUE)) %>%
  mutate(abs_total_diff = abs(total_diff)
         , abs_avg_diff = abs(avg_diff))

m2_variability_limits <- m2_obesity_outcome_avg_diff %>%
  group_by(variable_new) %>%
  summarise(q25 = quantile(sd_diff, 0.25, na.rm = TRUE)
            , q75 = quantile(sd_diff, 0.75, na.rm = TRUE)
            , iqr = q75-q25
            , low1.5 = q25 - (1.5*iqr)
            , low3 = q25 - (3*iqr)
            , high1.5 = q75 + (1.5*iqr)
            , high3 = q75 + (3*iqr)
  )

ggplot(m2_obesity_outcome_avg_diff, aes(x = reorder_within(utla17cd,sd_diff,variable_new), y = sd_diff)) +
  geom_point(size = 0.2, colour = NT_colour("NT ink")) +
  geom_hline(data = m2_variability_limits, mapping = aes(yintercept = high1.5), colour = NT_colour("bright blue")) +
  geom_hline(data = m2_variability_limits, mapping = aes(yintercept = high3), colour = NT_colour("bright red")) +
  geom_hline(data = m2_variability_limits, mapping = aes(yintercept = low1.5), colour = NT_colour("bright blue")) +
  geom_hline(data = m2_variability_limits, mapping = aes(yintercept = low3), colour = NT_colour("bright red")) +
  facet_wrap(~ factor(variable_new
                      , levels=c("rcp_ovw_", "rcp_ob_", "rcp_ovw_ob_")
                      , labels = c("Overweight", "Obese","Overweight and obese"))
             , scales = "fixed") +
  scale_x_reordered() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Local authority"
       , y = "Standard deviation of difference"
       , title = "Standard deviation of difference to yearly fitted values"
       , caption = "Blue line = 1.5 x IQR\nRed line = 3 x IQR") +
  NT_style() +
  theme(panel.spacing = unit(1,"lines")
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank())
ggsave("output/descriptives_dependent/plots/Variability_m2_sd_diff_obesity_rcp_utla.png", width = 13.7, height = 10, units = "cm", dpi = 600)

ggplot(m2_obesity_outcome_avg_diff, aes(y = sd_diff)) +
  stat_boxplot(geom = "errorbar", width = 0.2, colour = NT_colour("NT ink")) +
  geom_boxplot(outlier.colour = NT_colour("bright red"), outlier.shape = 8, colour = NT_colour("NT ink")) +
  facet_wrap(~ factor(variable_new
                      , levels=c("rcp_ovw_", "rcp_ob_", "rcp_ovw_ob_")
                      , labels = c("Overweight", "Obese","Overweight and obese"))
             , scales = "fixed") +
  labs(y = "Standard deviation of difference"
       , title = "Boxplot of standard deviation of difference to yearly fitted values"
       , caption = "Red outliers = greater than 1.5 x IQR") +
  scale_y_continuous(limits = c(0, NA)) +
  NT_style() +
  theme(panel.spacing = unit(1,"lines")
        , axis.ticks.x = element_blank()
        , axis.text.x = element_blank()
        , axis.title.x = element_blank())
ggsave("output/descriptives_dependent/plots/Variability_m2_box_obesity_rcp_utla.png", width = 13.7, height = 10, units = "cm", dpi = 600)

m2_variability_outliers_summary <- m2_obesity_outcome_avg_diff %>%
  left_join(m2_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(variable_new) %>%
  summarise(outliers_1.5 = sum(outlier_1.5)
            , outliers_3 = sum(outlier_3)) 

write_csv(m2_variability_outliers_summary, "output/descriptives_dependent/Variability_outliers_m2_obesity_rcp_utla.csv")

m2_variability_la_outliers_summary <- m2_obesity_outcome_avg_diff %>%
  left_join(m2_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  filter(outlier_1.5 == 1) %>%
  dplyr::select(variable_new, utla17cd, outlier_1.5, outlier_3)

write_csv(m2_variability_la_outliers_summary, "output/descriptives_dependent/Variability_outliers_m2_la_obesity_rcp_utla.csv")

ggplot(m2_obesity_outcome_long_avg %>%
         left_join(m2_variability_outliers_summary, by = "variable_new") %>%
         inner_join(m2_variability_la_outliers_summary, by = c("utla17cd", "variable_new")), aes(group = utla17cd, colour = utla17cd)) +
  geom_point(aes(x = factor(year_num), y = value), size = 1) +
  geom_line(aes(x = factor(year_num), y = value)) +
  facet_wrap(~ factor(variable_new
                      , levels=c("rcp_ovw_", "rcp_ob_", "rcp_ovw_ob_")
                      , labels = c("Overweight", "Obese","Overweight and obese"))
             , scales = "fixed") +
  labs(x = "Year"
       , y = "Prevalence"
       , title = "Local authorities with more than 1.5*IQR in difference to yearly fitted\nvalues") +
  scale_x_discrete(breaks = c(2010, 2012, 2014, 2016, 2018)
                   , labels = c("2010/11", "2012/13", "2014/15", "2016/17", "2018/19")) +
  scale_y_continuous(limits = c(0, NA)
                     , expand = c(0, 0)) +
  scale_colour_NT("Local authority", palette = NT_palette()) +
  NT_style() +
  theme(panel.spacing = unit(1, "lines")
        , axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1 ))
ggsave("output/descriptives_dependent/plots/Variability_outliers_m2_la_obesity_rcp_utla.png", width = 13.7, height = 10, units = "cm", dpi = 600)

########################################################################################################################

############## Distribution of dependent variable = normal? ##############

ggplot(obesity_outcome_long %>%
         left_join(obesity_desc %>%
                     dplyr::select(variable_new, year_char, mean, skew)
                   , by = c("variable_new", "year_char"))
       , aes(x = value)) +
  geom_density(fill = "#D3C4FC", color = NT_colour("NT ink")) +
  geom_vline(aes(xintercept = mean), color = NT_colour("NT ink"), linetype = "dotted", size = 0.5) +
  geom_text(obesity_desc 
            , mapping=aes(x=Inf, y=Inf, label = paste("skew: ",round(skew,2)))
            , hjust = "inward", vjust = "inward"
            , size = 3, colour = NT_colour("NT ink")) +
  facet_grid(year_char ~ factor(variable_new
                                , levels=c("rcp_ovw_", "rcp_ob_", "rcp_ovw_ob_")
                                , labels = c("Overweight", "Obese","Overweight and obese")), scales = "free_x") +
  labs(y = "Frequency"
       , x = "Prevalence (%)"
       , title = "Density plots: reception obesity outcomes all years") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  NT_style() +
  theme(panel.spacing = unit(1,"lines"))
ggsave("output/descriptives_dependent/plots/Normality_obesity_rcp_utla.png", width = 13.7, height = 17, units = "cm", dpi = 600)

ggplot(obesity_outcome_long %>%
         filter(variable_new == "rcp_ovw_ob_" & year_char == "2018") %>%
         left_join(obesity_desc %>%
                     dplyr::select(variable_new, year_char, mean, skew)
                   , by = c("variable_new", "year_char"))
       , aes(x = value)) +
  geom_density(fill = "#D3C4FC", color = NT_colour("NT ink")) +
  geom_vline(aes(xintercept = mean), color = NT_colour("NT ink"), linetype = "dotted", size = 1) +
  geom_text(obesity_desc %>%
              filter(variable_new == "rcp_ovw_ob_" & year_char == "2018")
            , mapping=aes(x=Inf, y=Inf, label = paste("skew: ",round(skew,2)))
            , hjust = "inward", vjust = "inward", colour = NT_colour("NT ink")) +
  labs(y = "Frequency"
       , x = "Prevalence (%)"
       , title = "Density plot: prevalence of overweight or obese reception children\nin 2018/19") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  NT_style() 
ggsave("output/descriptives_dependent/plots/Normality_obesity_rcp_utla_1819.png", width = 13.7, height = 10, units = "cm", dpi = 600)

########################################################################################################################

############## Map dependent variable ##############

# https://geoportal.statistics.gov.uk/maps/counties-and-unitary-authorities-december-2017-ew-bfe

shapefile_eng <- st_read("data/County_UA_Dec17_EW_BFE/County_UA_Dec17_EW_BFE.shp", layer = "County_UA_Dec17_EW_BFE") %>%
  clean_names() %>% 
  filter(str_detect(ctyua17cd, "^E")) %>%
  mutate(ctyua17cd = case_when(ctyua17cd == "E06000053" ~ "E06000052"
                               , ctyua17cd == "E09000001" ~ "E09000012"
                               , TRUE ~ ctyua17cd)) %>%
  left_join(obesity_outcome %>%
              dplyr::select(utla17cd, rcp_ovw_ob_1819) %>%
              mutate(rcp_ovw_ob_1819_round = round(rcp_ovw_ob_1819,2)
                     , q0 = round(quantile(rcp_ovw_ob_1819_round, probs = 0, na.rm = TRUE), 1)
                     , q2 = round(quantile(rcp_ovw_ob_1819_round, probs = 0.2, na.rm = TRUE), 1)
                     , q4 = round(quantile(rcp_ovw_ob_1819_round, probs = 0.4, na.rm = TRUE), 1)
                     , q6 = round(quantile(rcp_ovw_ob_1819_round, probs = 0.6, na.rm = TRUE), 1)
                     , q8 = round(quantile(rcp_ovw_ob_1819_round, probs = 0.8, na.rm = TRUE), 1)
                     , q10 = round(quantile(rcp_ovw_ob_1819_round, probs = 1, na.rm = TRUE), 1)
                     , rate_bucket = case_when(rcp_ovw_ob_1819_round < q2 ~ "1"
                                               , rcp_ovw_ob_1819_round >= q2 & rcp_ovw_ob_1819_round < q4 ~ "2"
                                               , rcp_ovw_ob_1819_round >= q4 & rcp_ovw_ob_1819_round < q6 ~ "3"
                                               , rcp_ovw_ob_1819_round >= q6 & rcp_ovw_ob_1819_round < q8 ~ "4"
                                               , rcp_ovw_ob_1819_round >= q8 ~ "5"))
            , by = c("ctyua17cd" = "utla17cd"))

england <- ggplot(shapefile_eng) +
  geom_sf(aes(fill = rate_bucket), colour = NT_colour("NT ink")) +
  coord_sf(datum = NA) +
  scale_fill_NT("Prevalence (%)"
                , palette = NT_palette("purple", reverse = TRUE)
                , labels = c("15.6 - <20.4", "20.4 - <22.2", "22.2 - <23.6", "23.6 - <25.0", "25.0 - <29.6")) +
  NT_style() +
  theme(
    axis.text = element_blank()
    , axis.title = element_blank()
    , legend.position = c(0.13, 0.5)
    , legend.text = element_text(colour = NT_colour("NT ink"), size = 8, face = "bold", family = "sans")
    , legend.title = element_text(colour = NT_colour("NT ink"), size = 8, face = "bold", family = "sans", hjust = 0)
    , panel.grid = element_blank()
  )


london <- ggplot(shapefile_eng %>% filter(str_detect(ctyua17cd, "^E09"))) +
  geom_sf(aes(fill = rate_bucket), colour = NT_colour("NT ink")) +
  coord_sf(datum = NA) +
  scale_fill_NT(palette = NT_palette("purple", reverse = TRUE)) +
  NT_style() +
  theme(
    axis.text = element_blank()
    , axis.title = element_blank()
    , legend.position = "none"
    , panel.grid = element_blank()
    , plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ="cm")
    , panel.border = element_rect(colour = NT_colour("NT ink"), fill = NA)
  )

ggdraw() +
  draw_plot(england) +
  draw_plot(london, x = 0.07, y = 0.7, width = 0.3, height = 0.3)

ggsave("output/descriptives_dependent/plots/map_rcp_obesity_utla_1819.png", width = 13.7, height = 15, units = "cm", dpi = 600)

########################################################################################################################
