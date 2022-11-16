########################################################################################################################

############# Summarise independent variables at local authority level #############

# Local authority distribution by year
# Descriptive statistics of lcoal authority values
# Identify missing values
# Detect outliers
# Detect variability in longitudinal data: method 1 and 2
# Distribution of independent variables
# Correlation with other independent variables

########################################################################################################################

############# Local authority distribution by year #############

npages_dist <- n_pages(ggplot(obesity_vars_long, aes(x = as.factor(year_num), y = value)) +
                         facet_wrap_paginate(~ variable_new, ncol = 2, nrow = 3, scales = "free_y"))

pdf("output/descriptives_independent/plots/Dist_vars_obesity_rcp_utla.pdf")

for(i in 1:npages_dist){
  
  print(
    ggplot(obesity_vars_long %>%
             group_by(variable_new, year_num) %>%
             mutate(q25               = quantile(value, na.rm = TRUE, probs = 0.25)
                    , q75             = quantile(value, na.rm = TRUE, probs = 0.75)
                    , iqr             = IQR(value, na.rm = TRUE)
                    , outlier         = case_when((value <= (q25-(1.5*iqr)) & value > (q25-(3*iqr)))
                                                  | (value >= (q75+(1.5*iqr)) & value < (q75+(3*iqr))) ~ value)
                    , extreme_outlier = case_when(value <= (q25-(3*iqr)) | value >= (q75+(3*iqr)) ~ value))
           , aes(x = as.factor(year_num), y = value)) +
      stat_boxplot(geom = "errorbar", width = 0.5, coef = 1.5, colour = NT_colour("NT ink")) +
      geom_boxplot(outlier.colour = NA, colour = NT_colour("NT ink")) +
      geom_point(aes(x = as.factor(year_num), y = outlier), colour = NT_colour("bright red"), shape = 16, na.rm = TRUE) +
      geom_point(aes(x = as.factor(year_num), y = extreme_outlier), colour = NT_colour("bright blue"), shape = 4, na.rm = TRUE) +
      facet_wrap_paginate(~ variable_new, ncol = 2, nrow = 3, scales = "free_y", page = i) +
      labs(x = "Year"
           , title = "Local authority distribution of obesity variables by year"
           , caption = "Red dot = outlier (1.5*IQR)\nBlue cross = extreme outlier (3*IQR)") +
      scale_x_discrete(breaks = c(2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
      NT_style() +
      theme(panel.spacing = unit(1,"lines"))
  )
  
}

dev.off()

########################################################################################################################

############# Descriptive stats of local authority values #############

obesity_desc <- as_tibble(describeBy(obesity_vars_long$value, group = obesity_vars_long$variable
                                     , mat = TRUE, IQR = TRUE, quant = c(.25, .75))) %>%
  rename("variable" = "group1") %>%
  mutate(year1 = str_sub(variable,-3,-1)
         , year2 = case_when(str_detect(str_sub(year1,1,1),"_") ~ 1
                             , TRUE ~ 0)
         , year_char = case_when(year2 == 0 ~ paste0(20,str_sub(variable,-4,-3))
                                 , TRUE ~ paste0(20,str_sub(variable,-2,-1)))
         , year_num = as.numeric(year_char)
         , year_label = case_when(year2 == 0 ~  paste0(year_char,"/",str_sub(variable,-2,-1))
                                  , TRUE ~ year_char)
         , variable_new = case_when(year2 == 0 ~ str_sub(variable, end=-5)
                                    , TRUE ~  str_sub(variable,end=-3))) %>%
  dplyr::select(-year1, -year2)

write_csv(obesity_desc,"output/descriptives_independent/Summary_vars_obesity_rcp_utla.csv")

########################################################################################################################

############# Identify missing values #############

obesity_missing <- obesity_vars_long %>%
  group_by(variable, variable_new, year_num, year_label) %>%
  summarise(missing = sum(is.na(value))) %>%
  filter(missing > 0)

write_csv(obesity_missing, "output/descriptives_independent/Missing_vars_obesity_rcp_utla.csv")

obesity_missing_la <- obesity_vars_long %>%
  filter(is.na(value)) %>%
  dplyr::select(utla17cd, variable, variable_new, year_num, year_label)

write_csv(obesity_missing_la, "output/descriptives_independent/Missing_la_vars_obesity_rcp_utla.csv")

########################################################################################################################

############# Detect outliers #############

# outliers = mean +/- (3 * sd) or Q1/3 +/- (1.5* IQR)
# extreme outliers = Q1/3 +/- (3 * IQR)

obesity_desc_limits <- obesity_desc %>%
  dplyr::select(variable, mean, sd, Q0.25, Q0.75, IQR) %>%
  mutate(loweriqr_1.5 = Q0.25 - (IQR * 1.5)
         , upperiqr_1.5 = Q0.75 + (IQR * 1.5)
         , lowersd_3 = mean - (sd * 3)
         , uppersd_3 = mean + (sd * 3)
         , loweriqr_3 = Q0.25 - (IQR * 3)
         , upperiqr_3 = Q0.75 + (IQR * 3)
  )

obesity_vars_long_stat <- obesity_vars_long %>%
  left_join(obesity_desc_limits, by = "variable") %>%
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

obesity_outliers <- obesity_vars_long_stat %>%
  group_by(variable, year_num, year_label) %>%
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

write_csv(obesity_outliers,"output/descriptives_independent/Outliers_vars_obesity_rcp_utla.csv")

obesity_outliers_la <- obesity_vars_long_stat %>%
  filter(sd_outlier_tot_3 == 1 | iqr_outlier_tot_1.5 == 1) %>%
  dplyr::select(year_num, year_label, utla17cd, variable
                , iqr_outlier_tot_1.5
                , sd_outlier_tot_3, iqr_outlier_tot_3
                # , sd_outlier_tot_5, iqr_outlier_tot_5
                # , sd_outlier_tot_10, iqr_outlier_tot_10
  )

write_csv(obesity_outliers_la,"output/descriptives_independent/Outliers_la_vars_obesity_rcp_utla.csv")

########################################################################################################################

############# Detect variability in longitudinal data #############

## Method 1
#	At each time point, compare the average value across all local authorities to each iindividual local authority value

m1_obesity_se_avg <- obesity_vars_long %>%
  group_by(variable_new, year_num) %>%
  summarise(avg_value = mean(value, na.rm = TRUE))

m1_obesity_vars_long_avg <- obesity_vars_long %>%
  left_join(m1_obesity_se_avg
            , by = c("variable_new", "year_num")) %>%
  mutate(diff = avg_value - value) 

m1_obesity_vars_avg_diff <- m1_obesity_vars_long_avg %>%
  group_by(variable_new, utla17cd) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE)
            , avg_diff = mean(diff, na.rm = TRUE)
            , sd_diff = sd(diff, na.rm = TRUE)) %>%
  mutate(abs_total_diff = abs(total_diff)
         , abs_avg_diff = abs(avg_diff)) %>%
  filter(!is.na(sd_diff))
#if sd is NA then not enough time points so excluded

m1_variability_limits <- m1_obesity_vars_avg_diff %>%
  group_by(variable_new) %>%
  summarise(q25 = quantile(sd_diff, 0.25, na.rm = TRUE)
            , q75 = quantile(sd_diff, 0.75, na.rm = TRUE)
            , iqr = q75-q25
            , low1.5 = q25 - (1.5 * iqr)
            , low3 = q25 - (3 * iqr)
            , high1.5 = q75 + (1.5 * iqr)
            , high3 = q75 + (3 * iqr)
  )

npages_dist_var_m1 <- n_pages(ggplot(m1_obesity_vars_avg_diff, aes(x = reorder_within(utla17cd,sd_diff,variable_new), y = sd_diff)) +
                                geom_point(size = 0.2) +
                                geom_hline(data = m1_variability_limits, mapping = aes(yintercept = high1.5), colour = NT_colour("bright blue")) +
                                geom_hline(data = m1_variability_limits, mapping = aes(yintercept = high3), colour = NT_colour("bright red")) +
                                geom_hline(data = m1_variability_limits, mapping = aes(yintercept = low1.5), colour = NT_colour("bright blue")) +
                                geom_hline(data = m1_variability_limits, mapping = aes(yintercept = low3), colour = NT_colour("bright red")) +
                                facet_wrap_paginate(~ variable_new, scales = "free", nrow = 3, ncol = 2))

pdf("output/descriptives_independent/plots/Variability_m1_sd_diff_vars_obesity_rcp_utla.pdf")

for(i in 1:npages_dist_var_m1){
  
  print(
    
    ggplot(m1_obesity_vars_avg_diff, aes(x = reorder_within(utla17cd,sd_diff,variable_new), y = sd_diff)) +
      geom_point(size = 0.2) +
      geom_hline(data = m1_variability_limits, mapping = aes(yintercept = high1.5), colour = NT_colour("bright blue")) +
      geom_hline(data = m1_variability_limits, mapping = aes(yintercept = high3), colour = NT_colour("bright red")) +
      geom_hline(data = m1_variability_limits, mapping = aes(yintercept = low1.5), colour = NT_colour("bright blue")) +
      geom_hline(data = m1_variability_limits, mapping = aes(yintercept = low3), colour = NT_colour("bright red")) +
      facet_wrap_paginate(~ variable_new, scales = "free", nrow = 3, ncol = 2, page = i) +
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
  )
  
}

dev.off()

npages_box_var_m1 <- n_pages(ggplot(m1_obesity_vars_avg_diff, aes(y = sd_diff)) +
                               stat_boxplot(geom = "errorbar", width = 0.2) +
                               geom_boxplot(outlier.colour = NT_colour("bright red"), outlier.shape = 8) +
                               facet_wrap_paginate(~ variable_new, scales = "free", nrow = 3, ncol = 2))

pdf("output/descriptives_independent/plots/Variability_m1_box_vars_obesity_rcp_utla.pdf")

for(i in 1:npages_box_var_m1){
  
  print(
    ggplot(m1_obesity_vars_avg_diff, aes(y = sd_diff)) +
      stat_boxplot(geom = "errorbar", width = 0.2, colour = NT_colour("NT ink")) +
      geom_boxplot(outlier.colour = NT_colour("bright red"), outlier.shape = 8, colour = NT_colour("NT ink")) +
      facet_wrap_paginate(~ variable_new, scales = "free", nrow = 3, ncol = 2, page = i) +
      labs(y = "Standard deviation of difference"
           , title = "Boxplot of standard deviation of difference to all local authorities yearly means"
           , caption = "Red outliers = greater than 1.5 x IQR") +
      NT_style() +
      theme(panel.spacing = unit(1,"lines")
            , axis.ticks.x = element_blank()
            , axis.text.x = element_blank()
            , axis.title.x = element_blank())
  )
  
}

dev.off()

m1_variability_outliers_summary <- m1_obesity_vars_avg_diff %>%
  left_join(m1_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(variable_new) %>%
  summarise(outliers_1.5 = sum(outlier_1.5)
            , outliers_3 = sum(outlier_3)) 

write_csv(m1_variability_outliers_summary, "output/descriptives_independent/Variability_outliers_m1_vars_obesity_rcp_utla.csv")

m1_variability_la_outliers_summary <- m1_obesity_vars_avg_diff %>%
  left_join(m1_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  filter(outlier_1.5 == 1) %>%
  dplyr::select(variable_new, utla17cd, outlier_1.5, outlier_3)

write_csv(m1_variability_la_outliers_summary, "output/descriptives_independent/Variability_outliers_m1_la_vars_obesity_rcp_utla.csv")

m1_outliers_la_plot <- m1_obesity_vars_long_avg %>%
  left_join(m1_variability_outliers_summary, by = "variable_new") %>%
  inner_join(m1_variability_la_outliers_summary, by = c("utla17cd", "variable_new"))

m1_ind_list_plot <- unique(m1_outliers_la_plot$variable_new)

pdf("output/descriptives_independent/plots/Variability_outliers_m1_la_vars_obesity_rcp_utla.pdf")

for(i in m1_ind_list_plot){
  
  print(
    ggplot(m1_outliers_la_plot %>%
             filter(variable_new == i)) +
      geom_point(aes(x = factor(year_num), y = value, group = utla17cd, colour = utla17cd), size = 1) +
      geom_line(aes(x = factor(year_num), y = value, group = utla17cd, colour = utla17cd)) +
      geom_line(data = m1_obesity_se_avg %>%
                  filter(variable_new == i), mapping = aes(x = factor(year_num), y = avg_value, group = variable_new)
                , colour = NT_colour("NT ink"), lty = "dashed") +
      labs(x = "Year"
           , title = i
           , caption = "-- = local authority average at each time point") +
      scale_colour_manual("Local authority"
                          , values = (colorRampPalette(RColorBrewer::brewer.pal(12,"Paired"))(as_vector(m1_outliers_la_plot %>%
                                                                                                          filter(variable_new == i) %>%
                                                                                                          summarise(n = n_distinct(utla17cd)))))) +
      NT_style() +
      theme(panel.spacing = unit(1, "lines"))
  )
  
}

dev.off()

#######################################################

## Method 2
# At each time point, use linear regression to estimate LA value and compare to actual value

m2_missing_prep <- obesity_vars_long %>%
  filter(is.na(value)) %>%
  dplyr::select(variable_new)

m2_missing <- unique(m2_missing_prep$variable_new)

m2_fitted_models <- obesity_vars_long %>%
  filter(variable_new %in% m1_ind_list_plot & variable_new %notin% m2_missing) %>%
  group_by(variable_new, utla17cd) %>%
  do(model = lm(value ~ year_num, data = .)) %>%
  mutate(a = summary(model)$coefficients[1]
         , b = summary(model)$coefficients[2]) %>%
  dplyr::select(-model)

m2_obesity_vars_long_avg <- obesity_vars_long %>%
  left_join(m2_fitted_models
            , by = c("variable_new", "utla17cd")) %>%
  mutate(fit = a + (b * year_num)
         , diff = value - fit) 

m2_obesity_vars_avg_diff <- m2_obesity_vars_long_avg %>%
  group_by(variable_new, utla17cd) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE)
            , avg_diff = mean(diff, na.rm = TRUE)
            , sd_diff = sd(diff, na.rm = TRUE)) %>%
  mutate(abs_total_diff = abs(total_diff)
         , abs_avg_diff = abs(avg_diff)) %>%
  filter(!is.na(sd_diff))
#if sd is NA then not enough time points so excluded

m2_variability_limits <- m2_obesity_vars_avg_diff %>%
  group_by(variable_new) %>%
  summarise(q25 = quantile(sd_diff, 0.25, na.rm = TRUE)
            , q75 = quantile(sd_diff, 0.75, na.rm = TRUE)
            , iqr = q75-q25
            , low1.5 = q25 - (1.5*iqr)
            , low3 = q25 - (3*iqr)
            , high1.5 = q75 + (1.5*iqr)
            , high3 = q75 + (3*iqr)
  )

npages_dist_var_m2 <- n_pages(ggplot(m2_obesity_vars_avg_diff, aes(x = reorder_within(utla17cd,sd_diff,variable_new), y = sd_diff)) +
                                geom_point(size = 0.2) +
                                geom_hline(data = m2_variability_limits, mapping = aes(yintercept = high1.5), colour = NT_colour("bright blue")) +
                                geom_hline(data = m2_variability_limits, mapping = aes(yintercept = high3), colour = NT_colour("bright red")) +
                                geom_hline(data = m2_variability_limits, mapping = aes(yintercept = low1.5), colour = NT_colour("bright blue")) +
                                geom_hline(data = m2_variability_limits, mapping = aes(yintercept = low3), colour = NT_colour("bright red")) +
                                facet_wrap_paginate(~ variable_new, scales = "free", nrow = 3, ncol = 2))

pdf("output/descriptives_independent/plots/Variability_m2_sd_diff_vars_obesity_rcp_utla.pdf")

for(i in 1:npages_dist_var_m2){
  
  print(
    ggplot(m2_obesity_vars_avg_diff, aes(x = reorder_within(utla17cd,sd_diff,variable_new), y = sd_diff)) +
      geom_point(size = 0.2, colour = NT_colour("NT ink")) +
      geom_hline(data = m2_variability_limits, mapping = aes(yintercept = high1.5), colour = NT_colour("bright blue")) +
      geom_hline(data = m2_variability_limits, mapping = aes(yintercept = high3), colour = NT_colour("bright red")) +
      geom_hline(data = m2_variability_limits, mapping = aes(yintercept = low1.5), colour = NT_colour("bright blue")) +
      geom_hline(data = m2_variability_limits, mapping = aes(yintercept = low3), colour = NT_colour("bright red")) +
      facet_wrap_paginate(~ variable_new, scales = "free", nrow = 3, ncol = 2, page = i) +
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
  )
  
}

dev.off()

npages_box_var_m2 <- n_pages(ggplot(m2_obesity_vars_avg_diff, aes(y = sd_diff)) +
                               stat_boxplot(geom = "errorbar", width = 0.2) +
                               geom_boxplot(outlier.colour = NT_colour("bright red"), outlier.shape = 8) +
                               facet_wrap_paginate(~ variable_new, scales = "free", nrow = 3, ncol = 2))

pdf("output/descriptives_independent/plots/Variability_m2_box_vars_obesity_rcp_utla.pdf")

for(i in 1:npages_box_var_m2){
  
  print(ggplot(m2_obesity_vars_avg_diff, aes(y = sd_diff)) +
          stat_boxplot(geom = "errorbar", width = 0.2, colour = NT_colour("NT ink")) +
          geom_boxplot(outlier.colour = NT_colour("bright red"), outlier.shape = 8) +
          facet_wrap_paginate(~ variable_new, scales = "free", nrow = 3, ncol = 2, page = i) +
          labs(y = "Standard deviation of difference"
               , title = "Boxplot of standard deviation of difference to yearly fitted values"
               , caption = "Red outliers = greater than 1.5 x IQR") +
          NT_style() +
          theme(panel.spacing = unit(1,"lines")
                , axis.ticks.x = element_blank()
                , axis.text.x = element_blank()
                , axis.title.x = element_blank()
          )
  )
  
}

dev.off()

m2_variability_outliers_summary <- m2_obesity_vars_avg_diff %>%
  left_join(m2_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(variable_new) %>%
  summarise(outliers_1.5 = sum(outlier_1.5)
            , outliers_3 = sum(outlier_3)) 

write_csv(m2_variability_outliers_summary, "output/descriptives_independent/Variability_outliers_m2_vars_obesity_rcp_utla.csv")

m2_variability_la_outliers_summary <- m2_obesity_vars_avg_diff %>%
  left_join(m2_variability_limits, by = "variable_new") %>%
  mutate(outlier_1.5 = case_when(sd_diff > high1.5 ~ 1
                                 , TRUE ~ 0)
         , outlier_3 = case_when(sd_diff > high3 ~ 1
                                 , TRUE ~ 0)) %>%
  filter(outlier_1.5 == 1) %>%
  dplyr::select(variable_new, utla17cd, outlier_1.5, outlier_3)

write_csv(m2_variability_la_outliers_summary, "output/descriptives_independent/Variability_outliers_m2_la_vars_obesity_rcp_utla.csv")

m2_outliers_la_plot <- m2_obesity_vars_long_avg %>%
  left_join(m2_variability_outliers_summary, by = "variable_new") %>%
  inner_join(m2_variability_la_outliers_summary, by = c("utla17cd", "variable_new"))

m2_ind_list_plot <- unique(m2_outliers_la_plot$variable_new)

pdf("output/descriptives_independent/plots/Variability_outliers_m2_la_vars_obesity_rcp_utla.pdf")

for(i in m2_ind_list_plot){
  
  print(ggplot(m2_outliers_la_plot %>%
                 filter(variable_new == i)) +
          geom_point(aes(x = factor(year_num), y = value, group = utla17cd, colour = utla17cd), size = 1) +
          geom_line(aes(x = factor(year_num), y = value, group = utla17cd, colour = utla17cd)) +
          labs(x = "Year"
               , title = i) +
          scale_colour_manual("Local authority"
                              , values = (colorRampPalette(RColorBrewer::brewer.pal(12,"Paired"))(as_vector(m2_outliers_la_plot %>%
                                                                                                              filter(variable_new == i) %>%
                                                                                                              summarise(n = n_distinct(utla17cd)))))) +
          
          NT_style() +
          theme(panel.spacing = unit(1, "lines"))
  )
  
}

dev.off()

########################################################################################################################

############# Distribution of independent variables #############

npages_norm_var <- n_pages(ggplot(obesity_vars_long %>%
                                    left_join(obesity_desc %>%
                                                dplyr::select(variable_new, year_char, mean, skew)
                                              , by = c("variable_new", "year_char"))
                                  , aes(x = value)) +
                             geom_density(fill = NT_colour("light purple 1")) +
                             geom_vline(aes(xintercept = mean), color = NT_colour("NT ink"), linetype = "dotted", size = 0.5) +
                             geom_text(obesity_desc 
                                       , mapping=aes(x=Inf, y=Inf, label = paste("skewness: ",round(skew,2)))
                                       , hjust = "inward", vjust = "inward"
                                       , size = 3, colour = NT_colour("NT ink")) +
                             facet_wrap_paginate( ~ variable, scales = "free_x", ncol = 3, nrow = 5))

pdf("output/descriptives_independent/plots/Normality_vars_obesity_rcp_utla.pdf")

for(i in 1:npages_norm_var){
  
  print(ggplot(obesity_vars_long %>%
                 left_join(obesity_desc %>%
                             dplyr::select(variable_new, year_char, mean, skew)
                           , by = c("variable_new", "year_char"))
               , aes(x = value)) +
          geom_density(fill = NT_colour("light purple 1")) +
          geom_vline(aes(xintercept = mean), color = NT_colour("NT ink"), linetype = "dotted", size = 0.5) +
          geom_text(obesity_desc
                    , mapping=aes(x=Inf, y=Inf, label = paste("skew: ", round(skew,2)))
                    , hjust = "inward", vjust = "inward"
                    , size = 3, colour = NT_colour("NT ink")) +
          facet_wrap_paginate( ~ variable, scales = "free", ncol = 3, nrow = 5, page = i) +
          labs(y = "Frequency"
               , title = "Density Plot") +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          NT_style() +
          theme(panel.spacing = unit(0.5,"lines"))
  )
  
}

dev.off()

########################################################################################################################

############# Correlation with other independent variables #############

excel_sheets <- list()

for(i in colnames(obesity_vars %>%
                  dplyr::select(-"utla17cd"))){
  
  pearson_correlation <- plyr::ddply(obesity_vars_long %>% 
                                       filter(variable != i) %>% 
                                       left_join(obesity_vars_long %>% 
                                                   filter(variable == i) %>%
                                                   dplyr::select(utla17cd, value) %>%
                                                   rename(i = value)
                                                 , by = "utla17cd"), plyr::.(variable), summarise
                                     , "pearson_correlation"= cor.test(i, value, method = "pearson", use = "na.or.complete")$estimate
                                     , "p_value"= cor.test(i, value, method = "pearson", use = "na.or.complete")$p.value)
  
  
  pearson_correlation_out <- pearson_correlation 
  
  #Rename file with varname
  excel_sheets[[i]] <- assign(paste0(i),data.frame(pearson_correlation_out))
  
}

write_xlsx(excel_sheets,"output/descriptives_independent/Correlation_all_vars_obesity_rcp_UTLA.xlsx", col_names = TRUE)

########################################################################################################################
