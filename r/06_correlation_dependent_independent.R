########################################################################################################################

############# Check the relationship between dependent and independent variable #############

# Linear relationship: Scatter plots with loess curve
# Pearson's correlation coefficient

########################################################################################################################

############# Create long file of variables with chosen outcome #############

# 2018/19 overweight and obese

obesity_outcome_vars_long <- obesity_vars_long %>%
  left_join(obesity_outcome_long %>%
              filter(variable_new == "rcp_ovw_ob_" & year_num == 2018) %>%
              dplyr::select(utla17cd, value) %>%
              dplyr::rename(rcp_ovw_ob_1819 = value)
            , by = "utla17cd")

########################################################################################################################

############# Linear relationship #############

# Scatter plots with loess curve

npages_corr <- n_pages(ggplot(obesity_outcome_vars_long, aes(y = rcp_ovw_ob_1819, x = value)) +
                         geom_point() +
                         geom_smooth(method = "loess", formula = y ~ x) +
                         facet_wrap_paginate(~ variable, scales = "free", nrow = 3, ncol = 2))

pdf("output/correlation_dependent_independent/plots/Correlation_vars_obesity_rcp_utla.pdf")

for(i in 1:npages_corr){
  
  print(ggplot(obesity_outcome_vars_long, aes(y = rcp_ovw_ob_1819, x = value)) +
          geom_point(colour = NT_colour("NT ink")) +
          geom_smooth(method = "loess", formula = y ~ x) +
          facet_wrap_paginate(~ variable, scales = "free", nrow = 3, ncol =2, page = i) +
          labs(title = "Correlation plot with locally estimated smoothing line"
               , x = "Prevalence (%)") +
          NT_style() + 
          theme(panel.spacing = unit(1,"lines"))
        
  )
  
}

dev.off()

########################################################################################################################

############# Pearson's correlation coefficient #############

pearson_correlation <- plyr::ddply(obesity_outcome_vars_long, plyr::.(variable), summarise
                                   , "pearson_correlation"= cor.test(rcp_ovw_ob_1819, value, method = "pearson", use = "na.or.complete")$estimate
                                   , "p_value"= cor.test(rcp_ovw_ob_1819, value, method = "pearson", use = "na.or.complete")$p.value)

write_csv(pearson_correlation %>%
            mutate(absolute_pearson = abs(pearson_correlation)), "output/correlation_dependent_independent/Pearson_correlation_vars_obesity_rcp_utla.csv")

########################################################################################################################
