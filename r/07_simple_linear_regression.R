########################################################################################################################

############# Simple linear regression #############

# Models of dependent variable with each independent variable individually

# Build simple linear models
# Coefficients and model fit file
# Linear model tests
# Linear model assumption plots
# Prediction and confidence interval plots
# Models with influential points excluded

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

############# Build simple linear models #############

# Each variable modelled against outcome individually

simple_models <- tibble(variable = unique(obesity_outcome_vars_long$variable)) %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(rcp_ovw_ob_1819 ~ value
                                              , dset))
  )

########################################################################################################################

############# Coefficients and model fit file #############

simple_models_out <- simple_models %>% 
  mutate(
    observations       = map_int(model, nobs)
    , intercept_coeff  = map_chr(model, function(x) tidy(x)$estimate[1])
    , intercept_se     = map_chr(model, function(x) tidy(x)$std.error[1])
    , intercept_lcl    = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[1])
    , intercept_ucl    = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[1])
    , intercept_pvalue = map_chr(model, function(x) tidy(x)$p.value[1])
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_se           = map_chr(model, function(x) tidy(x)$std.error[2])
    , var_lcl          = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[2])
    , var_ucl          = map_chr(model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , se               = map_chr(model, function(x) glance(x)$sigma)
    , fstat            = map_chr(model, function(x) glance(x)$statistic)
    , fstat_pvalue     = map_chr(model, function(x) glance(x)$p.value)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
    , standard_coeff   = map_chr(model, function(x) lm.beta::lm.beta(x)$standardized.coefficients[2])) %>%
  dplyr::select(-model, -dataset)

write_csv(simple_models_out, "output/simple_linear_regression/Linear_coeff_fit_obesity_rcp_utla.csv")

########################################################################################################################

############# Linear model tests #############

# Linear relationship - reject the null hypothesis that relationship not linear with p >= 0.05 for shapiro
# Linear relationship - check mean of residuals is zero or close to zero
# No autocorrelation - durbin watson statistics between 1.5 and 2.5, p >= 0.05
# No heteroscedasticity - breusch-pagan p >= 0.05

linear_model_tests <- simple_models %>% 
  mutate(
    linear_sp_stat           = map_chr(model, function(x) shapiro.test(x$residuals)$statistic)
    , linear_sp_pvalue       = map_chr(model, function(x) shapiro.test(x$residuals)$p.value)
    , linear_mean_resid      = map_chr(model, function(x) mean(x$residuals))
    , autocorr_dw_stat       = map_chr(model, function(x) lmtest::dwtest(x)$statistic)
    , autocorr_dw_pvalue     = map_chr(model, function(x) lmtest::dwtest(x)$p.value)
    , homosced_bp_stat       = map_chr(model, function(x) lmtest::bptest(x)$statistic)
    , homosced_bp_pvalue     = map_chr(model, function(x) lmtest::bptest(x)$p.value)
    , linear_check           = case_when(linear_sp_pvalue >= 0.05 ~ 1
                                         , TRUE ~ 0)
    , autocorrelation_check  = case_when(autocorr_dw_pvalue >= 0.05 ~ 1
                                         , TRUE ~ 0)
    , homoscedasticity_check = case_when(homosced_bp_pvalue >= 0.05 ~ 1
                                         , TRUE ~ 0)
    , checks_passed          = case_when(linear_check == 1 & autocorrelation_check ==  1 & homoscedasticity_check == 1 ~ 1
                                         , TRUE ~ 0)
    , checks_passed_excl_ac  = case_when(linear_check == 1 & homoscedasticity_check == 1 ~ 1
                                         , TRUE ~ 0)
  ) %>%
  dplyr::select(-model, -dataset)

write_csv(linear_model_tests, "output/simple_linear_regression/Linear_tests_obesity_rcp_utla.csv")

########################################################################################################################

############# Linear model assumption plots #############

# Residuals vs fitted - want to be randomly distributed around centre of zero, line should be as straight as possible
# Quantile plot - want residuals to hug the line
# Cook's distance - don't want any data point to have too much influence
# Autocorrelation - points after lag 1 to be within confidence bounds
# Histogram - residuals normally distributed

linear_model_plots <- simple_models %>%
  mutate(
    residuals = map(model, function(x) augment(x))
    , res_v_fit  = map2(model, variable, function(dset, var_lab) ggplot(dset, aes(x = .fitted, y = .resid)) +
                          geom_point(colour = NT_colour("NT ink")) + 
                          stat_smooth(method = "loess") + 
                          geom_hline(yintercept = 0, colour = NT_colour("bright red"), lty = "dashed") +
                          labs(title = var_lab, x = "Fitted values",  y = "Residuals") +
                          NT_style())
    , quantile   = map2(model, variable, function(dset, var_lab) ggplot(dset, aes(sample = .stdresid)) +
                          stat_qq(colour = NT_colour("NT ink")) + 
                          stat_qq_line(colour = NT_colour("NT ink")) + 
                          labs(x = "Theoretical quantiles", y = "Standardised residuals"
                               , title = var_lab) +
                          NT_style())
    , cooks_dist = map2(model, variable, function(dset, var_lab) ggplot(dset, aes(x = seq_along(.cooksd), y = .cooksd)) +
                          geom_bar(stat = "identity", position = "identity", fill = NT_colour("bright purple")) + 
                          labs(x = "Observation number",y = "Cook's distance", title = var_lab) +
                          NT_style())
    , acf        = map2(model, variable, function(dset, var_lab) ggplot(with(acf(dset$residuals, plot = FALSE), data.frame(lag, acf))) +
                          geom_hline(aes(yintercept = 0), colour = NT_colour("NT ink")) +
                          geom_hline(aes(yintercept = qnorm((1-0.95)/2)/sqrt(length(dset$residuals))), colour = NT_colour("bright blue"), lty = "dashed") +
                          geom_hline(aes(yintercept = -(qnorm((1-0.95)/2)/sqrt(length(dset$residuals)))), colour = NT_colour("bright blue"), lty = "dashed") +
                          geom_segment(aes(x = lag, y = acf, xend = lag, yend = 0), colour = NT_colour("NT ink")) +
                          labs(title = var_lab, y = "ACF", x = "Lag") +
                          NT_style())
    , hist_resid = map2(residuals, variable, function(dset, var_lab) ggplot(dset, aes(x = .resid)) +
                          geom_histogram(aes(y = ..density..), bins = 10, fill = NT_colour("light purple 1")) +
                          stat_function(fun = dnorm, args = list(mean = mean(dset$.resid), sd = sd(dset$.resid)), colour = NT_colour("bright blue")) +
                          labs(y = "Frequency", x = "Residuals", title = var_lab) +
                          NT_style())
  ) %>%
  dplyr::select(-model, -residuals, -dataset)

res_v_fit <- ggarrange(plotlist = linear_model_plots$res_v_fit, nrow = 3, ncol = 2)
ggexport(res_v_fit, filename = "output/simple_linear_regression/plots/Linear_res_v_fit_obesity_rcp_utla.pdf")

normqq <- ggarrange(plotlist = linear_model_plots$quantile, nrow = 3, ncol = 2)
ggexport(normqq, filename = "output/simple_linear_regression/plots/Linear_normqq_obesity_rcp_utla.pdf")

cooks_dist <- ggarrange(plotlist = linear_model_plots$cooks_dist, nrow = 3, ncol = 2)
ggexport(cooks_dist, filename = "output/simple_linear_regression/plots/Linear_cooks_dist_obesity_rcp_utla.pdf")

acf <- ggarrange(plotlist = linear_model_plots$acf, nrow = 3, ncol = 2)
ggexport(acf, filename = "output/simple_linear_regression/plots/Linear_acf_obesity_rcp_utla.pdf")

hist_resid <- ggarrange(plotlist = linear_model_plots$hist_resid, nrow = 3, ncol = 2)
ggexport(hist_resid, filename = "output/simple_linear_regression/plots/Linear_hist_resid_obesity_rcp_utla.pdf")

########################################################################################################################

############# Prediction and confidence interval plots #############

simple_model_la_output <- obesity_outcome_vars_long %>%
  left_join(simple_models %>%
              dplyr::select(variable, model)
            , by = "variable") %>%
  group_by(variable) %>%
  do(ciTools::add_pi(., first(.$model), name = c("lwr", "upr"))) %>%
  do(ciTools::add_ci(., first(.$model), name = c("lcl", "ucl"))) %>%
  dplyr::select(-model)

npages_int <- n_pages(ggplot(simple_model_la_output, aes(x=rcp_ovw_ob_1819, y=value)) +
                        geom_point(size=0.2) +
                        facet_wrap_paginate(~ variable, scales="free_y", ncol=2, nrow=3))

pdf("output/simple_linear_regression/plots/Linear_model_obesity_rcp_utla.pdf")

for(i in 1:npages_int){
  
  print(
    ggplot(simple_model_la_output, aes(y=rcp_ovw_ob_1819, x=value)) +
      geom_point(size = 0.2, colour = NT_colour("NT ink")) +
      facet_wrap_paginate(~ variable, scales = "free_x", ncol = 2, nrow = 3, page = i) +
      geom_line(aes(y = pred), size = 0.2, colour = NT_colour("NT ink")) +
      geom_ribbon(aes(ymin = lwr, ymax = upr), fill = NT_colour("bright yellow"), alpha = 0.3) +
      geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = NT_colour("bright blue"), alpha = 0.3) +
      labs( y = "Prevalence (%)", x = "Variable value") +
      NT_style() +
      theme(panel.spacing = unit(1,"lines"))
  )
  
}

dev.off()

########################################################################################################################

############# Models with influential points excluded - outliers #############

# Mean cook's distance for each model
# Points that are greater than 4 * mean cooks distance
# Exclude these and re run model
# Produce same outputs as above

outlier_models <- simple_models %>% 
  mutate(
    residuals   = map(model, function(x) augment(x) %>% 
                        mutate(mean_cook_threshold = 4*mean(.cooksd)
                               , observation = row_number()) %>%
                        filter(.cooksd < mean_cook_threshold))
    , new_data  = map2(dataset, residuals, function(dset, resids) dset %>% 
                         inner_join(resids %>% 
                                      dplyr::select(observation)
                                    , by = "observation"))
    , new_model  = map(new_data, function(dset) lm(rcp_ovw_ob_1819 ~ value
                                                   , dset))
  ) %>%
  dplyr::select(-dataset, -model, -residuals)


outlier_models_out <- outlier_models %>% 
  mutate(
    observations       = map_int(new_model, nobs)
    , intercept_coeff  = map_chr(new_model, function(x) tidy(x)$estimate[1])
    , intercept_se     = map_chr(new_model, function(x) tidy(x)$std.error[1])
    , intercept_lcl    = map_chr(new_model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[1])
    , intercept_ucl    = map_chr(new_model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[1])
    , intercept_pvalue = map_chr(new_model, function(x) tidy(x)$p.value[1])
    , var_coeff        = map_chr(new_model, function(x) tidy(x)$estimate[2])
    , var_se           = map_chr(new_model, function(x) tidy(x)$std.error[2])
    , var_lcl          = map_chr(new_model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.low[2])
    , var_ucl          = map_chr(new_model, function(x) tidy(x, conf.int = TRUE, conf.level = 0.95)$conf.high[2])
    , var_pvalue       = map_chr(new_model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(new_model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(new_model, function(x) glance(x)$adj.r.squared)
    , se               = map_chr(new_model, function(x) glance(x)$sigma)
    , fstat            = map_chr(new_model, function(x) glance(x)$statistic)
    , fstat_pvalue     = map_chr(new_model, function(x) glance(x)$p.value)
    , aic              = map_chr(new_model, function(x) glance(x)$AIC)
    , bic              = map_chr(new_model, function(x) glance(x)$BIC)) %>%
  dplyr::select(-new_data, -new_model)

write_csv(outlier_models_out, "output/simple_linear_regression/Linear_coeff_fit_outlier_obesity_rcp_utla.csv")

########################################################################################################################

############# Outlier model tests #############

# Linear relationship - reject the null hypothesis that relationship not linear with p >= 0.05 for shapiro
# Linear relationship - check mean of residuals is zero or close to zero
# No autocorrelation - durbin watson statistics between 1.5 and 2.5, p >= 0.05
# No heteroscedasticity - breusch-pagan p >= 0.05

outlier_model_tests <- outlier_models %>% 
  mutate(
    linear_sp_stat           = map_chr(new_model, function(x) shapiro.test(x$residuals)$statistic)
    , linear_sp_pvalue       = map_chr(new_model, function(x) shapiro.test(x$residuals)$p.value)
    , linear_mean_resid      = map_chr(new_model, function(x) mean(x$residuals))
    , autocorr_dw_stat       = map_chr(new_model, function(x) lmtest::dwtest(x)$statistic)
    , autocorr_dw_pvalue     = map_chr(new_model, function(x) lmtest::dwtest(x)$p.value)
    , homosced_bp_stat       = map_chr(new_model, function(x) lmtest::bptest(x)$statistic)
    , homosced_bp_pvalue     = map_chr(new_model, function(x) lmtest::bptest(x)$p.value)
    , linear_check           = case_when(linear_sp_pvalue >= 0.05 ~ 1
                                         , TRUE ~ 0)
    , autocorrelation_check  = case_when(autocorr_dw_pvalue >= 0.05 ~ 1
                                         , TRUE ~ 0)
    , homoscedasticity_check = case_when(homosced_bp_pvalue >= 0.05 ~ 1
                                         , TRUE ~ 0)
    , checks_passed          = case_when(linear_check == 1 & autocorrelation_check ==  1 & homoscedasticity_check == 1 ~ 1
                                         , TRUE ~ 0)
    , checks_passed_excl_ac  = case_when(linear_check == 1 & homoscedasticity_check == 1 ~ 1
                                         , TRUE ~ 0)
  ) %>%
  dplyr::select(-new_model, -new_data)

write_csv(outlier_model_tests, "output/simple_linear_regression/Linear_tests_outlier_obesity_rcp_utla.csv")

########################################################################################################################
