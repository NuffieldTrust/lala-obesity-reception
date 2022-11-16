########################################################################################################################

############# Blocks multiple linear regression #############

# Use literature themes to put variables in blocks
# Use stepwise regression, multicollinearity and knowledge of variables to refine block list
# Create a socioeconomic/demographic block which can be used for general adjustment
# Create a full multiple model with each theme and the socioeconomic/demographic block

# !!! Filtered out variables relating to the u11/u12 population
# !!! Included socioeconomic/demographic variables under existing outcome themes if more relevant there

# !!! Disadvantage block used a HES variable which is not included in the github files so results will differ

########################################################################################################################

############# Create long and wide file of variables with chosen outcome #############

# 2018/19 overweight and obese

# Wide

obesity_outcome_vars <- obesity_vars_unsupp %>%
  left_join(obesity_outcome_long %>%
              filter(variable_new == "rcp_ovw_ob_" & year_num == 2018) %>%
              dplyr::select(utla17cd, value) %>%
              dplyr::rename(rcp_ovw_ob_1819 = value)
            , by = "utla17cd")

# Long

obesity_outcome_vars_long <- obesity_vars_long %>%
  left_join(obesity_outcome_long %>%
              filter(variable_new == "rcp_ovw_ob_" & year_num == 2018) %>%
              dplyr::select(utla17cd, value) %>%
              dplyr::rename(rcp_ovw_ob_1819 = value)
            , by = "utla17cd")

########################################################################################################################

############# Import variable themes lookup #############

variable_themes <- read_csv("data/Variable_themes.csv")

########################################################################################################################

############# Obesogenic community environment #############

# Identify list of variables based on association to include in initial block
# Select from multiple time points

environment <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "obesogenic community environment" 
                                            | (theme == "socioeconomic" 
                                               & suggested == "obesogenic community environment")))$variable)
         & !str_detect(variable_new, "u11")) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

environment_simple_models <- environment %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(rcp_ovw_ob_1819 ~ value
                                              , dset))
    , observations     = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1)

#################################################################

# Create first multiple model and then use stepwise to refine
# Include one of each variables from the most associated list created in previous step
# Only keep one of any variables which you know to overlap e.g. prop_imd_15 and av_imd_15
# If the most associated list doesn't include an appropriate time point, include the most appropriate time point instead and drop the inappropriate

environment_model <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                                     , active_new_se_1617
                                                                                                     , air_fine_phe_16
                                                                                                     , prop_u5_rd_school_imd_15
                                                                                                     , prop_u5_rd_store_imd_15
                                                                                                     , ffood_u5_phe_17
                                                                                                     , prop_cyp_inactive_se_mean_1718
                                                                                                     , prop_rural_defra_11
                                                                                                     , sp_chpa_mhclg_pl_1516_1819
                                                                                                     , sp_chob_mhclg_pl_1617_1819
                                                                                                     , sp_osp_mhclg_pl_1415_1819
                                                                                                     , sp_sprec_mhclg_pl_1415_1819
                                                                                                     , sp_sprecfac_mhclg_pl_1617_1819
                                                                                                     , walk_leis_new_se_1718
                                                                                                     , woodland_phe_15
                                                                                                     , greenact_u5_ahah2_17
                                                                                                     , greenpas_u5_ahah2_17
                                                                                                     , blue_u5_ahah2_17
                                                                                                     , leis_u5_ahah2_17
))

stepwise_environment_model <- MASS::stepAIC(environment_model, trace = FALSE, direction = "both")

# Check for multicollinearity with variance inflation factor and pearson correlation

vif_stepwise_environment_model <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% dplyr::select(attr(terms(stepwise_environment_model), "term.labels"))
                                                            , obesity_outcome_vars$rcp_ovw_ob_1819)$idiags
                                            , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_environment_model <- as_tibble(cor(obesity_outcome_vars %>% dplyr::select(attr(terms(stepwise_environment_model), "term.labels")))
                                                , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_environment_model[pearson_stepwise_environment_model > -0.8 & pearson_stepwise_environment_model < 0.8] <- NA

summary(stepwise_environment_model)

# Save block with standardised coefficients

environment_block <- tidy(stepwise_environment_model) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_environment_model)$standardized.coefficients))

########################################################################################################################

############# Socioeconomic disadvantage #############

disadvantage <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "socioeconomic disadvantage"))$variable)
         & !str_detect(variable_new, "u11")
         & !str_detect(variable_new, "u12")) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

disadvantage_simple_models <- disadvantage %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(rcp_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff       = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

# Disadvantage block uses a HES variable which is not included in the github files

# disadvantage_model <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
#                                                                                                       , cariesadmiss_u6_hes_sens_1518))

# Removed visible tooth decay 2014/15 as is a measure of under 5's tooth decay so mostly won't cover this set of children 
# We have the caries admissions indicator as alternative

# summary(disadvantage_model)

# disadvantage_block <- tidy(disadvantage_model) %>%
#   bind_cols(standardised_coefficients = c(lm.beta::lm.beta(disadvantage_model)$standardized.coefficients))

########################################################################################################################

############# Childhood stress #############

stress <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "childhood stress"))$variable)
         & !str_detect(variable_new, "u11")) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

stress_simple_models <- stress %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(rcp_ovw_ob_1819 ~ value
                                              , dset))
    , observations     = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

stress_model <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd, rate_cin_dfe_1617))

summary(stress_model)

stress_block <- tidy(stress_model) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stress_model)$standardized.coefficients))

########################################################################################################################

############# Breastfeeding #############

bf <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "breastfeeding"))$variable)
         & !str_detect(variable_new, "u11")) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

bf_simple_models <- bf %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(rcp_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

bf_model <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                            , unique(bf_simple_models$variable)
                                                                                            , -bf_new_imp_1516_1819
))

summary(bf_model)

bf_block <- tidy(bf_model) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(bf_model)$standardized.coefficients))

########################################################################################################################

############# Neighbourhood safety #############

safety <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "neighbourhood safety"
                                            | (theme == "socioeconomic" & suggested == "neighbourhood safety")))$variable)
         & !str_detect(variable_new, "u11")) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

safety_simple_models <- safety %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(rcp_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1) 

safety_model <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                                , crime_ons_18
                                                                                                , prop_accidents_imd_15))

stepwise_safety_model <- MASS::stepAIC(safety_model, trace = FALSE, direction = "both")

vif_stepwise_safety_model <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% dplyr::select(attr(terms(stepwise_safety_model), "term.labels"))
                                                       , obesity_outcome_vars$rcp_ovw_ob_1819)$idiags
                                       , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_safety_model <- as_tibble(cor(obesity_outcome_vars %>% dplyr::select(attr(terms(stepwise_safety_model), "term.labels")))
                                           , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_safety_model[pearson_stepwise_safety_model > -0.8 & pearson_stepwise_safety_model < 0.8] <- NA

summary(stepwise_safety_model)

safety_block <- tidy(stepwise_safety_model) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_safety_model)$standardized.coefficients))

########################################################################################################################

############# Socioeconomic/demographic #############

# Exclude any of the socioeconomic/demographic variables that have now been absorbed into the existing outcome variable themes
# Create a block to adjust full models with
# Create a refined block using stepwise regression and checking for multicollinearity

socioeconomic <- obesity_outcome_vars_long %>% 
  filter(variable_new %in% unique((variable_themes %>% 
                                     filter(theme == "socioeconomic" 
                                            & (suggested != "obesogenic community environment" 
                                               & suggested != "neighbourhood safety")))$variable)
         & !str_detect(variable_new, "u11")) %>%
  dplyr::select(variable, variable_new) %>%
  distinct(variable, variable_new)

socioeconomic_simple_models <- socioeconomic %>% 
  mutate(
    dataset  = map(variable, function(var) obesity_outcome_vars_long %>% 
                     filter(variable == var) %>% 
                     mutate(observation = row_number()))
    , model  = map(dataset, function(dset) lm(rcp_ovw_ob_1819 ~ value
                                              , dset))
    , observations       = map_int(model, nobs)
    , var_coeff        = map_chr(model, function(x) tidy(x)$estimate[2])
    , var_pvalue       = map_chr(model, function(x) tidy(x)$p.value[2])
    , rsquared         = map_chr(model, function(x) glance(x)$r.squared)
    , adj_rsquared     = map_chr(model, function(x) glance(x)$adj.r.squared)
    , aic              = map_chr(model, function(x) glance(x)$AIC)
    , bic              = map_chr(model, function(x) glance(x)$BIC)
  ) %>%
  dplyr::select(-model, -dataset) %>%
  filter(observations == 150) %>%
  group_by(variable_new) %>%
  top_n(aic, n = -1)

#################################################################

# Create a multiple model
# Use the suggested variables attached to the themes to ensure only keeping one type of each type of socioeconomic/demographic variable
# If the year in the simple models output doesn't look appropriate then switch to appropriate year
# This block can be used to adjust other models although multicollinearity will need checking

socioeconomic_model <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                                       , prop_u5_owb_dwp_16
                                                                                                       , prop_u5_hse_afford_imd_15
                                                                                                       , av_hea_imd_15
                                                                                                       , av_adult_skills_imd_15
                                                                                                       , prop_nonwhite_dfe_18
                                                                                                       , sp_ey_esfa_1011
                                                                                                       , prop_u5_hse_crowd_imd_15
                                                                                                       , sp_chnpres_mhclg_pool_1516_1819
                                                                                                       , sp_chpres_mhclg_pl_1516_1819
                                                                                                       , dwelling_pp_voa_15
                                                                                                       , prop_u5_hse_cond_imd_15
                                                                                                       , av_edu_cyp_imd_15
                                                                                                       , prop_u5_hse_heat_imd_15))

# Use stepwise regression to refine the list of variables

stepwise_socioeconomic <- MASS::stepAIC(socioeconomic_model, trace = FALSE, direction = "both")

# Check for multicollinearity (both VIF > 5 and pearson > 0.8)

vif_stepwise_socioeconomic <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% dplyr::select(attr(terms(stepwise_socioeconomic), "term.labels"))
                                                        , obesity_outcome_vars$rcp_ovw_ob_1819)$idiags
                                        , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_socioeconomic <- as_tibble(cor(obesity_outcome_vars %>% dplyr::select(attr(terms(stepwise_socioeconomic), "term.labels")))
                                            , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_socioeconomic[pearson_stepwise_socioeconomic > -0.8 & pearson_stepwise_socioeconomic < 0.8] <- NA

summary(stepwise_socioeconomic)

# if multicollinearity exists then need to test model fit excluding each of the variables 
socioeconomic_model1a <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                                         , attr(terms(stepwise_socioeconomic), "term.labels")
                                                                                                         , -prop_u5_owb_dwp_16
                                                                                                         , -prop_nonwhite_dfe_18))

socioeconomic_model1b <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                                         , attr(terms(stepwise_socioeconomic), "term.labels")
                                                                                                         , -prop_u5_owb_dwp_16
                                                                                                         , -prop_u5_hse_afford_imd_15))

socioeconomic_model1c <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                                         , attr(terms(stepwise_socioeconomic), "term.labels")
                                                                                                         , -av_hea_imd_15
                                                                                                         , -prop_nonwhite_dfe_18))

socioeconomic_model1d <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                                         , attr(terms(stepwise_socioeconomic), "term.labels")
                                                                                                         , -av_hea_imd_15
                                                                                                         , -prop_u5_hse_afford_imd_15))

glance(socioeconomic_model1a)
glance(socioeconomic_model1b)
glance(socioeconomic_model1c)
glance(socioeconomic_model1d)

summary(socioeconomic_model1b)
summary(socioeconomic_model1d)

# Pick best model based on adjusted rsquared and AIC and further refine if multicollinearity remains
# Chosen to keep the model including u5_owb despite marginally worse fit as much more important in simple models

vif_socioeconomic_model1d <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% dplyr::select(attr(terms(socioeconomic_model1d), "term.labels"))
                                                       , obesity_outcome_vars$rcp_ovw_ob_1819)$idiags
                                       , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_socioeconomic_model1d <- as_tibble(cor(obesity_outcome_vars %>% dplyr::select(attr(terms(socioeconomic_model1d), "term.labels")))
                                           , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_socioeconomic_model1d[pearson_socioeconomic_model1d > -0.8 & pearson_socioeconomic_model1d < 0.8] <- NA

glance(socioeconomic_model1d)
summary(socioeconomic_model1d)

# There are some extra variables in here that are non-sig but may be important when combined with variables from themes

socioeconomic_block <- tidy(socioeconomic_model1d) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(socioeconomic_model1d)$standardized.coefficients))

##################################################################################################################################

############# Block adjusted r squareds #############

blocks_adj_rsq <- tibble(block = c("environment"
                                   # , "disadvantage"
                                   , "breastfeeding", "stress", "safety", "socioeconomic")
                         , adj_rsq = c(glance(stepwise_environment_model)$adj.r.squared
                                       # , glance(disadvantage_model)$adj.r.squared
                                       , glance(bf_model)$adj.r.squared
                                       , glance(stress_model)$adj.r.squared
                                       , glance(stepwise_safety_model)$adj.r.squared
                                       , glance(socioeconomic_model1d)$adj.r.squared))

########################################################################################################################

############# Models with all themes included #############

# Adjust final blocks for socioeconomic/demographics using the refined socioeconomic block 

model_all3 <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                       , attr(terms(stepwise_environment_model), "term.labels")
                                                                                       # , attr(terms(disadvantage_model), "term.labels")
                                                                                       , attr(terms(bf_model), "term.labels")
                                                                                       , attr(terms(stress_model), "term.labels")
                                                                                       , attr(terms(stepwise_safety_model), "term.labels")
                                                                                       , attr(terms(socioeconomic_model1d), "term.labels")
))

stepwise_model_all3 <- MASS::stepAIC(model_all3, trace = FALSE, direction = "both")

vif_stepwise_model_all3 <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% dplyr::select(attr(terms(stepwise_model_all3), "term.labels"))
                                                     , obesity_outcome_vars$rcp_ovw_ob_1819)$idiags
                                     , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_stepwise_model_all3 <- as_tibble(cor(obesity_outcome_vars %>% dplyr::select(attr(terms(stepwise_model_all3), "term.labels")))
                                         , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_stepwise_model_all3[pearson_stepwise_model_all3 > -0.8 & pearson_stepwise_model_all3 < 0.8] <- NA

summary(stepwise_model_all3)

final_model_all3 <- tidy(stepwise_model_all3) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr")) %>%
  bind_cols(standardised_coefficients = c(lm.beta::lm.beta(stepwise_model_all3)$standardized.coefficients))
final_fit_all3 <- glance(stepwise_model_all3)

############################

# Scaled/standardised coefficients

stdcoeff_model3 <- tidy(lm(scale(rcp_ovw_ob_1819) ~ scale(active_new_se_1617) + scale(ffood_u5_phe_17) + scale(prop_cyp_inactive_se_mean_1718) 
                           + scale(sp_chpa_mhclg_pl_1516_1819) + scale(sp_chob_mhclg_pl_1617_1819)
                           + scale(walk_leis_new_se_1718) + scale(greenact_u5_ahah2_17) + scale(greenpas_u5_ahah2_17) + scale(blue_u5_ahah2_17)
                           + scale(bf_imp_1112_1415) + scale(prop_u5_owb_dwp_16), obesity_outcome_vars))


############################

# Relative importance

relaimpo::calc.relimp(stepwise_model_all3, rela = TRUE)

# Manual method

glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ bf_imp_1112_1415, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ active_new_se_1617, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ ffood_u5_phe_17, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ sp_chpa_mhclg_pl_1516_1819, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ sp_chob_mhclg_pl_1617_1819, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ prop_cyp_inactive_se_mean_1718, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ walk_leis_new_se_1718, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ greenact_u5_ahah2_17, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ blue_u5_ahah2_17, obesity_outcome_vars))$r.squared
glance(lm(rcp_ovw_ob_1819 ~ greenpas_u5_ahah2_17, obesity_outcome_vars))$r.squared

final_relimp_all3 <- tibble(prop_u5_owb_dwp_16 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16, obesity_outcome_vars))$r.squared
                            , bf_imp_1112_1415 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16, obesity_outcome_vars))$r.squared
                            , active_new_se_1617 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415, obesity_outcome_vars))$r.squared
                            , ffood_u5_phe_17= glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                                         + ffood_u5_phe_17, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617, obesity_outcome_vars))$r.squared
                            , sp_chpa_mhclg_pl_1516_1819 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                                                     + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                        + ffood_u5_phe_17, obesity_outcome_vars))$r.squared
                            , sp_chob_mhclg_pl_1617_1819 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                                                     + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                        + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819, obesity_outcome_vars))$r.squared
                            , prop_cyp_inactive_se_mean_1718 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                                                         + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                                                         + prop_cyp_inactive_se_mean_1718, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                        + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819, obesity_outcome_vars))$r.squared
                            , walk_leis_new_se_1718 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                                                + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                                                + walk_leis_new_se_1718, obesity_outcome_vars))$r.squared
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                        + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                        , obesity_outcome_vars))$r.squared
                            , greenact_u5_ahah2_17 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                                               + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                                               + walk_leis_new_se_1718
                                                               + greenact_u5_ahah2_17, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                        + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                        + walk_leis_new_se_1718, obesity_outcome_vars))$r.squared
                            , blue_u5_ahah2_17 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                                           + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                                           + walk_leis_new_se_1718
                                                           + greenact_u5_ahah2_17 + blue_u5_ahah2_17, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                        + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                        + walk_leis_new_se_1718 + greenact_u5_ahah2_17, obesity_outcome_vars))$r.squared
                            , greenpas_u5_ahah2_17 = glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                                               + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                                               + walk_leis_new_se_1718
                                                               + greenact_u5_ahah2_17 + blue_u5_ahah2_17 + greenpas_u5_ahah2_17, obesity_outcome_vars))$r.squared 
                            - glance(lm(rcp_ovw_ob_1819 ~ prop_u5_owb_dwp_16 + bf_imp_1112_1415 + active_new_se_1617 
                                        + ffood_u5_phe_17 + sp_chpa_mhclg_pl_1516_1819 + sp_chob_mhclg_pl_1617_1819 
                                        + walk_leis_new_se_1718 + greenact_u5_ahah2_17 + blue_u5_ahah2_17, obesity_outcome_vars))$r.squared) %>%
  pivot_longer(everything(), names_to = "model", values_to = "added_r_squared") %>%
  mutate(r_squared = sum(added_r_squared)
         , prop_r_squared = added_r_squared / r_squared)

############################

# Test model assumptions

final_all3_tests <- tibble(linear_sp_stat           = shapiro.test(stepwise_model_all3$residuals)$statistic
                           , linear_sp_pvalue       = shapiro.test(stepwise_model_all3$residuals)$p.value
                           , linear_mean_resid      = mean(stepwise_model_all3$residuals)
                           , autocorr_dw_stat       = lmtest::dwtest(stepwise_model_all3)$statistic
                           , autocorr_dw_pvalue     = lmtest::dwtest(stepwise_model_all3)$p.value
                           , homosced_bp_stat       = lmtest::bptest(stepwise_model_all3)$statistic
                           , homosced_bp_pvalue     = lmtest::bptest(stepwise_model_all3)$p.value
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
) 

############################

# Top 5 areas where actual value is better than predicted

final_all3_best <- augment(stepwise_model_all3) %>%
  top_n(-5, .resid)

#################################################################

# Check for influential points

model_stepwise_all3_influen <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% 
                                    mutate(observation = row_number()) %>%
                                    filter(observation %in% unique(augment(stepwise_model_all3) %>%
                                                                     mutate(mean_cook_threshold = 4*mean(.cooksd)
                                                                            , observation = row_number()) %>%
                                                                     filter(.cooksd < mean_cook_threshold))$observation) %>%
                                    dplyr::select(rcp_ovw_ob_1819, utla17cd, attr(terms(stepwise_model_all3), "term.labels")) 
)

vif_model_stepwise_all3_influen <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% dplyr::select(attr(terms(model_stepwise_all3_influen), "term.labels"))
                                                             , obesity_outcome_vars$rcp_ovw_ob_1819)$idiags
                                             , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_model_stepwise_all3_influen <- as_tibble(cor(obesity_outcome_vars %>% dplyr::select(attr(terms(model_stepwise_all3_influen), "term.labels")))
                                                 , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_model_stepwise_all3_influen[pearson_model_stepwise_all3_influen > -0.8 & pearson_model_stepwise_all3_influen < 0.8] <- NA

summary(model_stepwise_all3_influen)

final_model_all3_influen <- tidy(model_stepwise_all3_influen) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr"))
final_fit_all3_influen <- glance(model_stepwise_all3_influen)

#############################

# Test model assumptions in influential model

final_all3_influen_tests <- tibble(linear_sp_stat   = shapiro.test(model_stepwise_all3_influen$residuals)$statistic
                                   , linear_sp_pvalue       = shapiro.test(model_stepwise_all3_influen$residuals)$p.value
                                   , linear_mean_resid      = mean(model_stepwise_all3_influen$residuals)
                                   , autocorr_dw_stat       = lmtest::dwtest(model_stepwise_all3_influen)$statistic
                                   , autocorr_dw_pvalue     = lmtest::dwtest(model_stepwise_all3_influen)$p.value
                                   , homosced_bp_stat       = lmtest::bptest(model_stepwise_all3_influen)$statistic
                                   , homosced_bp_pvalue     = lmtest::bptest(model_stepwise_all3_influen)$p.value
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
) 

############################

# Top 5 areas where actual value is better than predicted in influential model

final_all3_influen_best <- augment(model_stepwise_all3_influen) %>%
  top_n(-5, .resid)

####################################################################################

# Test refining model to just significant variables

summary(stepwise_model_all3)

# active, ffood, cyp_inactive, sp_chpa, greenact, blue

model_all3_1 <-  lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% dplyr::select(rcp_ovw_ob_1819, utla17cd
                                                                                                 , attr(terms(stepwise_model_all3), "term.labels")
                                                                                                 , -active_new_se_1617
                                                                                                 , -ffood_u5_phe_17
                                                                                                 , -prop_cyp_inactive_se_mean_1718
                                                                                                 , -sp_chpa_mhclg_pl_1516_1819
                                                                                                 , -greenact_u5_ahah2_17
                                                                                                 , -blue_u5_ahah2_17
))

glance(model_all3_1)

summary(model_all3_1)

refin_model_all3 <- tidy(model_all3_1) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr"))
refin_fit_all3 <- glance(model_all3_1)

############################

# Test model assumptions on refined model

refin_all3_tests <- tibble(linear_sp_stat           = shapiro.test(model_all3_1$residuals)$statistic
                           , linear_sp_pvalue       = shapiro.test(model_all3_1$residuals)$p.value
                           , linear_mean_resid      = mean(model_all3_1$residuals)
                           , autocorr_dw_stat       = lmtest::dwtest(model_all3_1)$statistic
                           , autocorr_dw_pvalue     = lmtest::dwtest(model_all3_1)$p.value
                           , homosced_bp_stat       = lmtest::bptest(model_all3_1)$statistic
                           , homosced_bp_pvalue     = lmtest::bptest(model_all3_1)$p.value
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
) 

############################

# Top 5 areas where actual value is better than predicted in refined model

refin_all3_best <- augment(model_all3_1) %>%
  top_n(-5, .resid)

#################################################################

# Check for influential points in refined model

refin_model_all3_1_influen <- lm(rcp_ovw_ob_1819 ~ . -utla17cd, data = obesity_outcome_vars %>% 
                                   mutate(observation = row_number()) %>%
                                   filter(observation %in% unique(augment(model_all3_1) %>%
                                                                    mutate(mean_cook_threshold = 4*mean(.cooksd)
                                                                           , observation = row_number()) %>%
                                                                    filter(.cooksd < mean_cook_threshold))$observation) %>%
                                   dplyr::select(rcp_ovw_ob_1819, utla17cd, attr(terms(model_all3_1), "term.labels")) 
)

vif_refin_model_all3_1_influen <- as_tibble(mctest::imcdiag(obesity_outcome_vars %>% dplyr::select(attr(terms(refin_model_all3_1_influen), "term.labels"))
                                                            , obesity_outcome_vars$rcp_ovw_ob_1819)$idiags
                                            , validate = NULL, .name_repair = "unique", rownames = "variables")

pearson_refin_model_all3_1_influen <- as_tibble(cor(obesity_outcome_vars %>% dplyr::select(attr(terms(refin_model_all3_1_influen), "term.labels")))
                                                , validate = NULL, .name_repair = NULL, rownames = "variables")
pearson_refin_model_all3_1_influen[pearson_refin_model_all3_1_influen > -0.8 & pearson_refin_model_all3_1_influen < 0.8] <- NA

summary(refin_model_all3_1_influen)

refin_model_all3_influen <- tidy(refin_model_all3_1_influen) %>%
  mutate(fdr_pvalue = p.adjust(p.value, method = "fdr"))
refin_fit_all3_influen <- glance(refin_model_all3_1_influen)

#############################

# Test model assumptions in refined influential model

refin_all3_influen_tests <- tibble(linear_sp_stat   = shapiro.test(refin_model_all3_1_influen$residuals)$statistic
                                   , linear_sp_pvalue       = shapiro.test(refin_model_all3_1_influen$residuals)$p.value
                                   , linear_mean_resid      = mean(refin_model_all3_1_influen$residuals)
                                   , autocorr_dw_stat       = lmtest::dwtest(refin_model_all3_1_influen)$statistic
                                   , autocorr_dw_pvalue     = lmtest::dwtest(refin_model_all3_1_influen)$p.value
                                   , homosced_bp_stat       = lmtest::bptest(refin_model_all3_1_influen)$statistic
                                   , homosced_bp_pvalue     = lmtest::bptest(refin_model_all3_1_influen)$p.value
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
) 

############################

# Top 5 areas where actual value is better than predicted in refined influential model

refin_all3_influen_best <- augment(refin_model_all3_1_influen) %>%
  top_n(-5, .resid)

##################################################################################################################################

############# Save full model results #############

write_xlsx(list(environment_block = environment_block 
                # disadvantage_block = disadvantage_block
                , stress_block = stress_block
                , bf_block = bf_block, safety_block = safety_block, socioeconomic_block = socioeconomic_block
                , blocks_adj_rsq = blocks_adj_rsq
                , model3 = final_model_all3, stdcoeff_model3 = stdcoeff_model3
                , model3_fit = final_fit_all3, model3_tests = final_all3_tests, model3_best = final_all3_best, model3_relimp = final_relimp_all3
                , model3_influen = final_model_all3_influen, model3_fit_influen = final_fit_all3_influen
                , model3_tests_influen = final_all3_influen_tests, model3_best_influen = final_all3_influen_best
                , refin_model3 = refin_model_all3, refin_model3_fit = refin_fit_all3, refin_model3_tests = refin_all3_tests
                , refin_model3_best = refin_all3_best, refin_model3_influen = refin_model_all3_influen
                , refin_model3_fit_influen = refin_fit_all3_influen, refin_model3_tests_influen = refin_all3_influen_tests
                , refin_model3_best_influen = refin_all3_influen_best)
           ,"output/multiple_linear_regression/Blocks_full_models_obesity_rcp_UTLA.xlsx", col_names=TRUE)

##################################################################################################################################

############# Summary information for final model variables #############


final_vars <- obesity_outcome_vars_long %>%
  filter(variable %in% attr(terms(stepwise_model_all3), "term.labels")) %>%
  dplyr::select(utla17cd, variable, value) %>%
  bind_rows(obesity_outcome_vars_long %>%
              filter(variable == "active_new_se_1617") %>%
              dplyr::select(utla17cd, rcp_ovw_ob_1819) %>%
              rename(value = rcp_ovw_ob_1819) %>%
              mutate(variable = "rcp_ovw_ob_1819")) %>%
  group_by(variable) %>%
  mutate(q20 = quantile(value, c(.2))
         , q80 = quantile(value, c(.8))
         , quintile1 = case_when(value < q20 ~ value
                                 , TRUE ~ NA_real_)
         , quintile5 = case_when(value >= q80 ~ value
                                 , TRUE ~ NA_real_))

obesity_vars_desc <- final_vars %>%
  group_by(variable) %>%
  summarise(min = min(value)
            , q25 = quantile(value, c(.25))
            , q40 = quantile(value, c(.40))
            , median = quantile(value, c(.50))
            , q60 = quantile(value, c(.60))
            , q75 = quantile(value, c(.75))
            , max = max(value)
            , mean = mean(value)
            , sd = sd(value)
            , mean_q1 = mean(quintile1, na.rm = TRUE)
            , mean_q5 = mean(quintile5, na.rm = TRUE))

write_csv(obesity_vars_desc, "output/multiple_linear_regression/Blocks_final_vars_desc.csv")

##################################################################################################################################
