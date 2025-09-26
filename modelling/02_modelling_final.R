library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(gt)
load("data/tidy_data/data_species_level.RData")

##filter data that has a chance at reaching RG : 30%RG, over 10 obs, over 10 RG, excluding prop==1 ----

species_counts <- df %>%
        group_by(species) %>%
        summarise(
                n_obs = n(),
                n_rg = sum(rg == 1),
                prop_rg = n_rg / n_obs
        ) %>%
        filter(n_obs >= 10 & prop_rg >= 0.3 & n_rg >=10 & prop_rg!=1 ) 

data_30_test_month <- df %>% 
        filter(species %in% (species_counts$species)) %>% 
        mutate(observed_on_month = as.character(observed_on_month)) #avoids ordered month

rm(df, species_counts)


##full model ----

fit_full <- glmmTMB(rg ~ log_ysu + Nph + Nproj + Nobsf + Ntags + Nnotes + 
                            n_info_no_proj:Nproj + n_info_no_obsf:Nobsf+ 
                            n_info_no_tags:Ntags + n_info_no_notes:Nnotes +
                            (1|observed_on_month) +(1|species) + (1|genus), 
                    data = data_30_test_month, family="binomial", na.action ="na.fail")

summary(fit_full)
AIC(fit_full)
#1460264

### drops full ----

fit_proj <- glmmTMB(rg ~ log_ysu + Nph + presence_projects + Nobsf + Ntags + Nnotes + n_info_no_obsf:Nobsf + 
                            n_info_no_tags:Ntags + n_info_no_notes:Nnotes +
                            n_info_no_proj:presence_projects +
                            (1|observed_on_month) +(1|species) + (1|genus), 
                    data = data_30_test_month, family="binomial", na.action ="na.fail")


fit_obsf <- glmmTMB(rg ~ log_ysu + Nph + Nproj + presence_obs_fields + Ntags + Nnotes +
                            n_info_no_proj:Nproj + n_info_no_tags:Ntags +
                            n_info_no_notes:Nnotes + n_info_no_obsf:presence_obs_fields +
                            (1|observed_on_month) +(1|species) + (1|genus), 
                    data = data_30_test_month, family="binomial", na.action ="na.fail")


fit_tags <- glmmTMB(rg ~ log_ysu + Nph + Nproj + Nobsf + presence_tags + Nnotes + 
                            n_info_no_proj:Nproj + n_info_no_obsf:Nobsf+ 
                            n_info_no_notes:Nnotes + n_info_no_tags:presence_tags +
                            (1|observed_on_month) +(1|species) + (1|genus), 
                    data = data_30_test_month, family="binomial", na.action ="na.fail")


fit_notes <- glmmTMB(rg ~ log_ysu + Nph + Nproj + Nobsf + Ntags + presence_notes + 
                             n_info_no_proj:Nproj + n_info_no_obsf:Nobsf + 
                             n_info_no_tags:Ntags + n_info_no_notes:presence_notes +
                             (1|observed_on_month) +(1|species) + (1|genus), 
                     data = data_30_test_month, family="binomial", na.action ="na.fail")


AIC(fit_full, fit_proj, fit_obsf, fit_tags, fit_notes)

### drops no obsf ----

fit_obsf_proj <- glmmTMB(rg ~ log_ysu + Nph + presence_projects + presence_obs_fields + Ntags + Nnotes +
                                 n_info_no_tags:Ntags + n_info_no_notes:Nnotes + 
                                 n_info_no_obsf:presence_obs_fields + n_info_no_proj:presence_projects +
                                 (1|observed_on_month) + (1|species) + (1|genus), 
                         data = data_30_test_month, family="binomial", na.action ="na.fail")


fit_obsf_tags <- glmmTMB(rg ~ log_ysu + Nph + Nproj + presence_obs_fields + presence_tags + Nnotes +
                                 n_info_no_proj:Nproj  + n_info_no_notes:Nnotes +
                                 n_info_no_obsf:presence_obs_fields + n_info_no_tags:presence_tags +
                                 (1|observed_on_month) + (1|species) + (1|genus), 
                         data = data_30_test_month, family="binomial", na.action ="na.fail")


fit_obsf_notes <- glmmTMB(rg ~ log_ysu + Nph + Nproj + presence_obs_fields + Ntags + presence_notes +
                                  n_info_no_proj:Nproj + n_info_no_tags:Ntags +
                                  n_info_no_obsf:presence_obs_fields + n_info_no_notes:presence_notes +
                                  (1|observed_on_month) + (1|species) + (1|genus), 
                          data = data_30_test_month, family="binomial", na.action ="na.fail")


AIC(fit_obsf_proj, fit_obsf_tags, fit_obsf_notes)

### drops fit_obsf_notes ----

fit_obsf_notes_proj <- glmmTMB(rg ~ log_ysu + Nph + presence_projects + presence_obs_fields + Ntags + presence_notes +
                                       n_info_no_tags:Ntags + n_info_no_proj:presence_projects +
                                       n_info_no_obsf:presence_obs_fields + n_info_no_notes:presence_notes +
                                       (1|observed_on_month) + (1|species) + (1|genus), 
                               data = data_30_test_month, family="binomial", na.action ="na.fail")



fit_obsf_notes_tags <- glmmTMB(rg ~ log_ysu + Nph + Nproj + presence_obs_fields + presence_tags + presence_notes +
                                       n_info_no_proj:Nproj + n_info_no_tags:presence_tags +
                                       n_info_no_obsf:presence_obs_fields + n_info_no_notes:presence_notes +
                                       (1|observed_on_month) + (1|species) + (1|genus), 
                               data = data_30_test_month, family="binomial", na.action ="na.fail")


AIC(fit_obsf_notes_proj, fit_obsf_notes_tags)

### drop fit_obsf_notes_proj ----

fit_obsf_notes_proj_tags <- glmmTMB(rg ~ log_ysu + Nph + presence_projects + presence_obs_fields + presence_tags + presence_notes +
                                            n_info_no_tags:presence_tags + n_info_no_notes:presence_notes + 
                                            n_info_no_obsf:presence_obs_fields + n_info_no_proj:presence_projects +
                                            (1|observed_on_month) + (1|species) + (1|genus), 
                                    data = data_30_test_month, family="binomial", na.action ="na.fail")



### drop1 on best model fit_obsf_notes_proj ----

drop1(fit_obsf_notes_proj)


fit_no_tags_inter_genus <- glmmTMB(rg ~ log_ysu + Nph + presence_projects + presence_obs_fields + Ntags + presence_notes +
                                           n_info_no_notes:presence_notes + n_info_no_obsf:presence_obs_fields + 
                                           n_info_no_proj:presence_projects +
                                           (1|observed_on_month) + (1|species) + (1|genus), 
                                   data = data_30_test_month, family="binomial", na.action ="na.fail")

drop1(fit_no_tags_inter_genus)
# Single term deletions
# 
# Model:
# rg ~ log_ysu + Nph + presence_projects + presence_obs_fields + 
#     Ntags + presence_notes + n_info_no_notes:presence_notes + 
#     n_info_no_obsf:presence_obs_fields + n_info_no_proj:presence_projects + 
#     (1 | observed_on_month) + (1 | species) + (1 | genus)
#                                    Df     AIC
# <none>                                1458209
# log_ysu                             1 1483227
# Nph                                 5 1460892
# Ntags                               1 1458487
# presence_notes:n_info_no_notes      1 1458738
# presence_obs_fields:n_info_no_obsf  1 1458375
# presence_projects:n_info_no_proj    1 1458292

summary(fit_no_tags_inter_genus)
#  Family: binomial  ( logit )
# Formula:          rg ~ log_ysu + Nph + presence_projects + presence_obs_fields +      Ntags + presence_notes + n_info_no_notes:presence_notes +  
#     n_info_no_obsf:presence_obs_fields + n_info_no_proj:presence_projects +      (1 | observed_on_month) + (1 | species) + (1 | genus)
# Data: data_30_test_month
# 
#       AIC       BIC    logLik -2*log(L)  df.resid 
# 1458209.2 1458415.7 -729087.6 1458175.2   1389530 
# 
# Random effects:
# 
# Conditional model:
#  Groups            Name        Variance Std.Dev.
#  observed_on_month (Intercept) 0.04009  0.2002  
#  species           (Intercept) 0.54038  0.7351  
#  genus             (Intercept) 0.53143  0.7290  
# Number of obs: 1389547, groups:  observed_on_month, 12; species, 2309; genus, 830
# 
# Conditional model:
#                                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         0.018894   0.066949    0.28    0.778    
# log_ysu                             1.258683   0.008050  156.36   <2e-16 ***
# Nph2                                0.154220   0.005079   30.36   <2e-16 ***
# Nph3                                0.231833   0.006489   35.73   <2e-16 ***
# Nph4                                0.280553   0.008981   31.24   <2e-16 ***
# Nph5-10                             0.288649   0.010175   28.37   <2e-16 ***
# Nph>10                              0.413160   0.040481   10.21   <2e-16 ***
# presence_projects                   0.357395   0.011710   30.52   <2e-16 ***
# presence_obs_fields                 0.752204   0.021020   35.79   <2e-16 ***
# Ntags                               0.106031   0.006441   16.46   <2e-16 ***
# presence_notes                      0.344931   0.007312   47.18   <2e-16 ***
# presence_notes:n_info_no_notes     -0.440754   0.019274  -22.87   <2e-16 ***
# presence_obs_fields:n_info_no_obsf -0.323423   0.025047  -12.91   <2e-16 ***
# presence_projects:n_info_no_proj    0.196695   0.021425    9.18   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

fit_no_tags_inter_genus_chr_month <- fit_no_tags_inter_genus
rm(fit_no_tags_inter_genus)

save(fit_no_tags_inter_genus_chr_month, file = "modelling/model_final.RData")

# Diagnostics ###

res <- simulateResiduals(fittedModel = fit_no_tags_inter_genus_chr_month, plot = F)
#load(file="modelling/model_final.RData")
#res <- readRDS("modelling/DHARMa_fit_no_tags_inter_genus_chr_month.rds")
load(file = "data/tidy_data/data_30perc.RData")

plot(res)
plotResiduals(res, data_30_test_month$Nph)
plotResiduals(res, data_30_test_month$Ntags)
plotResiduals(res, data_30_test_month$presence_projects)
plotResiduals(res, data_30_test_month$presence_obs_fields)
plotResiduals(res, data_30_test_month$presence_notes)

#get stats values
testQuantiles(res)
# Test for location of quantiles via qgam
# 
# data:  res
# p-value < 2.2e-16
# alternative hypothesis: both

sig_values_Nph <- testCategorical(res, catPred = data_30_test_month$Nph)
sig_values_Ntags <- testCategorical(res,data_30_test_month$Ntags)
sig_values_proj <- testCategorical(res, catPred = data_30_test_month$presence_projects)
sig_values_obsf <- testCategorical(res, catPred = data_30_test_month$presence_obs_fields)
sig_values_notes <-testCategorical(res, catPred = data_30_test_month$presence_notes)

sig_table_Nph <- data.frame(
                group = c("1", "2", "3", "4", "5-10", ">10"),
                D_stat = sapply(sig_values_Nph$uniformity$details, function(i) i$statistic[[1]]),
                 p_value = sig_values_Nph $uniformity$p.value,
              p_value_cor = sig_values_Nph $uniformity$p.value.cor)

sig_table_Ntags <- data.frame(
        group = c("0", "1", "2", "3", "4", "5", "6"),
        D_stat = sapply(sig_values_Ntags$uniformity$details, function(i) i$statistic[[1]]),
        p_value = sig_values_Ntags$uniformity$p.value,
        p_value_cor = sig_values_Ntags$uniformity$p.value.cor)

sig_table_proj <- data.frame(
        group = c("0","1"),
        D_stat = sapply(sig_values_proj$uniformity$details, function(i) i$statistic[[1]]),
        p_value = sig_values_proj$uniformity$p.value,
        p_value_cor = sig_values_proj$uniformity$p.value.cor)

sig_table_obsf <- data.frame(
        group = c("0","1"),
        D_stat = sapply(sig_values_obsf$uniformity$details, function(i) i$statistic[[1]]),
        p_value = sig_values_obsf$uniformity$p.value,
        p_value_cor = sig_values_obsf$uniformity$p.value.cor)

sig_table_notes <- data.frame(
        group = c("0","1"),
        D_stat = sapply(sig_values_notes$uniformity$details, function(i) i$statistic[[1]]),
        p_value = sig_values_notes$uniformity$p.value,
        p_value_cor = sig_values_notes$uniformity$p.value.cor)


stats_table <- bind_rows(Nph = sig_table_Nph, 
          Ntags = sig_table_Ntags, 
          proj = sig_table_proj,
          obsf = sig_table_obsf, 
          notes = sig_table_notes, 
          .id = "source")

stats_table <- stats_table %>%  gt() %>% 
        cols_label(
                source = "Variable",
                group = "Category")

gtsave(stats_table, "modelling/diagnostic_plots_final_model/stats_table.pdf")


#actual RG prop. of unique combinations of predictors vs fitted values
data_downsampled <- data_30_test_month %>% 
        group_by(log_ysu, Nph, presence_projects, presence_obs_fields, presence_notes, Ntags, n_info_no_notes, n_info_no_obsf, n_info_no_proj,genus, species) %>% 
        count(rg) %>% 
        pivot_wider(names_from = "rg", values_from = "n", values_fill = 0) %>% 
        mutate(prop = `1`/(`0`+`1`)) %>% 
        mutate(observed_on_month="May")

data_downsampled$fitted <- predict(fit_no_tags_inter_genus_chr_month, newdata=data_downsampled, 
                                   type = "response" ) # predicts response per grouping combination

ggplot(data_30_test_month %>% left_join(data_downsampled %>% select(-observed_on_month)), aes(x=fitted, y=prop))+geom_point(aes(colour=Nph))+
        geom_smooth(method="lm")+
        geom_smooth(colour="red", se=FALSE)+
        geom_abline(intercept=0, slope=1)

ggplot(data_30_test_month %>% left_join(data_downsampled), aes(x=fitted, y=prop))+
        geom_point(colour = "grey60", alpha = 0.5)+
        geom_smooth(method="lm")+
        geom_smooth(colour="red", se=FALSE)+
        geom_abline(intercept=0, slope=1) +
        #scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        #scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        #coord_fixed()+
        theme_classic()

# Joining with `by = join_by(observed_on_month, genus, presence_notes, species, log_ysu, presence_projects,
# presence_obs_fields, Ntags, Nph, n_info_no_notes, n_info_no_proj, n_info_no_obsf)`
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
# Warning messages:
# 1: Removed 1283522 rows containing non-finite outside the scale range (`stat_smooth()`). 
# 2: Removed 1283522 rows containing non-finite outside the scale range (`stat_smooth()`). 
# 3: Removed 1283522 rows containing missing values or values outside the scale range (`geom_point()`). 


####save files ----
save(fit_no_tags_inter_genus_chr_month, file = "modelling/model_final.RData")
save(data_30_test_month, file = "data/tidy_data/data_30perc.RData" )
