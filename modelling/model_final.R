library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(sjPlot)
library(emmeans)


load("data/data_species_level.RDS")

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

## drops full ----

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

## drops no obsf ----

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

## drops fit_obsf_notes ----

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

## drop fit_obsf_notes_proj ----

fit_obsf_notes_proj_tags <- glmmTMB(rg ~ log_ysu + Nph + presence_projects + presence_obs_fields + presence_tags + presence_notes +
                                            n_info_no_tags:presence_tags + n_info_no_notes:presence_notes + 
                                            n_info_no_obsf:presence_obs_fields + n_info_no_proj:presence_projects +
                                            (1|observed_on_month) + (1|species) + (1|genus), 
                                    data = data_30_test_month, family="binomial", na.action ="na.fail")


save(fit_obsf_notes_proj, file = "modelling/fit_obsf_notes_proj_genus.RDS")


## drop1 on best model fit_obsf_notes_proj ----

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

save(fit_no_tags_inter_genus_chr_month, file = "modelling/fit_no_tags_inter_genus_chr_month.RData")

## Diagnostics ----

plot(res)
plotResiduals(res, data_30_test_month$Nph)
plotResiduals(res, data_30_test_month$Ntags)
plotResiduals(res, data_30_test_month$presence_projects)
plotResiduals(res, data_30_test_month$presence_obs_fields)
plotResiduals(res, data_30_test_month$presence_notes)

#actual RG prop. of unique combinations of predictors vs fitted values
data_downsampled <- data_30_test_month %>% group_by(log_ysu, Nph, presence_projects, presence_obs_fields, presence_notes, Ntags, n_info_no_notes, n_info_no_obsf, n_info_no_proj,genus, species) %>% count(rg) %>% 
        pivot_wider(names_from = "rg", values_from = "n", values_fill = 0) %>% 
        mutate(prop = `1`/(`0`+`1`)) %>% 
        mutate(observed_on_month="May")
data_downsampled$fitted <- predict(fit_no_tags_inter_genus_chr_month, newdata=data_downsampled, 
                                   type = "response" )
ggplot(data_30_test_month %>% left_join(data_downsampled %>% select(-observed_on_month)), aes(x=fitted, y=prop))+geom_point(aes(colour=Nph))+
        geom_smooth(method="lm")+
        geom_smooth(colour="red", se=FALSE)+
        geom_abline(intercept=0, slope=1)

ggplot(data_30_test_month %>% left_join(data_downsampled), aes(x=fitted, y=prop))+
        geom_point(aes(colour=Nph))+
        geom_smooth(method="lm")+
        geom_smooth(colour="red", se=FALSE)+
        geom_abline(intercept=0, slope=1) +
        # scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        # scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        # coord_fixed()
        theme_classic()

# Joining with `by = join_by(observed_on_month, genus, presence_notes, species, log_ysu, presence_projects,
# presence_obs_fields, Ntags, Nph, n_info_no_notes, n_info_no_proj, n_info_no_obsf)`
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
# Warning messages:
# 1: Removed 1283522 rows containing non-finite outside the scale range (`stat_smooth()`). 
# 2: Removed 1283522 rows containing non-finite outside the scale range (`stat_smooth()`). 
# 3: Removed 1283522 rows containing missing values or values outside the scale range (`geom_point()`). 


## Plots: Projects ----

fit_final <- fit_no_tags_inter_genus_chr_month


#what is the actual probability?
emm_proj <- as.data.frame(
        emmeans(fit_final, ~ presence_projects * n_info_no_proj,
                at = list(n_info_no_proj = c(0,1,2,3)),
                type="response",
                weights = "cells",
                nesting = NULL))
 # presence_projects n_info_no_proj      prob         SE  df asymp.LCL asymp.UCL
 #                 0              0 0.6388645 0.01542645 Inf 0.6081136 0.6685178
 #                 1              0 0.7469953 0.01282524 Inf 0.7210440 0.7712981
 #                 0              1 0.6388645 0.01542645 Inf 0.6081136 0.6685178
 #                 1              1 0.7823384 0.01185891 Inf 0.7582027 0.8046856
 #                 0              2 0.6388645 0.01542645 Inf 0.6081136 0.6685178
 #                 1              2 0.8139738 0.01174157 Inf 0.7898564 0.8358985
 #                 0              3 0.6388645 0.01542645 Inf 0.6081136 0.6685178
 #                 1              3 0.8419402 0.01197242 Inf 0.8170377 0.8640174

#are the differences significant?
sig_proj <- emtrends(fit_final, ~ n_info_no_proj, 
                     at = list(n_info_no_proj = c(0,1,2,3)), var= "presence_projects",
                     nesting = NULL) %>%  
        test()%>%
        as.data.frame() %>%
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                presence_projects.trend>0~"+",
                TRUE~"-"
        ))

#   n_info_no_proj presence_projects.trend         SE  df  z.ratio       p.value dir
# 1              0               0.3573947 0.01170996 Inf 30.52056 1.390694e-204   +
# 2              1               0.5540896 0.01958983 Inf 28.28455 5.352786e-176   +
# 3              2               0.7507845 0.03935077 Inf 19.07929  3.753490e-81   +
# 4              3               0.9474795 0.06026016 Inf 15.72315  1.049758e-55   +


#prepare for plotting
dodge <- 0.1 #value for jittering points and segments in plot
n_groups <- length(unique(emm_proj$n_info_no_proj))
offsets <- seq(-dodge / 2, dodge / 2, length.out = n_groups) # value offset to either left and right for each value

emm_proj <- emm_proj %>%
        left_join(
                sig_proj %>% select(n_info_no_proj, p.value, dir),  #join significance from emmtrends
                by = "n_info_no_proj") %>% 
        mutate(
                offset = offsets[as.integer(factor(n_info_no_proj))], #get corresponding plot points offsets based on index numbers
                presence_dodged = presence_projects + offset
        )

segments <- emm_proj %>%
        arrange(n_info_no_proj, presence_projects) %>%
        group_by(n_info_no_proj) %>%
        summarise(
                x = first(presence_dodged), #x is either 0 or 1 plus dodge for plotting
                xend = last(presence_dodged),
                y = first(prob), #since it's grouped by n_info, first prob will be for x(presence)=0
                yend = last(prob), # presence of x=1 per n_info
                dir = first(dir),
                .groups = "drop") %>% 
        mutate(linetype = ifelse(dir == "n.s.", "n.s.", "sig."))


ggplot(emm_proj, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_proj))) +
        geom_hline(aes(yintercept=0.018894), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point() +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.05,
                      linewidth = 0.8) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.8)+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0,1) +
        labs(y="Proportion RG", 
             x="Presence of Projects",
             colour = "Amount of other info.",
             linetype = "Significance (p < 0.05)")+
        cowplot::theme_cowplot() +
        theme(legend.position = "bottom")


# Plots: obsf ----
emm_obsf <- as.data.frame(
        emmeans(fit_final, ~ presence_obs_fields * n_info_no_obsf,
                at = list(n_info_no_obsf = c(0,1,2,3)),
                type="response",
                weights = "cells",
                nesting = NULL))

# presence_obs_fields n_info_no_obsf      prob         SE  df asymp.LCL asymp.UCL
#                    0              0 0.6400841 0.01540371 Inf 0.6093735 0.6696889
#                    1              0 0.8244032 0.01013848 Inf 0.8036402 0.8433986
#                    0              1 0.6400841 0.01540371 Inf 0.6093735 0.6696889
#                    1              1 0.7725991 0.01223656 Inf 0.7477291 0.7956873
#                    0              2 0.6400841 0.01540371 Inf 0.6093735 0.6696889
#                    1              2 0.7108719 0.01598896 Inf 0.6785578 0.7411767
#                    0              3 0.6400841 0.01540371 Inf 0.6093735 0.6696889
#                    1              3 0.6401924 0.02124586 Inf 0.5975875 0.6806967

sig_obsf <- emtrends(fit_final, ~ n_info_no_obsf, 
                     at = list(n_info_no_obsf = c(0,1,2,3)), var= "presence_obs_fields",
                     nesting = NULL) %>%  
        test()%>%
        as.data.frame() %>%
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                presence_obs_fields.trend>0~"+",
                TRUE~"-"
        ))

#   n_info_no_obsf presence_obs_fields.trend         SE  df   z.ratio       p.value dir
# 1              0                 0.7522044 0.02101996 Inf 35.785248 1.873348e-280   +
# 2              1                 0.4287810 0.01980732 Inf 21.647599 6.403015e-104   +
# 3              2                 0.1053576 0.03996960 Inf  2.635944  8.390361e-03   +
# 4              3                -0.2180657 0.06369894 Inf -3.423381  6.184733e-04   -


dodge <- 0.1
n_groups <- length(unique(emm_obsf$n_info_no_obsf))
offsets <- seq(-dodge / 2, dodge / 2, length.out = n_groups) # value offset to either left and right for each value

emm_obsf <- emm_obsf %>%
        left_join(
                sig_obsf %>% select(n_info_no_obsf, p.value, dir), 
                by = "n_info_no_obsf") %>% 
        mutate(
                offset = offsets[as.integer(factor(n_info_no_obsf))], #get corresponding offsets based on index 
                presence_dodged = presence_obs_fields + offset
        )

segments <- emm_obsf %>%
        arrange(n_info_no_obsf, presence_obs_fields) %>%
        group_by(n_info_no_obsf) %>%
        summarise(
                x = first(presence_dodged), #x is either 0 or 1 plus dodge for plotting
                xend = last(presence_dodged),
                y = first(prob), #since it's grouped by n_info, first prob will be for x(presence)=0
                yend = last(prob), # presence of x=1 per n_info
                dir = first(dir),
                .groups = "drop") %>% 
        mutate(linetype = ifelse(dir == "n.s.", "n.s.", "sig."))


ggplot(emm_obsf, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_obsf))) +
        geom_hline(aes(yintercept=0.018894), 
                colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point() +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.05,
                      linewidth = 0.8) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.8)+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0,1) +
        labs(y="Proportion RG", 
             x="Presence of Observation Fields",
             colour = "Amount of other info.",
             linetype = "Significance (p < 0.05)")+
        cowplot::theme_cowplot() +
        theme(legend.position = "bottom")

#Plots: notes ----

emm_notes <- as.data.frame(
        emmeans(fit_final, ~ presence_notes * n_info_no_notes,
                at = list(n_info_no_notes = c(0,1,2,3)),
                type="response",
                weights = "cells",
                nesting = NULL))

 # presence_notes n_info_no_notes      prob         SE  df asymp.LCL asymp.UCL
 #              0               0 0.6359085 0.01548358 Inf 0.6050558 0.6656828
 #              1               0 0.7273818 0.01330782 Inf 0.7005347 0.7526719
 #              0               1 0.6359085 0.01548358 Inf 0.6050558 0.6656828
 #              1               1 0.6319578 0.01609351 Inf 0.5998904 0.6628994
 #              0               2 0.6359085 0.01548358 Inf 0.6050558 0.6656828
 #              1               2 0.5249470 0.01901596 Inf 0.4876067 0.5620104
 #              0               3 0.6359085 0.01548358 Inf 0.6050558 0.6656828
 #              1               3 0.4155949 0.02115176 Inf 0.3748245 0.4575543

sig_notes <- emtrends(fit_final, ~ n_info_no_notes, 
                     at = list(n_info_no_notes = c(0,1,2,3)), var= "presence_notes",
                     nesting = NULL) %>%  
        test()%>%
        as.data.frame() %>%
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                presence_notes.trend>0~"+",
                TRUE~"-"
        ))

#   n_info_no_notes presence_notes.trend          SE  df    z.ratio      p.value dir
# 1               0           0.34493149 0.007311629 Inf  47.175739 0.000000e+00   +
# 2               1          -0.09582279 0.018492008 Inf  -5.181849 2.196972e-07   -
# 3               2          -0.53657707 0.037059463 Inf -14.478814 1.649184e-47   -
# 4               3          -0.97733136 0.056105332 Inf -17.419581 5.860512e-68   -


dodge <- 0.1
n_groups <- length(unique(emm_notes$n_info_no_notes))
offsets <- seq(-dodge / 2, dodge / 2, length.out = n_groups) # value offset to either left and right for each value

emm_notes <- emm_notes %>%
        left_join(
                sig_notes %>% select(n_info_no_notes, p.value, dir), #add significance from emmtrends
                by = "n_info_no_notes") %>% 
        mutate(
                offset = offsets[as.integer(factor(n_info_no_notes))], #get corresponding offsets based on index 
                presence_dodged = presence_notes + offset
        )

segments <- emm_notes %>%
        arrange(n_info_no_notes, presence_notes) %>%
        group_by(n_info_no_notes) %>%
        summarise(
                x = first(presence_dodged), #x is either 0 or 1 plus dodge for plotting
                xend = last(presence_dodged),
                y = first(prob), #since it's grouped by n_info, first prob will be for x(presence)=0
                yend = last(prob), # presence of x=1 per n_info
                dir = first(dir),
                .groups = "drop") %>% 
        mutate(linetype = ifelse(dir == "n.s.", "n.s.", "sig."))


ggplot(emm_notes, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_notes))) +
        geom_hline(aes(yintercept=0.018894), 
        colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point() +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.05,
                      linewidth = 0.8) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.8)+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0,1) +
        labs(y="Proportion RG", 
             x="Presence of Notes",
             colour = "Amount of other info.",
             linetype = "Significance (p < 0.05)")+
        cowplot::theme_cowplot() +
        theme(legend.position = "bottom")

#Plots: logysu ----


emm_logysu <- as.data.frame(
        emmeans(fit_final, "log_ysu", 
                at = list(log_ysu=seq(min(data_30_test_month$log_ysu),
                                      max(data_30_test_month$log_ysu), by=0.1)),
                weights = "cells",
                type="response",
                nesting = NULL))


sig_logysu <- emtrends(fit_final, specs = ~ 1, var = "log_ysu", weights = "cells") %>%  #specs specifies no grouping
        test() %>% 
        as.data.frame() %>%
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                log_ysu.trend>0~"+",
                TRUE~"-"
        ))
#         1 log_ysu.trend        SE  df  z.ratio p.value dir
# 1 overall      1.258683 0.0080498 Inf 156.3621       0   +

#Since emmtrends was significant I just added line and ribbon (instead of segments)
ggplot(emm_logysu, aes(x = log_ysu, y = prob)) +
        geom_hline(aes(yintercept=0.018894), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_line() +
        geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                    alpha = 0.2) +
        ylim(0,1) +
        labs(y="Proportion RG", 
             x="Years since upload (log10)") +
        cowplot::theme_cowplot() 
        

#Plots: Nph ----

emm_Nph <- emmeans(fit_final, ~ Nph,
                weights = "cells",
                nesting = NULL)
# Nph  emmean     SE  df asymp.LCL asymp.UCL
# 1     0.498 0.0669 Inf     0.367     0.630
# 2     0.662 0.0670 Inf     0.531     0.794
# 3     0.746 0.0671 Inf     0.615     0.878
# 4     0.805 0.0673 Inf     0.673     0.937
# 5-10  0.855 0.0675 Inf     0.723     0.988
# >10   1.150 0.0779 Inf     0.997     1.303


#test significance between each step
sig_Nph <- contrast(emm_Nph, "consec", weights = "cells") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))
#       contrast   estimate          SE  df   z.ratio      p.value dir
# 1        2 - 1 0.16378303 0.005077478 Inf 32.256770 0.000000e+00   +
# 2        3 - 2 0.08408739 0.007102793 Inf 11.838638 0.000000e+00   +
# 3        4 - 3 0.05858708 0.010206969 Inf  5.739909 3.698855e-08   +
# 4   (5-10) - 4 0.05052658 0.012807971 Inf  3.944932 4.061512e-04   +
# 5 >10 - (5-10) 0.29465967 0.041431145 Inf  7.112033 3.532175e-12   +


#update emmeans value to probabilities
emm_Nph <- emm_Nph %>% 
        update(type="response") %>% 
        as.data.frame()


segments<- emm_Nph %>%
        mutate(Nph_order = row_number()) %>% # creates a numeric position for plotting
        
        mutate( x = Nph_order, #start of seg. correspond to actual Nph and prob
                y = prob) %>% 
        
        mutate(Nph_end = lead(Nph),     # just used to check - lead gets value that comes next in Nph
               xend = lead(x),        
               yend = lead(y)) %>% 
        filter(!is.na(xend)) %>%   # remove the last row - there's no next step
        
        #add contrast column so I can join with sig_Nph
        mutate(Nph_end = case_when(Nph_end == "5-10" ~ "(5-10)", T ~ Nph_end),
               Nph = case_when(Nph == "5-10" ~ "(5-10)",T ~ Nph),
               contrast = paste(Nph_end, "-", Nph)) %>% 
        left_join(
                sig_Nph %>% select(contrast, p.value, dir), #add significance from contrast
                by = "contrast") %>% 
        mutate(linetype = ifelse(dir == "n.s.", "n.s.", "sig."))


ggplot(emm_Nph, aes(x = Nph, y = prob)) +
        geom_hline(aes(yintercept=0.018894), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point()+
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.1)


ggplot(emm_Nph, aes(x = Nph, y = prob)) +
        geom_hline(aes(yintercept=0.018894), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point() +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.05,
                      linewidth = 0.8) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.8)+
        ylim(0,1) +
        labs(y="Proportion RG", 
             x="Number of Photos (binned)",
             linetype = "Significance (p < 0.05)")+
        cowplot::theme_cowplot() +
        theme(legend.position = "bottom")

        
     