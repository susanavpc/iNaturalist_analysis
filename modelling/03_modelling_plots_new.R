library(tidyverse)
library(emmeans)
library(glmmTMB)

load(file = "modelling/model_final.RData")
fit_final <- fit_no_tags_inter_genus_chr_month; rm(fit_no_tags_inter_genus_chr_month)

load(file = "data/tidy_data/data_30perc.RData")


##### YSU ----

### CALCULATION

#save EMMs in logit scale for reference
emm_log_ysu <- emmeans(fit_final, "log_ysu", at = list(log_ysu = c(min(data_30_test_month$log_ysu),
                                                                   max(data_30_test_month$log_ysu))),
                       weights = "cells",
                       nesting = NULL)

#  log_ysu emmean     SE  df asymp.LCL asymp.UCL
#      0.0  0.189 0.0669 Inf    0.0581      0.32
#      1.2  1.705 0.0673 Inf    1.5729      1.84
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields, presence_notes 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

#calculate contrasts in logit scale to check significance. Same as pairs(reverse = TRUE)
sig_log_ysu <- contrast(emm_log_ysu , method = "revpairwise",  weights = "cells") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#                             contrast estimate          SE  df  z.ratio p.value dir
# 1 log_ysu1.20411998265592 - log_ysu0 1.515606 0.009692925 Inf 156.3621       0   +


#calculate EMM values in response scale for plotting
emm_log_ysu_resp <- emmeans(fit_final, "log_ysu", at = list(log_ysu = c(min(data_30_test_month$log_ysu),
                                                    max(data_30_test_month$log_ysu))),
        weights = "cells", nesting = NULL, type = "response")

#  log_ysu  prob      SE  df asymp.LCL asymp.UCL
#      0.0 0.547 0.01660 Inf     0.515     0.579
#      1.2 0.846 0.00876 Inf     0.828     0.863
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields, presence_notes 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 

#check contrast values in response scale for reference
contrast(emm_log_ysu_resp , method = "revpairwise",  weights = "cells", type = "response")

# contrast                           odds.ratio     SE  df null z.ratio p.value
# log_ysu1.20411998265592 / log_ysu0       4.55 0.0441 Inf    1 156.362  <.0001
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields, presence_notes 
# Tests are performed on the log odds ratio scale 

emm_log_ysu_resp <- as.data.frame(emm_log_ysu_resp)

### PLOTTING

#Since emmtrends was significant I just added line and ribbon (instead of segments)
p1 <- ggplot(emm_log_ysu_resp, aes(x = log_ysu, y = prob)) +
        geom_hline(aes(yintercept=overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_line(colour = "#08519C", linewidth = 0.6) +
        geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                    alpha = 0.2, fill = "#6BAED6") +
        ylim(0.25,1) +
        labs(y="RG Proportion", 
             x="Years since upload (log10)") +
        cowplot::theme_cowplot()


##### TAGS ----

### CALCULATION

#save EMMs in logit scale for reference
emm_tags <- emmeans(fit_final, ~ Ntags, at = list(Ntags = c(min(data_30_test_month$Ntags),
                                                            max(data_30_test_month$Ntags))),
                    weights = "cells", nesting = NULL )

#  Ntags emmean     SE  df asymp.LCL asymp.UCL
#      0  0.596 0.0669 Inf     0.465     0.727
#      6  1.232 0.0771 Inf     1.081     1.383
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields, presence_notes 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 


#calculate contrasts in logit scale to check significance. Same as pairs(reverse = TRUE)
sig_tags <- contrast(emm_tags , method = "revpairwise",  weights = "cells") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#          contrast  estimate         SE  df  z.ratio      p.value dir
# 1 Ntags6 - Ntags0 0.6361834 0.03864327 Inf 16.46298 6.768425e-61   +

#calculate EMM values in response scale for plotting
emm_tags_resp <- emmeans(fit_final, ~ Ntags, at = list(Ntags = c(min(data_30_test_month$Ntags),
                                                            max(data_30_test_month$Ntags))),
                    weights = "cells", nesting = NULL, type="response")
        
#  Ntags  prob     SE  df asymp.LCL asymp.UCL
#      0 0.645 0.0153 Inf     0.614     0.674
#      6 0.774 0.0135 Inf     0.747     0.799
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields, presence_notes 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 

#check contrast values in response scale for reference
contrast(emm_tags_resp , method = "revpairwise",  weights = "cells", type = "response")

#  contrast        odds.ratio    SE  df null z.ratio p.value
#  Ntags6 / Ntags0       1.89 0.073 Inf    1  16.463  <.0001
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields, presence_notes 
# Tests are performed on the log odds ratio scale 

emm_tags_resp <- as.data.frame(emm_tags_resp)

### PLOTTING

#Since emmtrends was significant I just added line and ribbon (instead of segments)
p2 <- ggplot(emm_tags_resp, aes(x = Ntags, y = prob)) +
        geom_hline(aes(yintercept=overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_line(colour = "#08519C", linewidth = 0.6) +
        geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                    alpha = 0.2, , fill = "#6BAED6") +
        ylim(0.25,1) +
        labs(y="RG Proportion", 
             x="Number of Tags") +
        cowplot::theme_cowplot() 

#### PHOTOS ----

## CALCULATION

#logit EMMS
emm_Nph <- emmeans(fit_final, ~ Nph,
                   weights = "cells",
                   nesting = NULL)

#  Nph  emmean     SE  df asymp.LCL asymp.UCL
#  1     0.498 0.0669 Inf     0.367     0.630
#  2     0.662 0.0670 Inf     0.531     0.794
#  3     0.746 0.0671 Inf     0.615     0.878
#  4     0.805 0.0673 Inf     0.673     0.937
#  5-10  0.855 0.0675 Inf     0.723     0.988
#  >10   1.150 0.0779 Inf     0.997     1.303
# 
# Results are averaged over the levels of: presence_projects, presence_obs_fields, presence_notes 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

#logit contrasts
sig_Nph <- contrast(emm_Nph, "consec", weights = "cells") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#       contrast   estimate          SE  df   z.ratio      p.value dir
# 1        2 - 1 0.16378303 0.005077478 Inf 32.256770 0.000000e+00   +
# 2        3 - 2 0.08408739 0.007102793 Inf 11.838638 0.000000e+00   +
# 3        4 - 3 0.05858708 0.010206969 Inf  5.739909 4.237462e-08   +
# 4   (5-10) - 4 0.05052658 0.012807971 Inf  3.944932 3.630267e-04   +
# 5 >10 - (5-10) 0.29465967 0.041431145 Inf  7.112033 3.650968e-12   +

# EMMs response
emm_Nph_resp <- emmeans(fit_final, ~ Nph,
                   weights = "cells",
                   nesting = NULL, type = "response")

#  Nph   prob     SE  df asymp.LCL asymp.UCL
#  1    0.622 0.0157 Inf     0.591     0.652
#  2    0.660 0.0150 Inf     0.630     0.689
#  3    0.678 0.0146 Inf     0.649     0.706
#  4    0.691 0.0144 Inf     0.662     0.718
#  5-10 0.702 0.0141 Inf     0.673     0.729
#  >10  0.760 0.0142 Inf     0.731     0.786
# 
# Results are averaged over the levels of: presence_projects, presence_obs_fields, presence_notes 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 

#contrasts response
contrast(emm_Nph_resp, "consec", weights = "cells", type = "response") 

#  contrast     odds.ratio      SE  df null z.ratio p.value
#  2 / 1              1.18 0.00598 Inf    1  32.257  <.0001
#  3 / 2              1.09 0.00773 Inf    1  11.839  <.0001
#  4 / 3              1.06 0.01080 Inf    1   5.740  <.0001
#  (5-10) / 4         1.05 0.01350 Inf    1   3.945  0.0004
#  >10 / (5-10)       1.34 0.05560 Inf    1   7.112  <.0001
# 
# Results are averaged over the levels of: presence_projects, presence_obs_fields, presence_notes 
# P value adjustment: mvt method for 5 tests 
# Tests are performed on the log odds ratio scale 

emm_Nph_resp <- as.data.frame(emm_Nph_resp)

##PLOTTING

segments<- emm_Nph_resp %>%
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
                sig_Nph %>% select(contrast, p.value, dir), #add significance from contrast in log odds
                by = "contrast") %>% 
        mutate(linetype = ifelse(dir == "n.s.", "n.s.", "sig."))

p3<- ggplot(emm_Nph_resp, aes(x = Nph, y = prob)) +
        geom_hline(aes(yintercept=overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point(size = 0.8, colour = "#08519C") +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.06,
                      linewidth = 0.6, colour = "#08519C") +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.6, colour = "#08519C")+
        ylim(0.25,1) +
        labs(y="RG Proportion", 
             x="Number of Photos (binned)",
             linetype = "Significance (p ≤ 0.05)")+
        cowplot::theme_cowplot() +
        theme(legend.position = "bottom")
