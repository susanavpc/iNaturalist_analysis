library(tidyverse)
library(emmeans)
library(glmmTMB)

load(file = "modelling/model_final.RData")
fit_final <- fit_no_tags_inter_genus_chr_month; rm(fit_no_tags_inter_genus_chr_month)

load(file = "data/tidy_data/data_30perc.RData")


#actual proportion of RG in dataset
prop_RG <- mean(data_30_test_month$rg == 1)
#0.6540829

#overall expected probability based on emmeans grid
overall_emm <- emmeans(fit_final,~1, weights = "cells", type = "response", re_formula = NA) %>% 
        as.data.frame() %>% 
        pull(prob[1])
#0.6456797

#model intercept in probability scale        
logit2prob <- function(logit){
        odds <- exp(logit)
        prob <- odds / (1 + odds)
        return(prob)
}

logit2prob(0.018894) #value from summary table
#0.5047234

#prepare for plotting
dodge <- 0.15 #value for jittering points and segments in plot
n_groups <- length(unique(data_30_test_month$n_info_no_proj))
offsets <- seq(-dodge / 2, dodge / 2, length.out = n_groups) # value offset to either left and right for each value

#set up palette
colours <- brewer.pal(7, "Blues")[4:7]


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
        labs(y="RG Probability", 
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
        labs(y="RG Probability", 
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
        scale_linetype_manual(
                values = c("sig." = "solid", "n.s." = "dashed"))+
        ylim(0.25,1) +
        labs(y="RG Probability", 
             x="Number of Photos (binned)",
             linetype = "Significance (p ≤ 0.05)")+
        cowplot::theme_cowplot() +
        theme(legend.position = "bottom")

#### OBS FIELDS ----

## CALCULATION

#logit EMMS
emm_obsf <- emmeans(fit_final, ~ presence_obs_fields * n_info_no_obsf,
                at = list(n_info_no_obsf = c(0,1,2,3)),
                weights = "cells",
                nesting = NULL)

# presence_obs_fields n_info_no_obsf emmean     SE  df asymp.LCL asymp.UCL
#                    0              0  0.576 0.0669 Inf     0.445     0.707
#                    1              0  1.546 0.0700 Inf     1.409     1.684
#                    0              1  0.576 0.0669 Inf     0.445     0.707
#                    1              1  1.223 0.0696 Inf     1.087     1.360
#                    0              2  0.576 0.0669 Inf     0.445     0.707
#                    1              2  0.900 0.0778 Inf     0.747     1.052
#                    0              3  0.576 0.0669 Inf     0.445     0.707
#                    1              3  0.576 0.0922 Inf     0.395     0.757
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_notes 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

#logit contrasts
sig_obsf <- contrast(emm_obsf, method = "revpairwise", weights = "cells", by= "n_info_no_obsf") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#                                      contrast n_info_no_obsf     estimate         SE  df      z.ratio       p.value  dir
# 1 presence_obs_fields1 - presence_obs_fields0              0 0.9707403461 0.02140384 Inf 45.353552804  0.000000e+00    +
# 2 presence_obs_fields1 - presence_obs_fields0              1 0.6473169795 0.01995101 Inf 32.445322015 6.305365e-231    +
# 3 presence_obs_fields1 - presence_obs_fields0              2 0.3238936128 0.03990875 Inf  8.115854026  4.823803e-16    +
# 4 presence_obs_fields1 - presence_obs_fields0              3 0.0004702462 0.06357768 Inf  0.007396404  9.940986e-01 n.s.

#EMMs response
emm_obsf_resp <- emmeans(fit_final, ~ presence_obs_fields * n_info_no_obsf,
                    at = list(n_info_no_obsf = c(0,1,2,3)),
                    weights = "cells",
                    nesting = NULL, type = "response")

#  presence_obs_fields n_info_no_obsf  prob     SE  df asymp.LCL asymp.UCL
#                    0              0 0.640 0.0154 Inf     0.609     0.670
#                    1              0 0.824 0.0101 Inf     0.804     0.843
#                    0              1 0.640 0.0154 Inf     0.609     0.670
#                    1              1 0.773 0.0122 Inf     0.748     0.796
#                    0              2 0.640 0.0154 Inf     0.609     0.670
#                    1              2 0.711 0.0160 Inf     0.679     0.741
#                    0              3 0.640 0.0154 Inf     0.609     0.670
#                    1              3 0.640 0.0212 Inf     0.598     0.681
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_notes 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 

#contrasts response
contrast(emm_obsf_resp, method = "revpairwise", weights = "cells", by= "n_info_no_obsf", type = "response") 


# n_info_no_obsf = 0:
#  contrast                                    odds.ratio     SE  df null z.ratio p.value
#  presence_obs_fields1 / presence_obs_fields0       2.64 0.0565 Inf    1  45.354  <.0001
# 
# n_info_no_obsf = 1:
#  contrast                                    odds.ratio     SE  df null z.ratio p.value
#  presence_obs_fields1 / presence_obs_fields0       1.91 0.0381 Inf    1  32.445  <.0001
# 
# n_info_no_obsf = 2:
#  contrast                                    odds.ratio     SE  df null z.ratio p.value
#  presence_obs_fields1 / presence_obs_fields0       1.38 0.0552 Inf    1   8.116  <.0001
# 
# n_info_no_obsf = 3:
#  contrast                                    odds.ratio     SE  df null z.ratio p.value
#  presence_obs_fields1 / presence_obs_fields0       1.00 0.0636 Inf    1   0.007  0.9941


## PLOTTING

emm_obsf_resp <- as.data.frame(emm_obsf_resp)

emm_obsf_resp <- emm_obsf_resp %>%
        left_join(
                sig_obsf %>% select(n_info_no_obsf, p.value, dir), 
                by = "n_info_no_obsf") %>% 
        mutate(
                offset = offsets[as.integer(factor(n_info_no_obsf))], #get corresponding offsets based on index 
                presence_dodged = presence_obs_fields + offset
        )

segments <- emm_obsf_resp %>%
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


p4 <- ggplot(emm_obsf_resp, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_obsf))) +
        geom_hline(aes(yintercept= overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point(size = 0.8) +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.02,
                      linewidth = 0.6) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.6)+
        scale_linetype_manual(
                values = c("sig." = "solid", "n.s." = "dashed"))+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0.25,1) +
        labs(y="RG Probability", 
             x="Presence of Observation Fields",
             colour = "No. other elements",
             linetype = "Significance (p ≤ 0.05)")+
        cowplot::theme_cowplot() +
        theme(legend.position = "bottom")+
        scale_color_manual(values = colours)



#### PROJECTS ----

## CALCULATION

#logit EMMS
emm_proj <-emmeans(fit_final, ~ presence_projects * n_info_no_proj,
                at = list(n_info_no_proj = c(0,1,2,3)),
                weights = "cells",
                nesting = NULL)

#  presence_projects n_info_no_proj emmean     SE  df asymp.LCL asymp.UCL
#                  0              0   0.57 0.0669 Inf     0.439     0.701
#                  1              0   1.08 0.0679 Inf     0.950     1.216
#                  0              1   0.57 0.0669 Inf     0.439     0.701
#                  1              1   1.28 0.0696 Inf     1.143     1.416
#                  0              2   0.57 0.0669 Inf     0.439     0.701
#                  1              2   1.48 0.0775 Inf     1.324     1.628
#                  0              3   0.57 0.0669 Inf     0.439     0.701
#                  1              3   1.67 0.0900 Inf     1.496     1.849
# 
# Results are averaged over the levels of: Nph, presence_obs_fields, presence_notes 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

#logit contrasts
sig_proj <- contrast(emm_proj, method = "revpairwise", weights = "cells", by= "n_info_no_proj") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#                                  contrast n_info_no_proj  estimate         SE  df  z.ratio       p.value dir
# 1 presence_projects1 - presence_projects0              0 0.5122117 0.01216476 Inf 42.10618  0.000000e+00   +
# 2 presence_projects1 - presence_projects0              1 0.7089066 0.01979434 Inf 35.81360 6.784580e-281   +
# 3 presence_projects1 - presence_projects0              2 0.9056016 0.03941743 Inf 22.97465 8.357522e-117   +
# 4 presence_projects1 - presence_projects0              3 1.1022965 0.06028047 Inf 18.28613  1.067268e-74   +


#EMMs response

emm_proj_resp <-emmeans(fit_final, ~ presence_projects * n_info_no_proj,
                   at = list(n_info_no_proj = c(0,1,2,3)),
                   weights = "cells",
                   nesting = NULL, type = "response")

#  presence_projects n_info_no_proj  prob     SE  df asymp.LCL asymp.UCL
#                  0              0 0.639 0.0154 Inf     0.608     0.669
#                  1              0 0.747 0.0128 Inf     0.721     0.771
#                  0              1 0.639 0.0154 Inf     0.608     0.669
#                  1              1 0.782 0.0119 Inf     0.758     0.805
#                  0              2 0.639 0.0154 Inf     0.608     0.669
#                  1              2 0.814 0.0117 Inf     0.790     0.836
#                  0              3 0.639 0.0154 Inf     0.608     0.669
#                  1              3 0.842 0.0120 Inf     0.817     0.864
# 
# Results are averaged over the levels of: Nph, presence_obs_fields, presence_notes 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 

#contrasts response
contrast(emm_proj_resp, method = "revpairwise", weights = "cells", by= "n_info_no_proj", type = "response")

# n_info_no_proj = 0:
#  contrast                                odds.ratio     SE  df null z.ratio p.value
#  presence_projects1 / presence_projects0       1.67 0.0203 Inf    1  42.106  <.0001
# 
# n_info_no_proj = 1:
#  contrast                                odds.ratio     SE  df null z.ratio p.value
#  presence_projects1 / presence_projects0       2.03 0.0402 Inf    1  35.814  <.0001
# 
# n_info_no_proj = 2:
#  contrast                                odds.ratio     SE  df null z.ratio p.value
#  presence_projects1 / presence_projects0       2.47 0.0975 Inf    1  22.975  <.0001
# 
# n_info_no_proj = 3:
#  contrast                                odds.ratio     SE  df null z.ratio p.value
#  presence_projects1 / presence_projects0       3.01 0.1820 Inf    1  18.286  <.0001
# 
# Results are averaged over the levels of: Nph, presence_obs_fields, presence_notes 
# Tests are performed on the log odds ratio scale 


## PLOTTING

emm_proj_resp <- as.data.frame(emm_proj_resp)


emm_proj_resp <- emm_proj_resp %>%
        left_join(
                sig_proj %>% select(n_info_no_proj, p.value, dir),  #join significance from emmtrends
                by = "n_info_no_proj") %>% 
        mutate(
                offset = offsets[as.integer(factor(n_info_no_proj))], #get corresponding plot points offsets based on index numbers
                presence_dodged = presence_projects + offset
        )

segments <- emm_proj_resp %>%
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


p5 <- ggplot(emm_proj_resp, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_proj))) +
        geom_hline(aes(yintercept= overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point(size= 0.8) +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.02,
                      linewidth = 0.6) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.6)+
        scale_linetype_manual(
                values = c("sig." = "solid", "n.s." = "dashed"))+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0.25,1) +
        labs(y="RG Probability", 
             x="Presence of Projects",
             colour = "No. other elements",
             linetype = "Significance (p ≤ 0.05)")+
        cowplot::theme_cowplot() +
        scale_color_manual(values = colours) +
        theme(legend.position = "bottom")

#### NOTES ----

#logit EMMS
emm_notes <-emmeans(fit_final, ~ presence_notes * n_info_no_notes,
                at = list(n_info_no_notes = c(0,1,2,3)),
                weights = "cells",
                nesting = NULL)

#  presence_notes n_info_no_notes  emmean     SE  df asymp.LCL asymp.UCL
#               0               0  0.5576 0.0669 Inf    0.4266     0.689
#               1               0  0.9814 0.0671 Inf    0.8498     1.113
#               0               1  0.5576 0.0669 Inf    0.4266     0.689
#               1               1  0.5406 0.0692 Inf    0.4050     0.676
#               0               2  0.5576 0.0669 Inf    0.4266     0.689
#               1               2  0.0999 0.0763 Inf   -0.0496     0.249
#               0               3  0.5576 0.0669 Inf    0.4266     0.689
#               1               3 -0.3409 0.0871 Inf   -0.5116    -0.170
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields 
# Results are given on the logit (not the response) scale. 
# Confidence level used: 0.95 

#logit contrast

sig_notes <- contrast(emm_notes , method = "revpairwise", weights = "cells", by= "n_info_no_notes") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#                            contrast n_info_no_notes    estimate          SE  df     z.ratio      p.value  dir
# 1 presence_notes1 - presence_notes0               0  0.42372991 0.007383899 Inf  57.3856590 0.000000e+00    +
# 2 presence_notes1 - presence_notes0               1 -0.01702437 0.018474134 Inf  -0.9215245 3.567766e-01 n.s.
# 3 presence_notes1 - presence_notes0               2 -0.45777865 0.037027292 Inf -12.3632766 4.129736e-35    -
# 4 presence_notes1 - presence_notes0               3 -0.89853293 0.056068726 Inf -16.0255635 8.471935e-58    -

#EMMS response
emm_notes_resp <-emmeans(fit_final, ~ presence_notes * n_info_no_notes,
                    at = list(n_info_no_notes = c(0,1,2,3)),
                    type="response",
                    weights = "cells",
                    nesting = NULL)

# presence_notes n_info_no_notes  prob     SE  df asymp.LCL asymp.UCL
#               0               0 0.636 0.0155 Inf     0.605     0.666
#               1               0 0.727 0.0133 Inf     0.701     0.753
#               0               1 0.636 0.0155 Inf     0.605     0.666
#               1               1 0.632 0.0161 Inf     0.600     0.663
#               0               2 0.636 0.0155 Inf     0.605     0.666
#               1               2 0.525 0.0190 Inf     0.488     0.562
#               0               3 0.636 0.0155 Inf     0.605     0.666
#               1               3 0.416 0.0212 Inf     0.375     0.458
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 

# contrast resp
contrast(emm_notes_resp , method = "revpairwise", weights = "cells", by= "n_info_no_notes", type = "response")

# n_info_no_notes = 0:
#  contrast                          odds.ratio     SE  df null z.ratio p.value
#  presence_notes1 / presence_notes0      1.528 0.0113 Inf    1  57.386  <.0001
# 
# n_info_no_notes = 1:
#  contrast                          odds.ratio     SE  df null z.ratio p.value
#  presence_notes1 / presence_notes0      0.983 0.0182 Inf    1  -0.922  0.3568
# 
# n_info_no_notes = 2:
#  contrast                          odds.ratio     SE  df null z.ratio p.value
#  presence_notes1 / presence_notes0      0.633 0.0234 Inf    1 -12.363  <.0001
# 
# n_info_no_notes = 3:
#  contrast                          odds.ratio     SE  df null z.ratio p.value
#  presence_notes1 / presence_notes0      0.407 0.0228 Inf    1 -16.026  <.0001
# 
# Results are averaged over the levels of: Nph, presence_projects, presence_obs_fields 
# Tests are performed on the log odds ratio scale


## PLOTTING

emm_notes_resp <- as.data.frame(emm_notes_resp)


emm_notes_resp <- emm_notes_resp %>%
        left_join(
                sig_notes %>% select(n_info_no_notes, p.value, dir), #add significance from emmtrends
                by = "n_info_no_notes") %>% 
        mutate(
                offset = offsets[as.integer(factor(n_info_no_notes))], #get corresponding offsets based on index 
                presence_dodged = presence_notes + offset
        )

segments <- emm_notes_resp %>%
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


p6<- ggplot(emm_notes_resp, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_notes))) +
        geom_hline(aes(yintercept=overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point(size = 0.8) +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.02,
                      linewidth = 0.6) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.6)+
        scale_linetype_manual(
                values = c("sig." = "solid", "n.s." = "dashed"))+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0.25,1) +
        labs(y="RG Probability", 
             x="Presence of Notes",
             colour = "No. other elements",
             linetype = "Significance (p ≤ 0.05)")+
        cowplot::theme_cowplot() +
        scale_color_manual(values = colours) +
        theme(legend.position = "bottom")

###Make grid ----

# original cowplot get_legend not working - using function suggestion from github
get_legend_sub <- function(plot, legend = NULL) {
        
        gt <- ggplotGrob(plot)
        
        pattern <- "guide-box"
        if (!is.null(legend)) {
                pattern <- paste0(pattern, "-", legend)
        }
        
        indices <- grep(pattern, gt$layout$name)
        
        not_empty <- !vapply(
                gt$grobs[indices], 
                inherits, what = "zeroGrob", 
                FUN.VALUE = logical(1)
        )
        indices <- indices[not_empty]
        
        if (length(indices) > 0) {
                return(gt$grobs[[indices[1]]])
        }
        return(NULL)
}

#get legend from p5, making sure it's at the bottom 
p4 <- p4 + theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = element_text(hjust = 0.5),         # updated syntax
        legend.spacing.x = unit(0.5, "cm"),                # space between keys
        legend.box.margin = margin(t = 5)   # top margin
)

legend <- get_legend_sub(p4)


#update plots to not have legend
p1 <- p1 + theme_cowplot(font_size = 13) + theme(legend.position="none")
p2 <- p2 + theme_cowplot(font_size = 13) + theme(legend.position="none")
p3 <- p3 + theme_cowplot(font_size = 13) + theme(legend.position="none")
p4 <- p4 + theme_cowplot(font_size = 13) + theme(legend.position="none")
p5 <- p5 + theme_cowplot(font_size = 13) + theme(legend.position="none")
p6 <- p6 + theme_cowplot(font_size = 13) + theme(legend.position="none")

#create grid of plots
grid <- cowplot::plot_grid(plotlist = list(p1, p2, p3, p4, p5, p6),
                           labels = "auto",
                           label_size = 13, 
                           ncol =2)

#add legend from plot 5
plot_grid(grid, legend, ncol = 1, rel_heights = c(1, .1))

#change to capital letters
grid <- cowplot::plot_grid(plotlist = list(p1, p2, p3, p4, p5, p6),
                           ncol =2)
plot_grid(grid, legend, ncol = 1, rel_heights = c(1, .1))

