library(tidyverse)
library(emmeans)
library(glmmTMB)

load(file = "modelling/model_final.RData")
fit_final <- fit_no_tags_inter_genus_chr_month; rm(fit_no_tags_inter_genus_chr_month)

load(file = "data/tidy_data/data_30perc.RData")


### Contrasts: Photos ----

emm_Nph <- emmeans(fit_final, ~ Nph,
                   weights = "cells",
                   nesting = NULL)

emm_Nph_resp <-  

sig_Nph <- contrast(emm_Nph_resp, "consec", weights = "cells") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))
#       contrast   estimate          SE  df   z.ratio      p.value dir
# 1        2 - 1 0.03766647 0.001358230 Inf 27.732036 0.000000e+00   +
# 2        3 - 2 0.01861437 0.001620948 Inf 11.483636 0.000000e+00   +
# 3        4 - 3 0.01264670 0.002216887 Inf  5.704711 6.032177e-08   +
# 4   (5-10) - 4 0.01068242 0.002719555 Inf  3.928003 4.321852e-04   +
# 5 >10 - (5-10) 0.05782134 0.007839163 Inf  7.375958 6.682432e-13   +


#contrast between min a max (1 and over 10)

emm_Nph_min_max <- emmeans(fit_final, "Nph", at = list(Nph = c("1",">10" )),
                       weights = "cells")

emm_Nph_min_max_resp <-  regrid(emm_Nph_min_max, transform = "response")

sig_Nph_min_max <- contrast(emm_Nph_min_max_resp , method = "revpairwise",  weights = "cells") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#   contrast  estimate          SE  df z.ratio      p.value dir
# 1  >10 - 1 0.1374313 0.008219967 Inf 16.7192 9.497951e-63   +
### Contrasts: Projects ----

emm_proj <- emmeans(fit_final, ~ presence_projects * n_info_no_proj,
                at = list(n_info_no_proj = c(0,1,2,3)),
                type="response",
                weights = "cells",
                nesting = NULL)

emm_proj_resp <- regrid(emm_proj, transform = "response")

sig_proj <- contrast(emm_proj_resp, method = "revpairwise", weights = "cells", by= "n_info_no_proj") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#                                  contrast n_info_no_proj  estimate          SE  df  z.ratio       p.value dir
# 1 presence_projects1 - presence_projects0              0 0.1081308 0.003629808 Inf 29.78968 5.315586e-195   +
# 2 presence_projects1 - presence_projects0              1 0.1434739 0.005274238 Inf 27.20277 6.023006e-163   +
# 3 presence_projects1 - presence_projects0              2 0.1751093 0.007992117 Inf 21.91025 2.074328e-106   +
# 4 presence_projects1 - presence_projects0              3 0.2030757 0.010349371 Inf 19.62203  1.002607e-85   +

### Contrasts: Observation fields ----

emm_obsf <- emmeans(fit_final, ~ presence_obs_fields * n_info_no_obsf,
                at = list(n_info_no_obsf = c(0,1,2,3)),
                type="response",
                weights = "cells",
                nesting = NULL)

emm_obsf_resp <- regrid(emm_obsf, transform = "response")

sig_obsf <- contrast(emm_obsf_resp, method = "revpairwise", weights = "cells", by= "n_info_no_obsf") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#                                      contrast n_info_no_obsf     estimate          SE  df      z.ratio       p.value  dir
# 1 presence_obs_fields1 - presence_obs_fields0              0 0.1843191544 0.006531882 Inf 28.218385632 3.478950e-175    +
# 2 presence_obs_fields1 - presence_obs_fields0              1 0.1325150840 0.005082073 Inf 26.075008328 7.004132e-150    +
# 3 presence_obs_fields1 - presence_obs_fields0              2 0.0707878856 0.008372641 Inf  8.454666404  2.798864e-17    +
# 4 presence_obs_fields1 - presence_obs_fields0              3 0.0001083265 0.014644872 Inf  0.007396891  9.940982e-01 n.s.

### Contrasts: notes ----

emm_notes <- emmeans(fit_final, ~ presence_notes * n_info_no_notes,
                at = list(n_info_no_notes = c(0,1,2,3)),
                type="response",
                weights = "cells",
                nesting = NULL)

emm_notes_resp <- regrid(emm_notes, transform = "response")

sig_notes <- contrast(emm_notes_resp, method = "revpairwise", weights = "cells", by= "n_info_no_notes") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#                            contrast n_info_no_notes     estimate          SE  df     z.ratio       p.value  dir
# 1 presence_notes1 - presence_notes0               0  0.091473331 0.002689718 Inf  34.0085168 1.667216e-253    +
# 2 presence_notes1 - presence_notes0               1 -0.003950678 0.004296688 Inf  -0.9194707  3.578494e-01 n.s.
# 3 presence_notes1 - presence_notes0               2 -0.110961525 0.009303743 Inf -11.9265469  8.607022e-33    -
# 4 presence_notes1 - presence_notes0               3 -0.220313609 0.013635672 Inf -16.1571502  1.011298e-58    -

### Contrasts: tags - for min and max ----


emm_tags <- emmeans(fit_final, ~ Ntags, at = list(Ntags = c(min(data_30_test_month$Ntags),
                                                            max(data_30_test_month$Ntags))),
                    weights = "cells")

emm_tags_resp <-  regrid(emm_tags, transform = "response")

sig_tags <- contrast(emm_tags_resp , method = "revpairwise",  weights = "cells") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#          contrast  estimate          SE  df  z.ratio      p.value dir
# 1 Ntags6 - Ntags0 0.1294762 0.007679426 Inf 16.86015 8.837115e-64   +


### Contrasts: time for min and max----

emm_log_ysu <- emmeans(fit_final, "log_ysu", at = list(log_ysu = c(min(data_30_test_month$log_ysu),
                                                            max(data_30_test_month$log_ysu))),
                    weights = "cells")

emm_log_ysu_resp <-  regrid(emm_log_ysu, transform = "response")

sig_log_ysu <- contrast(emm_log_ysu_resp , method = "revpairwise",  weights = "cells") %>% 
        as.data.frame() %>% 
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                estimate>0~"+",
                TRUE~"-"))

#                             contrast  estimate          SE  df  z.ratio       p.value dir
# 1 log_ysu1.20411998265592 - log_ysu0 0.2990058 0.008007593 Inf 37.34029 3.645191e-305   +
