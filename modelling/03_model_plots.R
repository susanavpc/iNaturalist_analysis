library(tidyverse)
library(emmeans)
library(RColorBrewer)
library(glmmTMB)
library(cowplot)

load(file = "modelling/model_final.RData")
fit_final <- fit_no_tags_inter_genus_chr_month; rm(fit_no_tags_inter_genus_chr_month)

load(file = "data/tidy_data/data_30perc.RData")

#actual proportion of RG in dataset
prop_RG <- mean(data_30_test_month$rg == 1)
#0.6540829

#overall expected probability based on emmeans grid
overall_emm <- emmeans(fit_final,~1, weights = "cells", type = "response") %>% 
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


### Plots: Projects ----

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


p5 <- ggplot(emm_proj, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_proj))) +
        geom_hline(aes(yintercept= overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point(size= 0.8) +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.02,
                      linewidth = 0.6) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.6)+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0.25,1) +
        labs(y="RG Proportion", 
             x="Presence of Projects",
             colour = "Amount of other info.",
             linetype = "Significance (p ≤ 0.05)")+
        cowplot::theme_cowplot() +
        scale_color_manual(values = colours) +
        theme(legend.position = "bottom")



### Plots: obsf ----
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


p4 <- ggplot(emm_obsf, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_obsf))) +
        geom_hline(aes(yintercept= overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point(size = 0.8) +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.02,
                      linewidth = 0.6) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.6)+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0.25,1) +
        labs(y="RG Proportion", 
             x="Presence of Observation Fields",
             colour = "Amount of other info.",
             linetype = "Significance (p ≤ 0.05)")+
        cowplot::theme_cowplot() +
        theme(legend.position = "bottom")+
        scale_color_manual(values = colours)

### Plots: notes ----

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


p6<- ggplot(emm_notes, aes(x = presence_dodged, y = prob, colour = factor(n_info_no_notes))) +
        geom_hline(aes(yintercept=overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_point(size = 0.8) +
        geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                      width = 0.02,
                      linewidth = 0.6) +
        geom_segment(data = segments,
                     aes(x = x, xend = xend, y = y, yend = yend, linetype = linetype),
                     linewidth = 0.6)+
        scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))+
        ylim(0.25,1) +
        labs(y="RG Proportion", 
             x="Presence of Notes",
             colour = "Amount of other info.",
             linetype = "Significance (p ≤ 0.05)")+
        cowplot::theme_cowplot() +
        scale_color_manual(values = colours) +
        theme(legend.position = "bottom")

### Plots: logysu ----


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
p1 <- ggplot(emm_logysu, aes(x = log_ysu, y = prob)) +
        geom_hline(aes(yintercept=overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_line(colour = "#08519C", linewidth = 0.6) +
        geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                    alpha = 0.2, fill = "#6BAED6") +
        ylim(0.25,1) +
        labs(y="RG Proportion", 
             x="Years since upload (log10)") +
        cowplot::theme_cowplot()

### Plots: Ntags ----

emm_Ntags <- as.data.frame(
        emmeans(fit_final, "Ntags", 
                at = list(Ntags=seq(min(data_30_test_month$Ntags),
                                    max(data_30_test_month$Ntags), by=0.1)),
                weights = "cells",
                type="response",
                nesting = NULL))


sig_Ntags <- emtrends(fit_final, specs = ~ 1, var = "Ntags", weights = "cells") %>%  #specs specifies no grouping
        test() %>% 
        as.data.frame() %>%
        mutate(dir = case_when(
                p.value>0.05~"n.s.",
                Ntags.trend>0~"+",
                TRUE~"-"
        ))

#         1 Ntags.trend          SE  df  z.ratio      p.value dir
# 1 overall   0.1060306 0.006440545 Inf 16.46298 6.768425e-61   +


#Since emmtrends was significant I just added line and ribbon (instead of segments)
p2 <- ggplot(emm_Ntags, aes(x = Ntags, y = prob)) +
        geom_hline(aes(yintercept=overall_emm), 
                   colour="gray80", linewidth=0.4, linetype="longdash")+
        geom_line(colour = "#08519C", linewidth = 0.6) +
        geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                    alpha = 0.2, , fill = "#6BAED6") +
        ylim(0.25,1) +
        labs(y="RG Proportion", 
             x="Number of Tags") +
        cowplot::theme_cowplot() 



### Plots: Nph ----

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


p3<- ggplot(emm_Nph, aes(x = Nph, y = prob)) +
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
p5 <- p5 + theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.title = element_text(hjust = 0.5),         # updated syntax
        legend.spacing.x = unit(0.5, "cm"),                # space between keys
        legend.box.margin = margin(t = 5)   # top margin
)

legend <- get_legend_sub(p5)


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
