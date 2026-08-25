library(tidyverse)
library(cowplot)
library(scales)

df <- read_excel("data/iNat_data_comparison.xlsx", sheet = "Sheet1")

group_order <- c("Vertebrates","Invertebrates", "Plants", "Fungi" )

df<- df %>% 
        mutate(group = factor(group, levels = rev(group_order)))


## Panel A ----

coverage <- df %>%
        mutate(n_undocumented = global_n_species_described - global_n_species_iNat) %>%
        select(group, global_n_species_iNat, n_undocumented) %>%
        pivot_longer(c(global_n_species_iNat, n_undocumented), names_to = "status", values_to = "global_n_species") %>%
        mutate(status = factor(status, levels = c("n_undocumented", "global_n_species_iNat"),
                               labels = c("Not on iNaturalist", "On iNaturalist")))

p_coverage <- ggplot(coverage, aes(x = group, y = global_n_species, fill = status)) +
        geom_col(width = 0.7) +
        coord_flip() +
        scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.02))) +
        scale_fill_manual(values = c("On iNaturalist" = "#2C5F8A",
                                     "Not on iNaturalist" = "grey85")) +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(x = NULL, y = "Number of species", fill = NULL, title = "Species Coverage (Global)") +
        theme_cowplot(font_size = 14) +
        theme(legend.position = "bottom")+
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 12)),
              legend.position = "bottom", legend.justification = "center",
              plot.title = element_text(size = 13, margin = margin(b = 13)))

## Panel B ----

rg_prop <- df %>%
        mutate(
                prop_rg = RG / europe_n_obs_iNat,
                prop_not_rg = 1 - prop_rg
        ) %>%
        select(group, prop_rg, prop_not_rg) %>%
        pivot_longer(c(prop_rg, prop_not_rg), names_to = "status", values_to = "proportion") %>%
        mutate(status = factor(status, levels = c("prop_not_rg", "prop_rg"),
                               labels = c("Other", "Research")))

p_rg <- ggplot(rg_prop, aes(x = group, y = proportion, fill = status)) +
        geom_col(width = 0.7) +
        coord_flip() +
        scale_y_continuous(
                limits = c(0,1),
                expand = expansion(mult = c(0, 0)),
                breaks = scales::breaks_extended(n = 5))+
        scale_fill_manual(values = c("Research" = "#a8cc08", "Other" = "grey85")) +
        guides(fill = guide_legend(reverse = TRUE)) +
        labs(x = NULL, y = "Research Grade Proportion", fill = NULL, title = "      RG Proportion (Europe)") +
        theme_cowplot(font_size = 14) +
        theme(axis.text.y = element_blank(), legend.position = "bottom")+
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 12)),
              legend.position = "bottom", legend.justification = "center",
              plot.title = element_text(size = 13, margin = margin(b = 13)))

## Panel C -----

quality_counts <- df %>%
        select(group, RG, NeedsID, Casual) %>%
        pivot_longer(c(RG, NeedsID, Casual), names_to = "quality_grade", values_to = "n") %>%
        mutate(quality_grade = factor(quality_grade, levels = c("RG", "NeedsID", "Casual"),
                                      labels = c("Research", "Needs ID", "Casual")))

p_quality <- ggplot(quality_counts, aes(x = group, y = n, fill = quality_grade)) +
        geom_col(width = 0.7, position = position_dodge2(preserve = "single")) +
        coord_flip() +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()), expand = expansion(mult = c(0, 0.05))) +
        scale_fill_manual(values = c(Research = "#a8cc08", `Needs ID` = "#ffee91", Casual = "#aaaaaa")) +
        labs(x = NULL, y = "Number of observations", fill = NULL, title = "      Quality Grade (Europe)") +
        theme_cowplot(font_size = 14) +
        theme(legend.position = "bottom", axis.text.y = element_blank())+
        theme(axis.title.x = element_text(margin = margin(t = 12)),
              axis.title.y = element_text(margin = margin(r = 12)),
              legend.position = "bottom", legend.justification = "center",
              plot.title = element_text(size = 13, margin = margin(b = 13)))

## Combine, rows aligned across all three panels -----------------------------

aligned <- align_plots(p_coverage, p_rg, p_quality, align = "h", axis = "tb")
plot_grid(aligned[[1]], aligned[[2]], aligned[[3]], ncol = 3, rel_widths = c(1.2, 1,1))


# top_row <- plot_grid(p_coverage, p_rg, ncol = 2, rel_widths = c(1.2, 1), align = "h", axis = "tb")
# 
# plot_grid(top_row, p_quality, ncol = 1, rel_heights = c(1, 1))
