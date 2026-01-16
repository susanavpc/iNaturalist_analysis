library(tidyverse)
library(ggsankey)

tidy_data <- read.csv("data/tidy_data/tidy_data_verifiable.csv")

tidy_data <- tidy_data %>% 
        mutate(quality_sp_level = ifelse(quality_grade == "research" & specificEpithet == "Not identified", "needs_id", quality_grade),
               export = if_else( license_code %in% c("cc0", "cc-by-nc", "cc-by") & quality_sp_level  == "research", "GBIF Export","No Export"))


#classify ranks as IDed or not per observation     
binary_ranks <- tidy_data %>% 
        select(id,uri, kingdom, phylum, class, order, family, genus, specificEpithet, quality_sp_level, export) %>% 
        mutate(across(kingdom:specificEpithet, ~ ifelse(.x == "Not identified", "No ID", "ID"))) %>% 
        rename(grade = quality_sp_level,
               species = specificEpithet) 

rm(tidy_data)

#ggsankey function - determines the order of your "columns" in the flow so it can map each step (flow direction)
df <- binary_ranks %>%
        make_long(kingdom, phylum, class, order, family, genus, species, grade, export)


#change value names
df <- df %>%
        mutate(across(c(node, next_node), ~ ifelse(. == "needs_id", "Needs ID",
                                        ifelse(. == "research", "Research", .))))


#change order categories appear in (vertical order)
df$node <- factor(df$node,levels = c("No ID", "ID", "Needs ID", "Research", "No Export", "GBIF Export"))
df$next_node <- factor(df$next_node,levels = c("No ID", "ID", "Needs ID", "Research", "No Export", "GBIF Export"))

#plot
sankey<- ggplot(df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
        geom_sankey(flow.alpha = 0.5, node.color = 1) +
        geom_sankey_label(size = 4.5, color = 1, fill = "white") +
        scale_fill_manual(values = c("ID"= "lightskyblue", "No ID" = "lightcoral", "Research" = "olivedrab3", "Needs ID" = "lightgoldenrod1", "GBIF Export" = "darkseagreen3", "No Export" = "#FFB08C"))+
        theme_sankey(base_size = 16) +
        theme(legend.position = "none")+
         xlab(NULL)


ggsave("figs/sankey_gbif_final.png", plot = sankey, dpi = 400)
binary_ranks %>% 
        count(export)
