library(tidyverse)
library(lme4)
library(ggeffects)
library(ggplot2)

taxonomy_data <- read.delim("D:/meta/05_final_tables/merged-data_taxonomy/metadata.tsv", 
                            header = TRUE, 
                            row.names = NULL, 
                            sep = "\t", 
                            check.names = FALSE)

taxonomy_data <- taxonomy_data[-1, ]
metadata <- read.csv("D:/meta/meta-soil-fungi-variables.csv")
lifestyles <- read_tsv("D:/meta/fungi_db/FungalTraits_S1.txt")

# 1. Extract Genus
taxonomy_data <- taxonomy_data %>%
  mutate(Genus = str_extract(Taxon, "g__[^;]+") %>% str_replace("g__", ""))

# 2. Reshape to long format
long_df <- taxonomy_data %>%
  pivot_longer(
    cols = -c(Taxon, Confidence, Genus),
    names_to = "id",
    values_to = "Abundance"
  )

# 3. Convert to numeric
long_df <- long_df %>%
  mutate(Abundance = as.numeric(Abundance))

# 4. Group by Sample and Genus → sum abundance
long_grouped <- long_df %>%
  group_by(id, Genus) %>%
  summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop")

# 5. Calculate relative abundance per sample
long_relative <- long_grouped %>%
  group_by(id) %>%
  mutate(Relative_Abundance = Abundance / sum(Abundance, na.rm = TRUE)) %>%
  ungroup()

# Merge with FungalTraits and metadata
relative_merged <- long_relative %>%
  left_join(lifestyles, by = "Genus") %>%
  left_join(metadata, by = "id")

ectos <- relative_merged %>%
  filter(primary_lifestyle == "ectomycorrhizal")

mod <- lmer(def ~ Relative_Abundance + (1 | site), data = ectos)
summary(mod)

pred_df <- ggpredict(mod, terms = "Relative_Abundance")

ggplot(ectos, aes(x = Relative_Abundance, y = def)) +
  geom_point(aes(color = site), alpha = 0.7) +
  geom_line(data = pred_df, aes(x = x, y = predicted), color = "black", linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Effect of Ectomycorrhizal Abundance on Tree Defoliation",
    x = "Relative Abundance (Ectomycorrhiza)",
    y = "Defoliation (DEF)"
  )

