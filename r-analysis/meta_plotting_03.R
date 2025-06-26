library(tidyverse)
library(ggplot2)
library(ggforce)
library(ggeffects)
library(vegan)
library(patchwork)
library(lme4)
library(dplyr)
library(tidyr)
library(broom.mixed)

# ---- Load in Data ----

taxonomy_data <- read.delim("D:/meta/05_final_tables/merged-data_taxonomy/metadata.tsv", 
                            header = TRUE, 
                            row.names = NULL, 
                            sep = "\t", 
                            check.names = FALSE)

taxonomy_data <- taxonomy_data[-1, ]
metadata <- read_tsv("D:/meta/metadata_group_2-reindexed.txt")
metadata <- metadata[-1,]

field_data <- read.csv("D:/meta/meta-soil-fungi-variables.csv")

# ---- Exract Taxonomy Levels ----

taxonomy_data <- taxonomy_data %>%
  mutate(Phylum = str_extract(Taxon, "p__[^;]+") %>% str_replace("p__", ""))

taxonomy_data <- taxonomy_data %>%
  mutate(Class = str_extract(Taxon, "c__[^;]+") %>% str_replace("c__", ""))

taxonomy_data <- taxonomy_data %>%
  mutate(Order = str_extract(Taxon, "o__[^;]+") %>% str_replace("o__", ""))

taxonomy_data <- taxonomy_data %>%
  mutate(Genus = str_extract(Taxon, "g__[^;]+") %>% str_replace("g__", ""))

# 4. Identify sample columns
non_numeric_cols <- c("id", "Taxon", "Confidence", "Phylum", "Class", "Order", "Genus")
sample_cols <- setdiff(names(taxonomy_data), non_numeric_cols)

# 5. Convert sample columns to numeric
taxonomy_data[sample_cols] <- lapply(taxonomy_data[sample_cols], as.numeric)

# 6. Transpose data to long format
long_data_phylum <- taxonomy_data %>%
  select(all_of(sample_cols), Phylum) %>%
  pivot_longer(cols = all_of(sample_cols), names_to = "SampleID", values_to = "Abundance")

long_data_class <- taxonomy_data %>%
  select(all_of(sample_cols), Class) %>%
  pivot_longer(cols = all_of(sample_cols), names_to = "SampleID", values_to = "Abundance")

long_data_order <- taxonomy_data %>%
  select(all_of(sample_cols), Order) %>%
  pivot_longer(cols = all_of(sample_cols), names_to = "SampleID", values_to = "Abundance")



colnames(long_data_order)[2] <- "id"
field_merged <- merge(long_data_order, field_data, by = "id")

long_data_genus <- taxonomy_data %>%
  select(all_of(sample_cols), Genus) %>%
  pivot_longer(cols = all_of(sample_cols), names_to = "SampleID", values_to = "Abundance")

# 7. Join with metadata to get tree health info
long_data_phylum <- long_data_phylum %>%
  left_join(metadata, by = c("SampleID" = "id"))

long_data_class <- long_data_class %>%
  left_join(metadata, by = c("SampleID" = "id"))

long_data_order <- long_data_order %>%
  left_join(metadata, by = c("SampleID" = "id"))

#long_data_genus <- long_data_genus %>%
#  left_join(metadata, by = c("SampleID" = "id"))

long_data_genus <- long_data_genus %>%
  left_join(field_data, by = c("SampleID" = "id"))

# 8. Group by Phylum and health status
phylum_health_sums <- long_data_phylum %>%
  group_by(Phylum, tree_health) %>%
  summarise(Total_Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop")

class_health_sums <- long_data_class %>%
  group_by(Class, tree_health) %>%
  summarise(Total_Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop")

order_health_sums <- long_data_order %>%
  group_by(Order, tree_health) %>%
  summarise(Total_Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop")

genus_health_sums <- long_data_genus %>%
  group_by(Genus, tree_health) %>%
  summarise(Total_Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop")

# 9. Plot
ggplot(phylum_health_sums, aes(x = reorder(Phylum, -Total_Abundance), y = Total_Abundance, fill = tree_health)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Phylum") +
  ylab("Total Abundance") +
  ggtitle("Total Abundance per Phylum by Tree Health") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("healthy" = "forestgreen", "unhealthy" = "firebrick"))
                    
ggplot(class_health_sums, aes(x = reorder(Class, -Total_Abundance), y = Total_Abundance, fill = tree_health)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Class") +
  ylab("Total Abundance") +
  ggtitle("Total Abundance per Class by Tree Health") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("healthy" = "forestgreen", "unhealthy" = "firebrick"))
                    
ggplot(order_health_sums, aes(x = reorder(Order, -Total_Abundance), y = Total_Abundance, fill = tree_health)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Order") +
  ylab("Total Abundance") +
  ggtitle("Total Abundance per Order by Tree Health") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("healthy" = "forestgreen", "unhealthy" = "firebrick"))

# ---- Phylum Plot -----

# Step 1: Normalize to relative abundance per tree_health group
phylum_relative <- phylum_health_sums %>%
  group_by(tree_health) %>%
  mutate(Relative_Abundance = 100 * Total_Abundance / sum(Total_Abundance))


# Step 4: Plot
ggplot(phylum_relative,
       aes(x = reorder(Phylum, -Relative_Abundance), y = Relative_Abundance, fill = tree_health)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Class") +
  ylab("Relative Abundance (%)") +
  ggtitle("Phylum by Relative Abundance and Tree Health") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("healthy" = "forestgreen", "unhealthy" = "firebrick"))

# ---- Classes Plot -----

# Step 1: Normalize to relative abundance per tree_health group
class_relative <- class_health_sums %>%
  group_by(tree_health) %>%
  mutate(Relative_Abundance = 100 * Total_Abundance / sum(Total_Abundance))

# Step 2: Get top 10 most abundant classes overall (summed across both health categories)
top10_classes <- class_relative %>%
  group_by(Class) %>%
  summarise(Total = sum(Total_Abundance)) %>%
  slice_max(Total, n = 10) %>%
  pull(Class)

# Step 3: Filter to keep only top 10 classes
class_relative_top10 <- class_relative %>%
  filter(Class %in% top10_classes)

# Step 4: Plot
ggplot(class_relative_top10,
       aes(x = reorder(Class, -Relative_Abundance), y = Relative_Abundance, fill = tree_health)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Class") +
  ylab("Relative Abundance (%)") +
  ggtitle("Top 10 Classes by Relative Abundance and Tree Health") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("healthy" = "forestgreen", "unhealthy" = "firebrick"))


# ---- Order Plot -----

# Step 1: Normalize to relative abundance per tree_health group
order_relative <- order_health_sums %>%
  group_by(tree_health) %>%
  mutate(Relative_Abundance = 100 * Total_Abundance / sum(Total_Abundance))

# Step 2: Get top 10 most abundant classes overall (summed across both health categories)
top20_order <- order_relative %>%
  group_by(Order) %>%
  summarise(Total = sum(Total_Abundance)) %>%
  slice_max(Total, n = 20) %>%
  pull(Order)

# Step 3: Filter to keep only top 10 classes
order_relative_top20 <- order_relative %>%
  filter(Order %in% top20_order)

# Step 4: Plot
ggplot(order_relative_top20,
       aes(x = reorder(Order, -Relative_Abundance), y = Relative_Abundance, fill = tree_health)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Order") +
  ylab("Relative Abundance (%)") +
  ggtitle("Top 20 Orders by Relative Abundance and Tree Health") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("healthy" = "forestgreen", "unhealthy" = "firebrick"))

# ----- Genus Plot ----

# Step 1: Normalize to relative abundance per tree_health group
genus_relative <- genus_health_sums %>%
  group_by(tree_health) %>%
  mutate(Relative_Abundance = 100 * Total_Abundance / sum(Total_Abundance))

genus_relative2 <- long_data_genus %>%
  group_by(tree_health) %>%
  mutate(Relative_Abundance = 100 * Abundance / sum(Abundance))

# Step 2: Get top 10 most abundant classes overall (summed across both health categories)
top20_genus <- genus_relative %>%
  group_by(Genus) %>%
  summarise(Total = sum(Total_Abundance)) %>%
  slice_max(Total, n = 20) %>%
  pull(Genus)

# Step 3: Filter to keep only top 10 classes
genus_relative_top20 <- genus_relative %>%
  filter(Genus %in% top20_genus)

# Step 4: Plot
ggplot(genus_relative_top20,
       aes(x = reorder(Genus, -Relative_Abundance), y = Relative_Abundance, fill = tree_health)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Genus") +
  ylab("Relative Abundance (%)") +
  ggtitle("Top 20 Genus by Relative Abundance and Tree Health") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("healthy" = "forestgreen", "unhealthy" = "firebrick"))

# ---- Genus Lifestyles -----

lifestyles <- read_tsv("D:/meta/fungi_db/FungalTraits_S1.txt")
genus_lifestyles <- merge(genus_relative2, lifestyles, by="Genus")

# Step 2: Get top 10 most abundant classes overall (summed across both health categories)
top20_lifestyles <- genus_lifestyles %>%
  group_by(primary_lifestyle) %>%
  summarise(Total = sum(Total_Abundance)) %>%
  slice_max(Total, n = 9) %>%
  pull(primary_lifestyle)

# Step 3: Filter to keep only top 10 classes
ls_relative_top20 <- genus_lifestyles %>%
  filter(primary_lifestyle %in% top20_lifestyles)

ggplot(ls_relative_top20,
       aes(x = reorder(primary_lifestyle, -Relative_Abundance), y = Relative_Abundance, fill = tree_health)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal(base_size = 12) +
  labs(
    x = "Genus primary Lifestyle",
    y = "Relative Abundance (%)",
    color = "Tree Health",
    fill = "Tree Health"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.text = element_text(size = 11)) +
  scale_fill_manual(values = c("healthy" = "forestgreen", "unhealthy" = "firebrick"))

# ---- Regression Model ----

grouped_data <- genus_relative2%>%
  group_by(Genus, SampleID) %>%
  summarise(
    site = first(site),
    province = first(province),
    tree_health = first(tree_health),
    Total_Abundance = sum(Abundance, na.rm = TRUE),
    Total_Relative_Abundance = sum(Relative_Abundance, na.rm = TRUE),
    def = mean(def, na.rm = TRUE),
    height = mean(height, na.rm = TRUE),
    DBH = mean(DBH, na.rm = TRUE),
    pH = mean(pH, na.rm = TRUE),
    .groups = "drop"
  )

genus_lifestyles <- merge(grouped_data, lifestyles, by="Genus")

ectos <- genus_lifestyles %>%
  filter(primary_lifestyle == "ectomycorrhizal")

# Run linear mixed-effects model
mod <- lmer(def ~ Total_Relative_Abundance + (1 | site), data = ectos)

# Summary of the model
summary(mod)

pred_df <- ggpredict(mod, terms = "Total_Relative_Abundance")

# Plot actual data with model prediction
ggplot(ectos, aes(x = Total_Relative_Abundance, y = def)) +
  geom_point(aes(color = site), alpha = 0.7) +
  geom_line(data = pred_df, aes(x = x, y = predicted), color = "black", linewidth = 1.2) +
  #geom_ribbon(data = pred_df, aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "grey50") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Effect of Ectomycorrhizal Abundance on Tree Defoliation",
    x = "Relative Abundance (Ectomycorrhiza)",
    y = "Defoliation (DEF)"
  )

# ---- Regression Model old ----

field_data <- read.csv("D:/meta/meta-soil-fungi-variables.csv")

colnames(long_data_order)[2] <- "id"
field_merged <- merge(long_data_order, field_data, by = "id")

field_merged_relative <- field_merged %>%
  mutate(Relative_Abundance = 100 * Abundance / sum(Abundance))

mod <- lmer(def ~ Relative_Abundance + (1 | site.x), data = field_merged_relative)
summary(mod)

order_models <- field_merged_relative %>%
  group_by(Order) %>%
  group_map(~ {
    mod <- lmer(def ~ Relative_Abundance + (1 | site.x), data = .x)
    tidy(mod) %>% mutate(Order = .y$Order)
  }) %>%
  bind_rows()

order_models %>%
  filter(effect == "fixed", term == "Relative_Abundance") %>%
  arrange(p.value) %>%
  select(Order, estimate, std.error, p.value)

selected_classes <- c("Dothideomycetes", "Leotiomycetes")

# Assuming `long_class_data` contains columns: SampleID, Class, Relative_Abundance, tree_health
# (create long format if needed)

ggplot(field_merged_relative,
       aes(x = Relative_Abundance, y = def, fill = site.x)) +
  geom_point() +
  theme_minimal() +
  xlab("Lifestyle") +
  ylab("Relative Abundance (%)") +
  ggtitle("Genus Lifestyle by Relative Abundance and Tree Health") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Arga" = "forestgreen", "Capitana" = "firebrick", "Cortijo" = "orange"))

# 2. Pivot wider to have one column per class
wide_data <- class_relative %>%
  filter(Class %in% selected_classes) %>%
  select(Class, Relative_Abundance, tree_health) %>%
  tidyr::pivot_wider(names_from = Class, values_from = Relative_Abundance)

# 3. Convert tree_health to a binary factor (if not already)
wide_data <- wide_data %>%
  mutate(tree_health = factor(tree_health, levels = c("healthy", "unhealthy")))

# 4. Fit logistic regression
model <- glm(tree_health ~ Dothideomycetes + Leotiomycetes,
             data = wide_data,
             family = binomial())

# 5. Summary of the model
summary(model)


# ---- Alpha Plotting -----

richness <- read.table("D:/meta/07_diversity/richness/alpha-diversity.tsv", header=TRUE, check.names=FALSE)
evenness <- read.table("D:/meta/07_diversity/evenness/alpha-diversity.tsv", header=TRUE, check.names=FALSE)
shannon <- read.table("D:/meta/07_diversity/Shannon/alpha-diversity.tsv", header=TRUE, check.names=FALSE)

richness$id <- rownames(richness)
evenness$id <- rownames(evenness)
shannon$id <- rownames(shannon)

richness_merged <- merge(richness, metadata, by='id')
evenness_merged <- merge(evenness, metadata, by='id')
shannon_merged <- merge(shannon, metadata, by='id')

common_colors <- c("healthy" = "forestgreen", "unhealthy" = "firebrick")

# Richness (Observed Features)
p1 <- ggplot(richness_merged, aes(x = tree_health, y = observed_features, fill = tree_health)) + 
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.color = "black", alpha = 0.8) +
  scale_fill_manual(values = common_colors) +
  labs(title = "A. Richness", y = "Observed Features", x = "Tree Health Status") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Pielou's Evenness
p2 <- ggplot(evenness_merged, aes(x = tree_health, y = pielou_evenness, fill = tree_health)) + 
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.color = "black", alpha = 0.8) +
  scale_fill_manual(values = common_colors) +
  labs(title = "B. Pielou's Evenness", y = "Pielou's Evenness", x = "Tree Health Status") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Shannon Entropy
p3 <- ggplot(shannon_merged, aes(x = tree_health, y = shannon_entropy, fill = tree_health)) + 
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.color = "black", alpha = 0.8) +
  scale_fill_manual(values = common_colors) +
  labs(title = "C. Shannon Entropy", y = "Shannon Entropy", x = "Tree Health Status") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# Combine plots
combined_plot <- p1 | p2 | p3
combined_plot

# For healthy trees
shapiro.test(richness_merged$observed_features[richness_merged$tree_health == "healthy"])
shapiro.test(richness_merged$observed_features[richness_merged$tree_health == "unhealthy"])


wilcox.test(observed_features ~ tree_health, data = richness_merged)
wilcox.test(pielou_evenness ~ tree_health, data = evenness_merged)
wilcox.test(shannon_entropy ~ tree_health, data = shannon_merged)

t_richness <- t.test(observed_features ~ tree_health, data = richness_merged)
t_evenness <- t.test(pielou_evenness ~ tree_health, data = evenness_merged)
t_shannon <- t.test(shannon_entropy ~ tree_health, data = shannon_merged)

t_richness$p.value
t_evenness$p.value
t_shannon$p.value


# ---- Beta Plotting -----

dist_matrix <- read.table("D:/meta/07_diversity/exported_distance/distance-matrix.tsv", header=TRUE, check.names=FALSE)
dist_obj <- as.dist(as.matrix(dist_matrix))
nmds_results <- metaMDS(dist_obj, k=2, trymax=100)

nmds_coords <- as.data.frame(nmds_results$points)
nmds_coords$id <- rownames(nmds_coords)

dist_matrix_id <- dist_matrix
dist_matrix_id$id <- rownames(dist_matrix)
dist_matrix_merged <- merge(dist_matrix_id, metadata, by='id')

adonis_result <- adonis2(dist_obj ~ tree_health, data = metadata)
print(adonis_result)

bd <- betadisper(dist_obj, metadata$tree_health)

# 2. Perform test
anova(bd)  # Tests for differences in dispersion
permutest(bd)  # Permutation-based version

merged_data <- merge(nmds_coords, metadata, by='id')



# Optional: Plot it
plot(bd)
boxplot(bd)

bd_data <- data.frame(
  Distance = bd$distances,
  Group = bd$group
)

common_colors <- c("healthy" = "forestgreen", "unhealthy" = "firebrick")

# NMDS Ordination
p1 <- ggplot(merged_data, aes(x = MDS1, y = MDS2, color = tree_health, fill = tree_health)) +
  geom_point(size = 2, shape = 21, stroke = 0.3) +
  stat_ellipse(geom = "polygon", alpha = 0.2, color = NA) +
  scale_color_manual(values = common_colors) +
  scale_fill_manual(values = common_colors) +
  theme_minimal(base_size = 12) +
  labs(
    title = "A. NMDS Ordination (Bray–Curtis)",
    x = "NMDS1",
    y = "NMDS2",
    color = "Tree Health",
    fill = "Tree Health"
  ) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )

# Beta Dispersion Boxplot
p2 <- ggplot(bd_data, aes(x = Group, y = Distance, fill = Group)) +
  geom_boxplot(width = 0.6, outlier.shape = 21, outlier.color = "black", alpha = 0.8) +
  scale_fill_manual(values = common_colors) +
  theme_minimal(base_size = 12) +
  labs(
    title = "B. Beta Diversity Dispersion",
    x = "Tree Health Status",
    y = "Distance to Group Centroid"
  ) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Combine both plots side by side
combined_plot <- p1 | p2
combined_plot

# ---- regression model ----

fungi_variables <- read.csv("D:/meta/meta-soil-fungi-variables.csv")
meta_merged <- merge(fungi_variables, )