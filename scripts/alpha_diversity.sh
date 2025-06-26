#!/bin/bash

qiime diversity-lib observed-features \
  --i-table ./03_denoising/table230_filtered-reindexed.qza \
  --o-vector ./07_diversity/richness_diversity.qza

qiime diversity-lib pielou-evenness \
  --i-table ./03_denoising/table230_filtered-reindexed.qza \
  --o-vector ./07_diversity/evenness_diversity.qza

qiime diversity-lib shannon-entropy \
  --i-table ./03_denoising/table230_filtered-reindexed.qza \
  --o-vector ./07_diversity/shannon_diversity.qza

qiime tools export \
	--input-path ./07_diversity/shannon_diversity.qza \
	--output-path ./07_diversity/Shannon

qiime tools export \
	--input-path ./07_diversity/evenness_diversity.qza \
	--output-path ./07_diversity/evenness

qiime tools export \
	--input-path ./07_diversity/richness_diversity.qza \
	--output-path ./07_diversity/richness
