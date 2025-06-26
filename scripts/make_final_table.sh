#!/bin/bash

qiime feature-table transpose \
	--i-table ./03_denoising/table220_filtered-reindexed.qza \
	--o-transposed-feature-table ./03_denoising/table220_transposed.qza

qiime metadata tabulate \
	--m-input-file ./03_denoising/table220_transposed.qza \
	--m-input-file ./04_taxonomy/classified_sequences.qza \
	--o-visualization ./05_final_tables/merged-data_taxonomy.qzv

qiime tools export \
	--input-path ./05_final_tables/merged-data_taxonomy.qzv \
	--output-path ./05_final_tables/merged-data_taxonomy
