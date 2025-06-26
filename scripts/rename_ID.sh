#!/bin/bash

qiime feature-table group \
	--i-table ./03_denoising/table220_filtered.qza \
	--p-axis sample \
	--m-metadata-file ./metadata_group_2.txt \
	--m-metadata-column tree \
	--p-mode sum \
	--o-grouped-table ./03_denoising/table220_filtered-reindexed.qza 


qiime feature-table summarize \
	--i-table ./03_denoising/table220_filtered-reindexed.qza \
	--m-sample-metadata-file ./metadata_group_2-reindexed.txt \
	--o-visualization ./03_denoising/table220_filtered-reindexed.qzv
