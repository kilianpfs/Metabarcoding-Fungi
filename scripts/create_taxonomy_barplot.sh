#!/bin/bash

qiime taxa barplot \
	--i-table ./03_denoising/table220_filtered-reindexed.qza \
	--i-taxonomy ./04_taxonomy/classified_sequences.qza \
	--m-metadata-file ./metadata_group_2-reindexed.txt \
	--o-visualization ./04_taxonomy/taxa-bar-plots.qvz
