#!/bin/bash

qiime diversity beta \
	--i-table ./03_denoising/table230_filtered-reindexed.qza \
	--p-metric braycurtis \
	--o-distance-matrix ./07_diversity/bray_curtis_distance.qza

qiime diversity nmds \
	--i-distance-matrix ./07_diversity/bray_curtis_distance.qza \
	--o-nmds ./07_diversity/bray_curtis_nmds.qza

qiime emperor plot \
	--i-pcoa ./07_diversity/bray_curtis_nmds.qza \
	--m-metadata-file metadata_group_2-reindexed.txt \
	--o-visualization ./07_diversity/bray_curtis_nmds.qzv

qiime diversity beta-group-significance \
	--i-distance-matrix ./07_diversity/bray_curtis_distance.qza \
	--m-metadata-file metadata_group_2-reindexed.txt \
	--m-metadata-column tree_health \
	--p-method permanova \
	--o-visualization ./07_diversity/bray_curtis_permanova.qzv
