#!/bin/bash

qiime diversity alpha-rarefaction \
	--i-table ./03_denoising/table230_filtered-reindexed.qza \
	--p-max-depth 73695 \
	--m-metadata-file ./metadata_group_2-reindexed.txt \
	--o-visualization ./06_rarefaction/rarefaction230.qzv

qiime feature-table rarefy \
	--i-table ./03_denoising/table230_filtered-reindexed.qza \
	--p-sampling-depth 23379 \
	--o-rarefied-table ./06_rarefaction/table230_rarefied.qza
