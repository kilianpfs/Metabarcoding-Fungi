#!/bin/bash

qiime feature-table summarize \
	--i-table ./03_denoising/table250.qza \
	--o-visualization ./03_denoising/table250.qzv \
	--m-sample-metadata-file ./metadata_group_2.txt

qiime feature-table tabulate-seqs \
	--i-data ./03_denoising/rep-seqs250.qza \
	--o-visualization ./03_denoising/rep-seqs250.qzv \

qiime metadata tabulate \
	--m-input-file ./03_denoising/denoising-stats250.qza \
	--o-visualization ./03_denoising/denoising-stats250.qzv
