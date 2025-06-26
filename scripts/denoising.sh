#!/bin/bash

qiime dada2 denoise-paired \
	--i-demultiplexed-seqs ./02_cutadapt/trimmed-seqs.qza \
	--p-trunc-len-f 220 \
	--p-trunc-len-r 220 \
	--p-n-threads 4 \
	--o-table ./03_denoising/table220.qza \
	--o-representative-sequences ./03_denoising/rep-seqs220.qza \
 	--o-denoising-stats ./03_denoising/denoising-stats.qza
