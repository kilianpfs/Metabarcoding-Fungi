#!/bin/bash

qiime feature-table filter-features \
  --i-table ./03_denoising/table220.qza \
  --p-min-frequency 2 \
  --o-filtered-table ./03_denoising/table220_filtered.qza

qiime feature-table filter-seqs \
  --i-data ./03_denoising/rep-seqs220.qza \
  --i-table ./03_denoising/table220_filtered.qza \
  --o-filtered-data ./03_denoising/rep-seqs220_filtered.qza

qiime feature-table summarize \
  --i-table ./03_denoising/table220.qza \
  --o-visualization ./03_denoising/table220.qzv

qiime feature-table summarize \
  --i-table ./03_denoising/table220_filtered.qza \
  --o-visualization ./03_denoising/table220_filtered.qzv



