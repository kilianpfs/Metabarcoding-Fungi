#!/bin/bash

qiime feature-classifier classify-sklearn \
	--i-classifier ./fungi_db/classifier_ITS2.qza \
	--i-reads ./03_denoising/rep-seqs220_filtered.qza \
	--o-classification ./04_taxonomy/classified_sequences.qza
