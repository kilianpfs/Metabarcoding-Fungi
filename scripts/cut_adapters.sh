#!/bin/bash

qiime cutadapt trim-paired \
	--i-demultiplexed-sequences ./01_input_qiime/paired-end-demux.qza \
	--p-cores 4 \
	--p-front-f GTGARTCATCGAATCTTTG \
	--p-front-r TCCTCCGCTTATTGATATGC \
	--p-error-rate 0.1 \
	--o-trimmed-sequences ./02_cutadapt/trimmed-seqs.qza \
	--verbose | tee ./02_cutadapt/CutadaptStdout.txt

qiime demux summarize \
	--i-data ./02_cutadapt/trimmed_reads.qza \
	--o-visualization ./02_cutadapt/trimmed_reads.qvz
