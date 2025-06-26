#!/bin/bash

qiime tools import \
	--type 'SampleData[PairedEndSequencesWithQuality]' \
	--input-path ./raw_data/ \
	--input-format CasavaOneEightSingleLanePerSampleDirFmt \
	--output-path ./01_input_qiime/paired-end-demux.qza
