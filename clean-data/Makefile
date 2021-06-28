#
# Authors:     AF
# Maintainers: AF, SW, GR
# Copyright:   2021, Data Cívica, GPL v2 or newer
# ===============================================
# blog-trabajo-maternidad/import/Makefile

.PHONY: all clean

all: output/t118.rds \
	output/t119.rds \
	output/t120.rds \
	output/t121.rds \
	output/t218.rds \
	output/t219.rds \
	output/t318.rds \
	output/t319.rds \
	output/t320.rds \
	output/t418.rds \
	output/t419.rds \
	output/t420.rds 
	
clean: rm output/*

ENOE := output/t118.rds \
	output/t119.rds \
	output/t120.rds \
	output/t121.rds \
	output/t218.rds \
	output/t219.rds \
	output/t318.rds \
	output/t319.rds \
	output/t320.rds \
	output/t418.rds \
	output/t419.rds \
	output/t420.rds 

$(ENOE): src/clean-data.R
	Rscript --vanilla $<

# done
