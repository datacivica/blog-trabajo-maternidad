#
# Authors:     AF
# Maintainers: AF, SW, GR
# Copyright:   2021, Data Cívica, GPL v2 or newer
# ===============================================
# blog-trabajo-maternidad/import/Makefile

.PHONY: all clean

all: output/coe2t118.rds \
	output/coe2t119.rds \
	output/coe2t120.rds \
	output/coe2t121.rds \
	output/coe2t218.rds \
	output/coe2t219.rds \
	output/coe2t318.rds \
	output/coe2t319.rds \
	output/coe2t320.rds \
	output/coe2t418.rds \
	output/coe2t419.rds \
	output/coe2t420.rds \
	output/sdemt118.rds \
	output/sdemt119.rds \
	output/sdemt120.rds \
	output/sdemt121.rds \
	output/sdemt218.rds \
	output/sdemt219.rds \
	output/sdemt318.rds \
	output/sdemt319.rds \
	output/sdemt320.rds \
	output/sdemt418.rds \
	output/sdemt419.rds \
	output/sdemt420.rds

clean: 
		rm output/*

ENOE := output/coe2t118.rds \
	output/coe2t119.rds \
	output/coe2t120.rds \
	output/coe2t121.rds \
	output/coe2t218.rds \
	output/coe2t219.rds \
	output/coe2t318.rds \
	output/coe2t319.rds \
	output/coe2t320.rds \
	output/coe2t418.rds \
	output/coe2t419.rds \
	output/coe2t420.rds \
	output/sdemt118.rds \
	output/sdemt119.rds \
	output/sdemt120.rds \
	output/sdemt121.rds \
	output/sdemt218.rds \
	output/sdemt219.rds \
	output/sdemt318.rds \
	output/sdemt319.rds \
	output/sdemt320.rds \
	output/sdemt418.rds \
	output/sdemt419.rds \
	output/sdemt420.rds

$(ENOE): src/import.R
	Rscript --vanilla $<

# done
