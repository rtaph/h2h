
# Rafael Pilliard Hellwig, Selma Duric, Tanmay Sharma, Zeliha Ural Merpez, Debananda Sarkar
# 2021-03-13
#
# This make file is for data capture
#
# Usage:
# make all

all : data/g.rda

data-raw/license_data.csv : src/get_license_data.py
	python src/get_license_data.py --target_file=data-raw/license_data.csv

data-raw/linked_corp_data.csv : src/get_linked_corp_data.py
	python src/get_linked_corp_data.py --target_file=data-raw/linked_corp_data.csv

data-processed/combined_data.csv : src/get_linked_corp_data.py data-raw/license_data.csv data-raw/linked_corp_data.csv
	python src/join_license_linked_corp.py --license_file=data-raw/license_data.csv  --company_hierarchy_file=data-raw/linked_corp_data.csv  --target_file=data-processed/combined_data.csv

data-processed/filtered_hierarchy_data.csv : src/join_license_linked_corp.py data-processed/combined_data.csv data-raw/linked_corp_data.csv
	python src/filter_hierarchy_file.py --combined_file=data-processed/combined_data.csv  --company_hierarchy_file=data-raw/linked_corp_data.csv  --target_file=data-processed/filtered_hierarchy_data.csv

data/vbr.rda : R/clean_data.R data-processed/combined_data.csv
	Rscript -e "library(tidyverse); source('R/clean_data.R'); data_vbr()"

data/g.rda : R/clean_data.R data/vbr.rda
	Rscript -e "library(tidyverse); library(igraph); source('R/clean_data.R'); build_graph()"

clean :
	rm -rf data-raw/license_data.csv
	rm -rf data-raw/linked_corp_data.csv
	rm -rf data-processed/combined_data.csv
	rm -rf data-processed/filtered_hierarchy_data.csv
	rm -rf data/vbr.rda
	rm -rf data/g.rda
