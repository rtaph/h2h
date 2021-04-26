
# Rafael Pilliard Hellwig, Selma Duric, Tanmay Sharma, Zeliha Ural Merpez, Debananda Sarkar
# 2021-03-13
#
# This make file is for data capture
#
# Usage:
# make all

all : data/g.rda data-processed/filtered_hierarchy_data.feather

data-raw/license_data.csv : src/get_license_data.py
	python src/get_license_data.py --target_file=data-raw/license_data.csv

data-raw/linked_corp_data.csv : src/get_linked_corp_data.py
	python src/get_linked_corp_data.py --target_file=data-raw/linked_corp_data.csv

data-processed/combined_data.csv : src/join_license_linked_corp.py data-raw/license_data.csv data-raw/linked_corp_data.csv
	python src/join_license_linked_corp.py --license_file=data-raw/license_data.csv  --company_hierarchy_file=data-raw/linked_corp_data.csv  --target_file=data-processed/combined_data.csv

data-processed/filtered_hierarchy_data.feather : src/join_license_linked_corp.py data-processed/combined_data.csv data-raw/linked_corp_data.csv
	python src/filter_hierarchy_file.py --combined_file=data-processed/combined_data.csv  --company_hierarchy_file=data-raw/linked_corp_data.csv  --target_file=data-processed/filtered_hierarchy_data.feather

data/vbr.rda : src/clean_vbr.R data-processed/combined_data.csv
	Rscript src/clean_vbr.R

data/g.rda : src/make_graph.R data/vbr.rda data-raw/linked_corp_data.csv data-processed/combined_data.csv
	Rscript src/make_graph.R

clean :
	rm -rf data-raw/license_data.csv
	rm -rf data-raw/linked_corp_data.csv
	rm -rf data-processed/combined_data.csv
	rm -rf data/vbr.rda
	rm -rf data/g.rda

clean-processed :
	rm -rf data-processed/combined_data.csv
	rm -rf data-processed/filtered_hierarchy_data.csv
	rm -rf data-processed/filtered_hierarchy_data.feather
	rm -rf data/vbr.rda
	rm -rf data/g.rda
