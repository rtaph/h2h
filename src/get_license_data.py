# author:
# date:

'''This script will pull data from a source url
and store it in a specified file

Usage: get_linked_corp_data.py --target_file=<target_file>

Options:
--target_file=<target_file>     Target file name to store data (with path)
'''

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime
from docopt import docopt

opt = docopt(__doc__)

def main(target_file):
    def prov_cleanup(x):
        invalid_prov_dict = {
            "BC": "BC",
            "Quebec": "QC",
            "ON": "ON",
            "MB": "MB",
            "AB": "AB",
            "QC": "QC",
            "British Columbia": "BC",
            "NB": "NB",
            "NS": "NS",
            "SK": "SK",
            "Ontario": "ON",
            "Alberta": "AB"
        }
        
        if np.nan in [x]:
            return x
        elif x in invalid_prov_dict:
            return invalid_prov_dict[x]
        else:
            return "Other"


    try:
        source_url="https://opendata.vancouver.ca/explore/dataset/business-licences/download/?format=csv&timezone=America/Los_Angeles&lang=en&use_labels_for_header=true&csv_separator=%3B"
        dataset_df_1 = pd.read_csv(source_url, sep=";", low_memory=False, dtype={20:'str'} )

        dataset_df_1['id'] = dataset_df_1.index + 1
        dataset_df_1['id'] = 'B' + dataset_df_1['id'].astype(str)

        source_url="https://opendata.vancouver.ca/explore/dataset/business-licences-1997-to-2012/download/?format=csv&timezone=America/Los_Angeles&lang=en&use_labels_for_header=true&csv_separator=%3B"
        dataset_df_2 = pd.read_csv(source_url, sep=";", low_memory=False, dtype={20:'str'} )

        dataset_df_2['id'] = dataset_df_2.index + 1
        dataset_df_2['id'] = 'A' + dataset_df_2['id'].astype(str)

        dataset_df = pd.concat([dataset_df_2, dataset_df_1], ignore_index=True)

        dataset_df = dataset_df.assign(NumberofEmployees = dataset_df.NumberofEmployees.apply(lambda x: np.nan if x=="000" else x))
        dataset_df = dataset_df.assign(perc_missing = dataset_df.isnull().sum(axis=1)/dataset_df.shape[1]*100)
        dataset_df = dataset_df.assign(prov_cleaned = dataset_df.Province.str.lower.apply(prov_cleanup))
        dataset_df = dataset_df.assign(age =  datetime.today().year-2000- dataset_df.FOLDERYEAR)




        dataset_df.to_csv(target_file, index=False)
        print("Download complete")
    except FileNotFoundError as fx:
        print("Error in target file path")
        print(fx)
        print(type(fx))   
    except Exception as ex:
        print(ex)
        print(type(ex))

if __name__=="__main__":
    main(opt["--target_file"])

