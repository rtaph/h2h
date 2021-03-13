# author:
# date:

'''This script will pull data from a source url
and store it in a specified file

Usage: get_linked_corp_data.py --target_file=<target_file>

Options:
--target_file=<target_file>     Target file name to store data (with path)
'''

import pandas as pd
from docopt import docopt

opt = docopt(__doc__)

def main(target_file):
    try:
        source_url="https://opendata.vancouver.ca/explore/dataset/business-licences/download/?format=csv&timezone=America/Los_Angeles&lang=en&use_labels_for_header=true&csv_separator=%3B"
        dataset_df_1 = pd.read_csv(source_url, sep=";", low_memory=False, dtype={20:'str'} )
        source_url="https://opendata.vancouver.ca/explore/dataset/business-licences-1997-to-2012/download/?format=csv&timezone=America/Los_Angeles&lang=en&use_labels_for_header=true&csv_separator=%3B"
        dataset_df_2 = pd.read_csv(source_url, sep=";", low_memory=False, dtype={20:'str'} )

        dataset_df = pd.concat([dataset_df_2, dataset_df_1], ignore_index=True)
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

