# author:
# date:

'''This script will pull data from a source url
and store it in a specified file

Usage: get_license_data.py --target_file=<target_file>

Options:
--target_file=<target_file>     Target file name to store data (with path)
'''

import urllib.request
from docopt import docopt

opt = docopt(__doc__)

def main(target_file):
    try:
        source_url="https://www150.statcan.gc.ca/n1/pub/61-517-x/2019004/ICOCD_ICOE.CSV"
        urllib.request.urlretrieve(source_url, target_file)
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

