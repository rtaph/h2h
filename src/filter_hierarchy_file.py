# author: Selma Duric
# date: March 21, 2021

"""This script will pull data from the linked_corp_data.csv and combined_data.feather and filter for matches on PID
and store it in a specified file,

Usage: filter_hierarchy_file.py --combined_file=<combined_file>  --company_hierarchy_file=<company_hierarchy_file>  --target_file=<target_file>

Options:
--combined_file=<combined_file>                         Combined Business License and Hierarchy file
--company_hierarchy_file=<company_hierarchy_file>       Company Hierarchy File of all parent & related companies
--target_file=<target_file>                             Target file name to store data (with path)
"""

import pandas as pd
from docopt import docopt

opt = docopt(__doc__)


def main(combined_file, company_hierarchy_file, target_file):
    stat_canada_data = pd.read_csv(company_hierarchy_file, encoding ='latin1')
    combined_df = pd.read_csv(
        combined_file,
        sep=",",
        low_memory=False,
        error_bad_lines=False,
        dtype={20: "str"},
    )

    stat_canada_data.columns = stat_canada_data.columns.str.strip()
    stat_canada_data["IC_ID"] = (
        "C"
        + stat_canada_data.CCID.astype(str)
        + stat_canada_data.PID.astype(str)
    )
    stat_canada_data["NAME_JOIN"] = (
        stat_canada_data["NAME"].str.lower().str.strip()
    )
    stat_canada_data["NAME_JOIN"] = stat_canada_data["NAME_JOIN"].str.replace(
        r"[^\w]", "", regex=True
    )

    # filter to only obtain list of identifiers to match
    unique_PID_list = combined_df["PID"].dropna().unique()

    stat_canada_data = stat_canada_data.query(
        "PID in @unique_PID_list | CCID in @unique_PID_list"
    ).reset_index()

    stat_canada_data.to_feather(target_file)


if __name__ == "__main__":
    main(
        opt["--combined_file"],
        opt["--company_hierarchy_file"],
        opt["--target_file"],
    )
