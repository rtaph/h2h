# author:
# date:

"""This script will pull data from a source url
and store it in a specified file

Usage: join_license_linked_corp.py  --license_file=<license_file>  --company_hierarchy_file=<company_hierarchy_file>  --target_file=<target_file>

Options:
--license_file=<license_file>                           Business License file
--company_hierarchy_file=<company_hierarchy_file>       Company Hierarchy File
--target_file=<target_file>                             Target file name to store data (with path)
"""

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime
from docopt import docopt

opt = docopt(__doc__)


def main(license_file, company_hierarchy_file, target_file):
    cov_data = pd.read_csv(
        license_file, sep=",", low_memory=False, dtype={20: "str"}
    )
    stat_canada_data = pd.read_csv(company_hierarchy_file, encoding='latin1')

    cov_data = cov_data.assign(
        BusinessName_join=cov_data.BusinessName.str.lower().str.strip()
    )
    cov_data = cov_data.assign(
        BusinessTradeName_join=cov_data.BusinessTradeName.str.lower().str.strip()
    )
    cov_data = cov_data.assign(
        BusinessName_join=cov_data.BusinessName_join.str.replace(
            r"[^\w]", "", regex=True
        )
    )
    cov_data = cov_data.assign(
        BusinessTradeName_join=cov_data.BusinessTradeName_join.str.replace(
            r"[^\w]", "", regex=True
        )
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

    # create rankings
    stat_canada_data_ranked = stat_canada_data
    stat_canada_data_ranked = stat_canada_data_ranked.assign(
        ranking=stat_canada_data_ranked.groupby("NAME_JOIN")["COP"]
        .rank(ascending=False)
        .astype("int")
    )
    stat_canada_data_ranked = stat_canada_data_ranked.query("ranking ==1")
    stat_canada_data_ranked = stat_canada_data_ranked.assign(
        ranking=stat_canada_data_ranked.groupby("NAME_JOIN")["LEVEL"]
        .rank(ascending=False)
        .astype("int")
    )
    stat_canada_data_ranked = stat_canada_data_ranked.query("ranking ==1")
    stat_canada_data_ranked = stat_canada_data_ranked.assign(
        ranking=stat_canada_data_ranked.groupby("NAME_JOIN")["RECID"]
        .rank(ascending=False)
        .astype("int")
    )
    stat_canada_data_ranked = stat_canada_data_ranked.query("ranking ==1")

    # merge both dataframes
    merged_df = cov_data.merge(
        stat_canada_data_ranked,
        how="left",
        left_on="BusinessName_join",
        right_on="NAME_JOIN",
    )
    merged_BN_df = merged_df[merged_df.NAME_JOIN.isnull() == False]

    assert merged_df.shape[0] == cov_data.shape[0], "Check Data inflation"

    # try remaining unmatched on second name column
    cov_data_remainder = merged_df[merged_df.NAME_JOIN.isnull()]
    merged_BTN_df = cov_data_remainder.merge(
        stat_canada_data_ranked,
        how="inner",
        left_on="BusinessTradeName_join",
        right_on="NAME_JOIN",
    )

    merged_df = pd.concat([merged_BN_df, merged_BTN_df], axis=0)

    merged_df.to_csv(target_file, index=False)


if __name__ == "__main__":
    main(
        opt["--license_file"],
        opt["--company_hierarchy_file"],
        opt["--target_file"],
    )
