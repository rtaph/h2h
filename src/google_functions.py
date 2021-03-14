"""
Author: Zeliha Ural Merpez

Date: March,13 2021

"""

import requests
import json
import pandas as pd
from bs4 import BeautifulSoup
import altair as alt
import numpy as np
from math import sin, cos, sqrt, atan2, radians
import matplotlib.pyplot as plt

def get_keys(path):
    with open(path) as f:
        return json.load(f)

def get_address_google(name_list, API_Key, city_list=0):
    placesAPI_data = pd.DataFrame(
        columns=["name", "formatted_address", "geometry", "permanently_closed"]
    )  # initialize dataframe
    if isinstance(name_list, str):
        name_list = [name_list]
    for i in range(len(name_list)):
        if city_list == 0:
            city = ""
        else:
            city = city_list[i]
        name = (
            name_list[i].replace(" ", "%20").replace("&", "%26")
        )  # make sure there are no blank spaces for the URL and deal with &
        b = "!@#$()"
        for char in b:
            name = name.replace(char, "")
        address_search = name + ",%20" + city
        url = (
            "https://maps.googleapis.com/maps/api/place/findplacefromtext/json?input="
            + address_search
            + "&inputtype=textquery&fields=name,formatted_address,geometry,permanently_closed&key="
            + API_Key
        )
        response = requests.get(url).json()
        placesAPI_data = pd.concat(
            [placesAPI_data, pd.DataFrame(response["candidates"])],
            ignore_index=True,
            sort=False,
        )  # append retrieved information to a dataframe
    google_data = placesAPI_data
    lat_list = []
    lng_list = []
    for i in range(google_data.shape[0]):
        lat_list.append(google_data["geometry"][i]["location"]["lat"])
        lng_list.append(google_data["geometry"][i]["location"]["lng"])
    google_data["lat"] = lat_list
    google_data["lng"] = lng_list

    return google_data

def format_coordinate(dataframe, drop=False):
    df = dataframe
    if drop:
        df = df.dropna(subset=["Geom"])
    df[["drop_this", "location"]] = df.Geom.str.split("[", expand=True)
    df[["lng", "lat"]] = df.location.str.split(",", expand=True)
    df["lat"] = df.lat.str.replace("]}", "")
    df = df.drop(columns=["drop_this", "location"])
    return df

def get_distance_by_coordinate(formatted_business, name, API_Key):
    google_data = get_address_google(name, API_Key)
    google_name = google_data["name"][0]
    filter_name = formatted_business[formatted_business["BusinessName"] == name]
    if filter_name.shape[0] == 0:
        warn = (
            "No geometric information provided for "
            + filter_name
            + " in Business Licences data."
        )
        return warn, 5000
    else:
        lat = float(filter_name[["lat"]].iloc[0])
        lng = float(filter_name[["lng"]].iloc[0])
    if google_data.shape[0] == 0:
        warn = "Could not find information about " + filter_name + " on Google maps."
        return warn, 5000
    else:
        google_lat = google_data["lat"][0]
        google_lng = google_data["lng"][0]
    warn = "Giving distance between geometric information obtained."
    dlon = radians(lng) - radians(google_lng)
    dlat = radians(lat) - radians(google_lat)
    a = (sin(dlat / 2)) ** 2 + cos(radians(lat)) * cos(radians(google_lat)) * (
        sin(dlon / 2)
    ) ** 2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    R = 6373.0
    distance = R * c
    return warn, distance, google_name

def get_comparison_dataframe(funerals, API_Key):
    name_list = list(funerals["BusinessName"])
    distance_list = []
    warn_list = []
    google_name_list = []
    for i in range(len(name_list)):
        warn, distance, google_name = get_distance_by_coordinate(
            funerals, name=name_list[i], API_Key = API_Key
        )
        distance_list.append(distance)
        warn_list.append(warn)
        google_name_list.append(google_name)
    distance_data = pd.DataFrame(
        {
            "Name": name_list,
            "Google Name": google_name_list,
            "Distance(km)": distance_list,
            "Warning": warn_list,
        }
    )
    return distance_data

def google_map_figure(output_data):
    chart = alt.Chart(output_data, title = "Comparing Distance Between locations of Businesses (Licence vs GoogleMaps)").mark_circle(size = 50).encode(
        x = alt.X('Distance(km)'),
        y = alt.Y('Name',axis=alt.Axis(title=" ")),
        color = alt.Color('Google Name'),
        tooltip = 'Google Name'
    )
    return chart

def fancy_table(data, col_width=3.0, row_height=0.625, row_colors=['#f1f1f2', 'w'],
                header_columns=0, ax=None, **kwargs):

    """[Modified from ref: https://stackoverflow.com/questions/19726663/how-to-save-the-pandas-dataframe-series-data-as-a-figure]
       [Prints given dataframe in a nice format, that is easy to save]
    Parameters
    ----------
    data : [data frame]
        [data frame]
    col_width : float, optional
        [column width], by default 3.0
    row_height : float, optional
        [row height], by default 0.625
    row_colors : list, optional
        [row color], by default ['#f1f1f2', 'w']
    header_columns : int, optional
        [header columns], by default 0
    ax : [type], optional
        [plotting table, by default None
    Returns
    -------
    [object]
        [figure]
    """
    if ax is None:
        size = (np.array(data.shape[::-1]) + np.array([0, 1])) * np.array([col_width, row_height])
        fig, ax = plt.subplots(figsize=size)
        ax.axis('off')
    mpl_table = ax.table(cellText=data.values, bbox=[0, 0, 1, 1], colLabels=data.columns, **kwargs)
    mpl_table.auto_set_font_size(False)
    mpl_table.set_fontsize(14)

    for k, cell in mpl_table._cells.items():
        cell.set_edgecolor('w')
        if k[0] == 0 or k[1] < header_columns:
            cell.set_text_props(weight='bold', color='w')
            cell.set_facecolor('firebrick')
        else:
            cell.set_facecolor(row_colors[k[0]%len(row_colors) ])
    return ax.get_figure(), ax

def generate_dataset_overview(data_frame):
    """
    Generates an overview of the dataset.
    Also saves resulting table as file in given output folder.
    Parameters:
    -----------
    data_frame : pandas.DataFrame
        input path to be verified
    output_folder : str
        output folder path to save the chart
    file_name : str
        file name for generated chart image
        
    Returns:
    -----------
    None
    """
    data_overview = [
            {"Dataset": "Number of features", "Value": len(data_frame.columns)},
            {"Dataset": "Number of characters", "Value": len(data_frame)},
            {"Dataset": "Number of Missing cells", "Value": (data_frame.isnull()).sum().sum()},
            {"Dataset": "Percentage of Missing cells", "Value": round((data_frame.isnull()).sum().sum()/data_frame.size*100, 2)}
        ]
    overview_frame = pd.DataFrame(data_overview)
    return overview_frame

def generate_feature_overview(data_frame):
    """
    Generates an overview of the features in dataset.
    Also saves resulting table as file in given output folder.
    Parameters:
    -----------
    data_frame : pandas.DataFrame
        input path to be verified
    output_folder : str
        output folder path to save the chart
    file_name : str
        file name for generated chart image
        
    Returns:
    -----------
    None
    """
    distinct_class = dict()
    nonnull_count = dict()

    for col in data_frame.columns:
        nonnull_count[col]=len(data_frame)-data_frame[col].isnull().sum()
        distinct_class[col]=len(list(data_frame[col].unique()))

    features_frame=pd.DataFrame([distinct_class, nonnull_count]).T.reset_index()
    features_frame.columns=["Features","Distinct Class", "Non-Null Count"]
    features_frame["Missing Percentage"]=round((len(data_frame) - features_frame["Non-Null Count"])/len(data_frame)*100,2)
    return features_frame
