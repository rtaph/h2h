"""
Author: Zeliha Ural Merpez

Date: March,13 2021

This script reads data and generates images and csv files to be used in further analysis.

Usage: get_google_data.py -i=<input> -o=<output> [--name=<business_name>] [--type=<business_type>]

Options:
-i <input>, --input <input>     Local raw data csv filename and path
-o <output>, --output <output>  Local output directory for created png
[--name=<business_name>]                         Business name
[--type=<business_type>]                         Business type
"""


# example to run: python src/get_google_data.py -i "data/raw/license_data.csv" -o "data/processed"
# example to run: python src/get_google_data.py -i "data/raw/license_data.csv" -o "data/processed" --name "First Memorial Services Ltd"
# example to run: python src/get_google_data.py -i "data/raw/license_data.csv" -o "data/processed" --type "Casino"
import sys
import os
import requests
import json
import pandas as pd
from bs4 import BeautifulSoup
import altair as alt
import numpy as np
from math import sin, cos, sqrt, atan2, radians
from docopt import docopt

args = docopt(__doc__)

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

def main(input_file, output_dir):
    cov_data = pd.read_csv(input_file, sep=";")

    keys = get_keys(".secret/key.json")
    API_Key = keys['API_Key']
    print(cov_data.shape)

    formatted_cov_data = format_coordinate(cov_data, drop=True)
    if business_name:
        funerals = formatted_cov_data[formatted_cov_data['BusinessName'] == business_name]
    else:
        if business_type:
            funerals = formatted_cov_data[formatted_cov_data['BusinessType'] == business_type]
        else:
            funerals = formatted_cov_data[formatted_cov_data['BusinessType'] == "Funeral Services"]

    output_data = get_comparison_dataframe(funerals, API_Key)
    output_data.to_csv(path_or_buf= output_dir + '/google_map.csv', index=False)
    chart = google_map_figure(output_data)
    chart.save(output_dir + '/google_map.html')



    merged_df = pd.concat([merged_BN_df,merged_BTN_df], axis=0)

    merged_df.to_csv('data/processed/combined_data.csv', index=False)

if __name__=="__main__":
    input_file = args["--input"]
    output_dir = args["--output"]
    business_name = args["--name"]
    business_type = args["--type"]
    main(input_file, output_dir)
