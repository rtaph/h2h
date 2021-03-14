# h2h

Group 10 Repository for the ESDC Integrity Hackathon with Simon Fraser University: Canadian Companies Deep Dive.

Authors : Debananda Sarkar, Rafael Pilliard Hellwig, Selma Duric, Tanmay Sharma, Zeliha Ural Merpez

### Objectives of the Hackathon
- Append meaningful information to a business that could be used for analytics.  
- Generate business profiles for integrity and service improvements.  
- Generate insights of value for integrity and service improvements.  
- Generate visualisation tools to explore business characteristics/insights.  
- Scope: a minimum dataset of City of Vancouver business licences is provided, up to a maximum of all Canadian businesses.


### Code Usage:
1. Creating csv and comparison of Business Licences data and GoogleMaps data: Examples to run from GitBash
  - `python src/get_google_data.py -i "data/raw/license_data.csv" -o "data/processed"`
  - `python src/get_google_data.py -i "data/raw/license_data.csv" -o "data/processed" --name "First Memorial Services Ltd"`
  - `python src/get_google_data.py -i "data/raw/license_data.csv" -o "data/processed" --type "Casino"`
  
  Note: In order to access GoogleMaps data, this code requires secret API_Key. 
