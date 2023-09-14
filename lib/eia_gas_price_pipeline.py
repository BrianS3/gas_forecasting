import requests
import json
import os
import sqlite3
import pandas as pd
from load_creds import load_env_credentials
from datetime import datetime

current_date = datetime.now().date()

root = os.path.abspath('..')

filepath = os.path.join(root, f'logs\\run_log_{current_date}.txt')

file = open(filepath, 'w+')
file.close()

load_env_credentials()

api_key = os.getenv('eia_api_key')

db_file = os.path.join(root, 'database\\gas_data.db')
connection = sqlite3.connect(db_file)

query = """
select 
distinct padd, 
max(period) as max_period 
from gas_prices group by padd
"""
max_dates = pd.read_sql(query, connection)
max_dates_dict = dict(zip(max_dates['padd'], max_dates['max_period']))

padds = {
    "PADD 1": "PET.EMM_EPM0_PTE_R10_DPG.W", #east coast
    "PADD 2": "PET.EMM_EPM0_PTE_R20_DPG.W", #midwest
    "PADD 3": "PET.EMM_EPM0_PTE_R30_DPG.W", #gulf coast
    "PADD 4": "PET.EMM_EPM0_PTE_R40_DPG.W", #rocky mountain
    "PADD 5": "PET.EMM_EPM0_PTE_R50_DPG.W" #west coast
}

for key, value in padds.items():
    series_id = padds[key]
    url = f"https://api.eia.gov/v2/seriesid/{series_id}?api_key={api_key}"

    with open(filepath, "a") as file:
        response = requests.get(url)
        if response.status_code == 200:
            file.write(f"Request successful: {series_id}\n")
        else:
            file.write(f"Request failed with status code: {response.status_code}\n")

    data = json.loads(response.text)
    df = pd.DataFrame(data['response']['data'])

    df = df[["area-name", "duoarea", "period", "product", "product-name", "series-description", "value", "units"]]
    df = df.rename(columns = {"area-name":"padd",
                              "product-name":"product_name",
                              "series-description":"series_description",
                              "value":"price"})
    
    max_date_check = max_dates_dict[key]
    df['period'] = pd.to_datetime(df['period'])
    df = df[df['period']>pd.to_datetime(max_date_check)]
    df['period'] = pd.to_datetime(df['period']).dt.date

    if len(df) > 0 :
        df.to_sql(name = 'gas_prices',
                  con=connection,
                  if_exists='append',
                  index=False)

        with open(filepath, "a") as file:
            file.write(f'{series_id}: write successful\n')
    else:
        with open(filepath, "a") as file:
            file.write(f'{series_id}: No Data\n')

connection.commit()
connection.close()