import requests
import json
import os
import sqlite3
import pandas as pd
from load_creds import load_env_credentials

load_env_credentials()
api_key = os.getenv('eia_api_key')

db_file = os.path.join('database', 'gas_data.db')
connection = sqlite3.connect(db_file)

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

    with open(f"logs/initial_loading.txt", "a") as file:
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

    df.to_sql(name = 'gas_prices',
              con=connection,
              if_exists='append',
              index=False)

    with open(f"logs/initial_loading.txt", "a") as file:
        file.write(f'{series_id}: write successful\n')

connection.commit()
connection.close()