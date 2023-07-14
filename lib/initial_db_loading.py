import requests
import json
import os
import pymysql
from sqlalchemy import create_engine
import pandas as pd
from lib.load_creds import load_env_credentials

load_env_credentials()

api_key = os.getenv('eia_api_key')
host = os.getenv('database_host')
user = os.getenv('user')
password = os.getenv('pass')
database = os.getenv('database')

connection = pymysql.connect(
    host = host,
    user = user,
    password = password,
    database = database
)

engine = create_engine(f"mysql+pymysql://{user}:{password}@{host}/{database}")

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

    with open(f"../logs/initial_loading.txt", "a") as file:
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
              con=engine,
              if_exists='append',
              index=False)

    with open(f"../logs/initial_loading.txt", "a") as file:
        file.write(f'{series_id}: write successful\n')


