import requests
import json
import os
import sqlite3
import pandas as pd
from lib.load_creds import load_env_credentials

load_env_credentials()
api_key = os.getenv('eia_api_key')

padds = {
    "east_coast": "PET.EMM_EPM0_PTE_R10_DPG.W",
    "midwest": "PET.EMM_EPM0_PTE_R20_DPG.W",
    "gulf_coast": "PET.EMM_EPM0_PTE_R30_DPG.W",
    "rocky_mountain": "PET.EMM_EPM0_PTE_R40_DPG.W",
    "west_coast": "PET.EMM_EPM0_PTE_R50_DPG.W"
}

holder_df = pd.DataFrame()

for key, value in padds.items():
    series_id = padds[key]
    url = f"https://api.eia.gov/v2/seriesid/{series_id}?api_key={api_key}"

    response = requests.get(url)
    if response.status_code == 200:
        print("Request successful!")
    else:
        print(f"Request failed with status code: {response.status_code}")

    data = json.loads(response.text)
    df = pd.DataFrame(data['response']['data'])
    holder_df = pd.concat([holder_df, df])

print(hold_df)

# print(holder_df['area-name'].unique())
