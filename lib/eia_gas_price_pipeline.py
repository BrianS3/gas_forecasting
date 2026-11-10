"""
Pulls weekly retail gasoline prices (all grades, all formulations) from the
EIA API and appends any new weeks to database/gas_data.db -> gas_prices.

Run from anywhere:  python lib/eia_gas_price_pipeline.py
"""
import os
import sqlite3
from datetime import datetime

import pandas as pd
import requests

from load_creds import load_env_credentials

current_date = datetime.now().date()

# project root = the folder above /lib, no matter where the script is run from
root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

log_path = os.path.join(root, "logs", f"run_log_{current_date}.txt")
open(log_path, "w+").close()


def log(message):
    with open(log_path, "a") as f:
        f.write(message + "\n")


load_env_credentials()
api_key = os.getenv("eia_api_key")

db_file = os.path.join(root, "database", "gas_data.db")
connection = sqlite3.connect(db_file)

# latest week already stored for each series
max_dates = pd.read_sql("select padd, max(period) as max_period from gas_prices group by padd", connection)
max_dates_dict = dict(zip(max_dates["padd"], max_dates["max_period"]))

series = {
    "U.S.":   "PET.EMM_EPM0_PTE_NUS_DPG.W",  # national average (used by the R reports)
    "PADD 1": "PET.EMM_EPM0_PTE_R10_DPG.W",  # east coast
    "PADD 2": "PET.EMM_EPM0_PTE_R20_DPG.W",  # midwest
    "PADD 3": "PET.EMM_EPM0_PTE_R30_DPG.W",  # gulf coast
    "PADD 4": "PET.EMM_EPM0_PTE_R40_DPG.W",  # rocky mountain
    "PADD 5": "PET.EMM_EPM0_PTE_R50_DPG.W",  # west coast
}

for key, series_id in series.items():
    url = f"https://api.eia.gov/v2/seriesid/{series_id}?api_key={api_key}"
    response = requests.get(url, timeout=60)

    if response.status_code != 200:
        log(f"Request failed with status code: {response.status_code} ({series_id})")
        continue
    log(f"Request successful: {series_id}")

    df = pd.DataFrame(response.json()["response"]["data"])
    df = df[["area-name", "duoarea", "period", "product", "product-name", "series-description", "value", "units"]]
    df = df.rename(columns={"area-name": "padd",
                            "product-name": "product_name",
                            "series-description": "series_description",
                            "value": "price"})
    df["padd"] = key  # keep one consistent label per series
    df["price"] = pd.to_numeric(df["price"])

    # only keep weeks newer than what is already stored (all weeks for a brand-new series)
    max_date_check = max_dates_dict.get(key)
    df["period"] = pd.to_datetime(df["period"])
    if max_date_check is not None:
        df = df[df["period"] > pd.to_datetime(max_date_check)]
    df["period"] = df["period"].dt.date
    df = df.drop_duplicates(subset=["padd", "period"])

    if len(df) > 0:
        df.to_sql(name="gas_prices", con=connection, if_exists="append", index=False)
        log(f"{series_id}: wrote {len(df)} new weeks")
    else:
        log(f"{series_id}: No Data")

# --- Brent crude oil, weekly ($/barrel): rebuilt in full each run -----------
response = requests.get(f"https://api.eia.gov/v2/seriesid/PET.RBRTE.W?api_key={api_key}", timeout=60)
if response.status_code == 200:
    crude = pd.DataFrame(response.json()["response"]["data"])[["period", "value"]]
    crude = crude.rename(columns={"value": "price"})
    crude["price"] = pd.to_numeric(crude["price"])
    crude.to_sql(name="crude_prices", con=connection, if_exists="replace", index=False)
    log(f"PET.RBRTE.W: wrote {len(crude)} weeks of Brent crude")
else:
    log(f"Brent request failed with status code: {response.status_code}")

# remove any duplicate weeks (an old load double-wrote some East Coast rows)
connection.execute("""
    delete from gas_prices
    where rowid not in (select min(rowid) from gas_prices group by padd, period)
""")

connection.commit()
connection.close()
