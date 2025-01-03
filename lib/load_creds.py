import os

def load_env_credentials():
    """
    Sets variables in .env file for current session.
    :return: None
    """
    with open(r"C:\Users\brian\OneDrive\Desktop\git repos\gas_forecasting\.env", "r")as f:
        for line in f.readlines():
            try:
                key, value = line.split('=')
                os.environ[key] = value.strip()
            except ValueError:
                # syntax error
                pass