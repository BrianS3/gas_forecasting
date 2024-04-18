
# EIA Gasoline Price Data ETL Pipeline

This repository contains the code for setting up an Extract, Transform, Load (ETL) pipeline using the U.S. Energy Information Administration (EIA) API to gather and analyze gasoline price data. The project aims to facilitate exploratory analysis and time-series modeling to forecast gasoline prices.

## Introduction

The U.S. Energy Information Administration (EIA) provides critical, impartial energy information that aids in informed decision-making. This project leverages the EIA's API to access gasoline price data, which is essential for creating predictive models and gaining deeper insights into energy trends.

## Getting Started

### Prerequisites

- Python 3.x
- R (for exploratory analysis)
- Jupyter Notebook
- SQLite

### Setup

1. Clone the repository:
   git clone https://github.com/yourusername/eia-gasoline-etl.git
2. Install required Python packages:
   pip install -r requirements.txt
3. Set up your environment variables in a `.env` file to store API credentials securely.

### Structure

- `/notebooks` - Jupyter notebooks for API testing and exploratory analysis.
- `/src` - Python scripts for the ETL process.
- `/data` - Directory for storing extracted and transformed data.

## Usage

Run the main script to start the ETL process:
```
python src/main.py
```

## Features

- **Data Collection**: Fetches data from the EIA API.
- **Data Transformation**: Structures data for analysis and stores it in a SQLite database.
- **Logging**: Implements logging for transparency and troubleshooting.

## Contributing

Feel free to fork this repository and submit pull requests. You can also open issues if you find any bugs or have feature requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.
