import pandas as pd

wind_data = pd.read_csv("./AGC_Wind_Data.csv")

cols_to_convert = ['WE_mode_wind_direction_100m','WE_mode_wind_direction_10m']

mapping = {
    "N":    0.0,
    "NE":  45.0,
    "E":   90.0,     
    "SE": 135.0,
    "S":  180.0,
    "SW": 225.0,
    "W":  270.0,
    "NW": 315.0
}

for c in cols_to_convert:
    wind_data[c] = wind_data[c].map(mapping)

wind_data.to_csv("./Converted_AGC_Wind_Data.csv", index=False)

print(wind_data.head())