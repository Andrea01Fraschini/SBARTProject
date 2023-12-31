import pandas as pd
import numpy as np

wind_data = pd.read_csv("./python/wind_stuff/AGC_Wind_Data.csv")

cols_to_convert = ['WE_mode_wind_direction_100m','WE_mode_wind_direction_10m']

PI_QUARTERS = np.pi/4

mapping = {
    "N":  2*PI_QUARTERS,
    "NE": 1*PI_QUARTERS,
    "E":  0*PI_QUARTERS,     
    "SE": 7*PI_QUARTERS,
    "S":  6*PI_QUARTERS,
    "SW": 5*PI_QUARTERS,
    "W":  4*PI_QUARTERS,
    "NW": 3*PI_QUARTERS,
}

for c in cols_to_convert:
    wind_data[c] = wind_data[c].map(mapping)

# wind_data = wind_data.loc[(wind_data['Time'] >= '2016-06-01')
#                      & (wind_data['Time'] < '2016-06-20')]

wind_data.to_csv("./python/wind_stuff/Converted_AGC_Wind_Data.csv", index=False)

print(wind_data.head())