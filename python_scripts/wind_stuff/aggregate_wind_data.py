import pandas as pd
import numpy as np

wind_data = pd.read_csv("./Converted_AGC_Wind_Data.csv")

wind_data = wind_data.groupby(['Latitude', 'Longitude'], as_index=False).agg(
    WE_wind_speed_10m_mean=('WE_wind_speed_10m_mean', 'mean'),
    WE_wind_speed_100m_mean=('WE_wind_speed_100m_mean', 'mean'),
    WE_mode_wind_direction_10m=('WE_mode_wind_direction_10m', 'mean'),
    WE_mode_wind_direction_100m=('WE_mode_wind_direction_100m', 'mean'))

wind_data.to_csv("./Aggregated_wind_data.csv", index=False)
print(wind_data.head())