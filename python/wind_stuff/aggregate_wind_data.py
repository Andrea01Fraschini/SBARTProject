import pandas as pd
import numpy as np

wind_data = pd.read_csv("./python_scripts/wind_stuff/Converted_AGC_Wind_Data.csv")

# DIRTY AND QUICK AGGREGATION
# wind_data = wind_data.groupby(['Latitude', 'Longitude'], as_index=False).agg(
#     WE_wind_speed_10m_mean=('WE_wind_speed_10m_mean', 'mean'),
#     WE_wind_speed_100m_mean=('WE_wind_speed_100m_mean', 'mean'),
#     WE_mode_wind_direction_10m=('WE_mode_wind_direction_10m', 'mean'),
#     WE_mode_wind_direction_100m=('WE_mode_wind_direction_100m', 'mean'))

# CLEVERER AGGREGATION
groups = wind_data.groupby(['Longitude', 'Latitude'])

rows = []
for group in groups:
    lon, lat = group[0]
    group_data = group[1]

    speeds_10m = group_data['WE_wind_speed_10m_mean']
    speeds_100m = group_data['WE_wind_speed_100m_mean']
    
    average_speed_10m = np.average(speeds_10m)
    average_speed_100m = np.average(speeds_100m)

    directions_10m = group_data['WE_mode_wind_direction_10m']
    directions_100m = group_data['WE_mode_wind_direction_100m']

    avg_direction_10m = np.average(directions_10m, weights=speeds_10m)
    avg_direction_100m = np.average(directions_100m, weights=speeds_100m)
    
    new_row = {'Longitude': lon, 'Latitude': lat, 
               'prevalent_direction_100m': avg_direction_100m, 
               'average_speed_100m':average_speed_100m, 
               'prevalent_direction_10m':avg_direction_10m, 
               'average_speed_10m':average_speed_10m, 
               }
    rows.append(new_row)

aggregated_data = pd.DataFrame(rows, columns=['Longitude', 'Latitude', 
                                              'prevalent_direction_100m',
                                              'average_speed_100m',                                               
                                              'prevalent_direction_10m',
                                              'average_speed_10m'])

print(aggregated_data.head())

aggregated_data.to_csv("./python_scripts/wind_stuff/Aggregated_wind_data.csv", index=False)