import geopandas as gpd
import shapely as shp
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

map = gpd.read_file('./python_scripts/REGIONE_LOMBARDIA/Comuni_correnti_poligonali.shp')
map = map.to_crs({'init': 'epsg:4326'})
municipality_polygons = gpd.GeoDataFrame(map[["NOME_COM", "geometry"]], crs = map.crs)

wind_data = pd.read_csv("./python_scripts/wind_stuff/Aggregated_wind_data.csv")

max_speed = wind_data['WE_wind_speed_100m_mean'].max()
max_length = 50
arrows={'X':[], 'Y':[], 'U':[], 'V':[]}
for i, data in wind_data.iterrows():
    arrows['X'].append(data['Longitude'])
    arrows['Y'].append(data['Latitude'])
    
    #from polar to cartesian
    arg = data['WE_mode_wind_direction_100m']
    #radius = data['WE_wind_speed_100m_mean']/max_speed*max_length
    radius = data['WE_wind_speed_100m_mean']

    arrows['U'].append(np.cos(arg)*radius)
    arrows['V'].append(np.sin(arg)*radius)

#fig, ax = plt.subplots(1, 1, figsize=(10, 10))

ax = municipality_polygons.plot(edgecolor='black', color='white', linewidth=0.5)
#plt.quiver(arrows['positions'], arrows['directions'])
ax.quiver(arrows['X'],arrows['Y'],arrows['U'],arrows['V'], 
          color='red',
          headlength=10,
          headaxislength=10
)

plt.show()
