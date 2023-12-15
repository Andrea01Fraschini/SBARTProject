import geopandas as gpd
import shapely as shp
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

map = gpd.read_file('./python_scripts/REGIONE_LOMBARDIA/Comuni_correnti_poligonali.shp')
map = map.to_crs({'init': 'epsg:4326'})
municipality_polygons = gpd.GeoDataFrame(map[["NOME_COM", "geometry"]], crs = map.crs)

wind_data = pd.read_csv("./python_scripts/wind_stuff/Aggregated_wind_data.csv")

#Longitude, Latitude, prevalent_direction_100m, prevalent_direction_10m

arrows={'X':[], 'Y':[], 'U':[], 'V':[]}
for i, data in wind_data.iterrows():
    arrows['X'].append(data['Longitude'])
    arrows['Y'].append(data['Latitude'])
    
    #from polar to cartesian
    arg = data['prevalent_direction_100m']

    arrows['U'].append(np.cos(arg))
    arrows['V'].append(np.sin(arg))

#fig, ax = plt.subplots(1, 1, figsize=(10, 10))
ax = municipality_polygons.plot(edgecolor='black', color='white', linewidth=0.5)
#plt.quiver(arrows['positions'], arrows['directions'])
ax.quiver(arrows['X'],arrows['Y'],arrows['U'],arrows['V'], 
          color='red',
          pivot='mid',
          headlength=2,
          headaxislength=2
)

plt.show()
