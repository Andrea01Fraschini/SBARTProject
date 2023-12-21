import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

map = gpd.read_file('./python_scripts/REGIONE_LOMBARDIA/Comuni_correnti_poligonali.shp')
map = map.to_crs({'init': 'epsg:4326'})
municipality_polygons = gpd.GeoDataFrame(map[["NOME_COM", "geometry"]], crs = map.crs)

wind_data = pd.read_csv("./python_scripts/wind_stuff/Aggregated_wind_data.csv")

arrows={'X':[], 'Y':[], 'U':[], 'V':[]}
for i, data in wind_data.iterrows():
    arrows['X'].append(data['Longitude'])
    arrows['Y'].append(data['Latitude'])
    
    #from polar to cartesian
    arg = data['prevalent_direction_100m']
    radius = data['average_speed_100m']

    arrows['U'].append(np.cos(arg)*radius)
    arrows['V'].append(np.sin(arg)*radius)

#fig, ax = plt.subplots(1, 1, figsize=(10, 10))
bg_color = tuple([0.97]*3)
ax = municipality_polygons.plot(edgecolor='red', alpha=1, color=bg_color, linewidth=0.2)
ax.set_facecolor(bg_color)

u = np.array(arrows['U'])
v = np.array(arrows['V'])

M = np.sqrt(u*u+v*v) # magnitude

ax.quiver(arrows['X'],arrows['Y'],arrows['U'],arrows['V'], 
          M,
          cmap='jet',
          pivot='mid',
          headlength=2,
          headaxislength=2,
          width = 0.004
)
# X = np.linspace(min(arrows['X']), max(arrows['X']), 200)
# Y = np.linspace(min(arrows['Y']), max(arrows['Y']), 200)

# x, y = np.meshgrid(X, Y)

# # Interpolate the velocities on the meshgrid to avoid missing data problems
# u = np.interp(x, arrows['X'], arrows['U'])
# v = np.interp(y, arrows['Y'], arrows['V'])


# ax.streamplot(x, y, u, v, density=2.5, linewidth=0.75, color='red', arrowstyle='->', arrowsize=1)


plt.show()
