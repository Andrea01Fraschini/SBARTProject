import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

map = gpd.read_file('./python_scripts/REGIONE_LOMBARDIA/Comuni_correnti_poligonali.shp')
map = map.to_crs({'init': 'epsg:4326'})
municipality_polygons = gpd.GeoDataFrame(map[["NOME_COM", "geometry"]], crs = map.crs)

centroids_pdf = pd.read_csv("./MunicipalitiesCentroids.csv") # read CSV as pandas dataframe
# convert dataframe into GeoDataFrame
centroids = gpd.GeoDataFrame(
    # select all columns apart from the "geometry" column
    centroids_pdf.loc[:, [c for c in centroids_pdf.columns if c != "geometry"]],
    # use column "geometry" for geometries
    geometry=gpd.GeoSeries.from_wkt(centroids_pdf["geometry"]),
    crs="epsg:4326" # set Coordinate Reference System
)

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
ax = municipality_polygons.plot(edgecolor='black', alpha=1, color=bg_color, linewidth=0.2)
ax.set_facecolor(bg_color)

u = np.array(arrows['U'])
v = np.array(arrows['V'])

M = np.sqrt(u*u+v*v) # magnitude

ax.quiver(arrows['X'],arrows['Y'],arrows['U'],arrows['V'], 
          #M,
          alpha=0.5,
          #cmap='jet',
          pivot='mid',
          headlength=2,
          headaxislength=2,
          width = 0.004
)

#ID = 610 #sondrio
#ID = 183 #milano
ID = 1279 #cremona

adjacency_matrix = pd.read_csv("./python_scripts/adjacency_files/wind_adjacency_matrix_30.csv").to_numpy()
color_list = []

for adj in adjacency_matrix[ID]:
    # isAdjacent = adj == 1 
    # color_list.append('red' if isAdjacent else 'black')
    color_list.append(adj == 1)

centroids['color']=color_list
#centroids = centroids.assign(color=color_list)
centroids.plot(ax=ax, markersize=10, column="color", cmap='seismic')

# X = np.linspace(min(arrows['X']), max(arrows['X']), 200)
# Y = np.linspace(min(arrows['Y']), max(arrows['Y']), 200)

# x, y = np.meshgrid(X, Y)

# # Interpolate the velocities on the meshgrid to avoid missing data problems
# u = np.interp(x, arrows['X'], arrows['U'])
# v = np.interp(y, arrows['Y'], arrows['V'])


# ax.streamplot(x, y, u, v, density=2.5, linewidth=0.75, color='red', arrowstyle='->', arrowsize=1)


plt.show()
