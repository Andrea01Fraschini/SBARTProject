import geopandas as gpd
import shapely as shp
import pandas as pd
import matplotlib.pyplot as plt

map = gpd.read_file('./REGIONE_LOMBARDIA/Comuni_correnti_poligonali.shp')
map = map.to_crs({'init': 'epsg:4326'})
municipality_polygons = gpd.GeoDataFrame(map[["NOME_COM", "geometry"]], crs = map.crs)

wind_data = pd.read_csv("./wind_stuff/Aggregated_wind_data.csv")

grid_points=[]
for t in wind_data[["Longitude", "Latitude"]].itertuples():
    grid_points.append(shp.Point(t[1], t[2]))

grid = gpd.GeoDataFrame(geometry=grid_points, crs = 'epsg:4326')
fig, ax = plt.subplots(1, 1, figsize=(10, 10))

municipality_polygons.plot(ax=ax, edgecolor='black', linewidth=0.5)
grid.plot(ax=ax, color='red', markersize=5)

plt.show()
