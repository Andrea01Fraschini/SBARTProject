import geopandas as gpd
#import shapely as shp
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

ID = 378

map = gpd.read_file('./REGIONE_LOMBARDIA/Comuni_correnti_poligonali.shp') # load map of polygons
map = map.to_crs({'init': 'epsg:4326'}) # set a Coordinate Reference System
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

fig, ax = plt.subplots(2, 3, figsize=(10, 10))


for i in range(5):
    cost_matrix = pd.read_csv(f"./cost_matrices/cost_matrix_{i}.csv")
    m = cost_matrix.to_numpy()

    m = m[1:, 1:]

    distances_from_munic = m[ID]

    maximum_dist = max(distances_from_munic)
    #minimum_dist = min(distances_from_milan)
    colors = distances_from_munic
    row = i//3
    col = i%3
    municipality_polygons.plot(ax=ax[row][col], edgecolor='black', color="white", linewidth=0.5)
    centroids.plot(ax=ax[row][col], c=colors, markersize=5)

plt.show()


