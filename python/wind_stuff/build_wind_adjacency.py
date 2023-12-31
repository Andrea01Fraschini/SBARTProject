import wind_adjacency_builder as wab
import sys
import pandas as pd
import geopandas as gpd

if len(sys.argv) < 3:
    raise Exception("too few arguments!\nExample of correct command: 'build_wind_adjacency.py 100 60' for generating the wind adjacency matrix using 100m altitude and 60° angle parameter")

altitude = int(sys.argv[1])
angle = int(sys.argv[2])

wind_data = pd.read_csv("./python/wind_stuff/Aggregated_wind_data.csv")
centroids_pdf = pd.read_csv("./MunicipalitiesCentroids.csv") # read CSV as pandas dataframe

# convert dataframe into GeoDataFrame
centroids = gpd.GeoDataFrame(
    geometry=gpd.GeoSeries.from_wkt(centroids_pdf["geometry"]),
    crs="epsg:4326" # set Coordinate Reference System
)

centroids_array = centroids[['geometry']].to_numpy().flatten()

known_wind = wab.build_wind_dataframe(wind_data, altitude)

#tuples, matrix = wab.build_wind_adjacency_matrix(centroids_array, known_wind, angle=60)

matrix = wab.build_wind_adjacency_matrix(centroids_array, known_wind, angle)

#pd.DataFrame(tuples).to_csv("./python/adjacency_files/wind_adjacency_tuples_60b.csv", index=False)
pd.DataFrame(matrix).to_csv(f"./python/adjacency_files/wind_adjacency_matrix_{altitude}m_{angle}.csv", index=False)
