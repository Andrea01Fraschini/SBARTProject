import wind_adjacency_builder as wab
import pandas as pd
import geopandas as gpd


wind_data = pd.read_csv("./python_scripts/wind_stuff/Aggregated_wind_data.csv")
centroids_pdf = pd.read_csv("./MunicipalitiesCentroids.csv") # read CSV as pandas dataframe

# convert dataframe into GeoDataFrame
centroids = gpd.GeoDataFrame(
    # select all columns apart from the "geometry" column
    centroids_pdf.loc[:, [c for c in centroids_pdf.columns if c != "geometry"]],
    # use column "geometry" for geometries
    geometry=gpd.GeoSeries.from_wkt(centroids_pdf["geometry"]),
    crs="epsg:4326" # set Coordinate Reference System
)


known_wind = wab.build_wind_dataframe(wind_data, altitude=100)
tuples, matrix = wab.build_wind_adjacency_matrix(centroids, known_wind, angle=60)

pd.DataFrame(tuples).to_csv(".python_scripts/adjacency_files/wind_adjacency_tuples_60.csv", index=False)
pd.DataFrame(matrix).to_csv(".python_scripts/adjacency_files/adjacency_matrix_60.csv", index=False)
