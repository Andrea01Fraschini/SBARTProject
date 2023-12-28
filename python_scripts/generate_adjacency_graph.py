import geopandas as gpd
import shapely as shp
import pandas as pd
import matplotlib.pyplot as plt

centroids_pdf = pd.read_csv("./MunicipalitiesCentroids.csv") # read CSV as pandas dataframe

# convert dataframe into GeoDataFrame
centroids = gpd.GeoDataFrame(
    # select all columns apart from the "geometry" column
    centroids_pdf.loc[:, [c for c in centroids_pdf.columns if c != "geometry"]],
    # use column "geometry" for geometries
    geometry=gpd.GeoSeries.from_wkt(centroids_pdf["geometry"]),
    crs="epsg:4326" # set Coordinate Reference System
)

map = gpd.read_file('./python_scripts/REGIONE_LOMBARDIA/Comuni_correnti_poligonali.shp') # load map of polygons
map = map.to_crs({'init': 'epsg:4326'}) # set a Coordinate Reference System

# convert into geodataframe
municipality_polygons = gpd.GeoDataFrame(map[["NOME_COM", "geometry"]], crs = map.crs)

adjacency_tuples = pd.read_csv("./python_scripts/adjacency_files/adjacency_tuples.csv")

v = adjacency_tuples.to_numpy()
N = len(municipality_polygons)
v = v[N:] # ignore the (a, a) tuples
v = v[::2] # select (a, b) and skip the successive (b, a)

lines_geometry = []
for _, i, j in v: # _ is the enumeration index, i and j form the tuple (i, j)
    point1 = centroids.iloc[i]['geometry']
    point2 = centroids.iloc[j]['geometry']
    lines_geometry.append(shp.LineString((point1, point2)))

# wrap lines in a geodataframe for plotting
lines = gpd.GeoDataFrame(geometry=lines_geometry)

fig, ax = plt.subplots(1, 1, figsize=(10, 10))

municipality_polygons.plot(ax=ax, edgecolor='black', linewidth=0.5)
centroids.plot(ax=ax, color='red', markersize=2)
lines.plot(ax=ax, color='red', linewidth=1);

plt.show()

