import geopandas as gpd
from geopandas.tools import sjoin
import shapely as shp
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


# centroids_pdf = pd.read_csv(".\MunicipalitiesCentroids.csv")

# centroids_filtered = centroids_pdf[centroids_pdf.MUNICIPALITY_NAME != "ALBAREDO ARNABOLDI"]
# centroids_filtered = centroids_filtered.loc[:, ~centroids_filtered.columns.str.contains('^Unnamed')]
# centroids_filtered.to_csv(".\MunicipalitiesCentroids_no_Albaredo.csv", index=False)

# centroids_gpdf = gpd.read_file(".\MunicipalitiesCentroids_no_Albaredo.csv", crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO')

# bounding_box = shp.box(8, 44.5, 10, 45.6)
# box_df = pd.DataFrame([bounding_box], columns=['geometry'])

# print(box_df.head())
# box_gpdf = gpd.GeoDataFrame(box_df, crs = 'epsg:4326')
# box_gpdf.crs = centroids_gpdf.crs
# # ax = centroids_gpdf.plot(color="green")
# # box_gpdf.plot(ax=ax)

# centroids_moncati = sjoin(centroids_gpdf, box_gpdf)
# centroids_moncati = centroids_moncati.drop(['index_right'], axis=1)
# print(centroids_moncati.columns)
# centroids_moncati.to_csv(".\BringMeToLife_Evanescence_(2003).csv", index=False)

# centroids_moncati_pdf = pd.read_csv(".\BringMeToLife_Evanescence_(2003).csv")
# centroids_moncati_pdf.to_csv(".\MunicipalitiesCentroid_REDUCED.csv")


# REORDER THE GRID AND NAME THE SQUARES

# centroids_gpdf = gpd.read_file(".\sf_squares\sf_squares.shp", crs = 'epsg:4326')
# centroids_gpdf = centroids_gpdf.sort_values(["Longitude", "Latitude"], ascending=[True, False])
# points = centroids_gpdf[["Longitude", "Latitude"]]
# points_geometry = shp.points(points.to_numpy())

# names = ["SQ"+str(i) for i in range(len(points_geometry))]

# centroids_gpdf.insert(1, "points", points_geometry, True)
# centroids_gpdf.insert(1, "MUNICIPALITY_NAME", names, True)

# centroids_gpdf = centroids_gpdf[["MUNICIPALITY_NAME", "points", "geometry"]]
# centroids_gpdf.to_csv(".\OrderedGrid.csv", index=False)

# centroids_gpdf = gpd.read_file(".\OrderedGrid.csv" ,crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO')

# print(centroids_gpdf.head())

# # CREATE GRIDDED LOMBARDY

# region = gpd.read_file(".\REGIONE_LOMBARDIA\Regione_correnti.shp")
# region = region.to_crs('epsg:4326')

# grid = gpd.read_file(".\OrderedGrid.csv", crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO')
# grid.crs = region.crs

# region = region[['geometry']]

# intersection = gpd.overlay(region, grid, how='intersection')

# intersection.to_csv(".\Gridded_Lombardy.csv", index=False)

# # intersection.plot(edgecolor='black')
# # plt.show()

# GET MUNICIPALITIES WITH STATIONS

# stations_locations = [('SONDRIO', 'LOMBARDIA'),
#  ('LODI', 'LOMBARDIA'),
#  ('BERTONICO', 'LOMBARDIA'),
#  ('DALMINE', 'LOMBARDIA'),
#  ('PERLEDO', 'LOMBARDIA'),
#  ('SPINADESCO', 'LOMBARDIA'),
#  ('MONZA', 'LOMBARDIA'),
#  ('BRESCIA', 'LOMBARDIA'),
#  ('BRESCIA', 'LOMBARDIA'),
#  ('MILANO', 'LOMBARDIA'),
#  ('SESTO SAN GIOVANNI', 'LOMBARDIA'),
#  ('RHO', 'LOMBARDIA'),
#  ('TURBIGO', 'LOMBARDIA'),
#  ('MILANO', 'LOMBARDIA'),
#  ('CINISELLO BALSAMO', 'LOMBARDIA'),
#  ('PIOLTELLO', 'LOMBARDIA'),
#  ('MILANO', 'LOMBARDIA'),
#  ('MEDA', 'LOMBARDIA'),
#  ('CORMANO', 'LOMBARDIA'),
#  ('MOTTA VISCONTI', 'LOMBARDIA'),
#  ('MAGENTA', 'LOMBARDIA'),
#  ('MILANO', 'LOMBARDIA'),
#  ('ARCONATE', 'LOMBARDIA'),
#  ('VARESE', 'LOMBARDIA'),
#  ('SARONNO', 'LOMBARDIA'),
#  ('BUSTO ARSIZIO', 'LOMBARDIA'),
#  ('VARESE', 'LOMBARDIA'),
#  ('COMO', 'LOMBARDIA'),
#  ('ERBA', 'LOMBARDIA'),
#  ("CANTU'", 'LOMBARDIA'),
#  ('SONDRIO', 'LOMBARDIA'),
#  ('BORMIO', 'LOMBARDIA'),
#  ('MORBEGNO', 'LOMBARDIA'),
#  ('COLICO', 'LOMBARDIA'),
#  ('LECCO', 'LOMBARDIA'),
#  ('MERATE', 'LOMBARDIA'),
#  ('BERGAMO', 'LOMBARDIA'),
#  ('BERGAMO', 'LOMBARDIA'),
#  ('TAVERNOLA BERGAMASCA', 'LOMBARDIA'),
#  ('TREVIGLIO', 'LOMBARDIA'),
#  ('FILAGO', 'LOMBARDIA'),
#  ('OSIO SOTTO', 'LOMBARDIA'),
#  ('SAN ROCCO AL PORTO', 'LOMBARDIA'),
#  ('LODI', 'LOMBARDIA'),
#  ('MELEGNANO', 'LOMBARDIA'),
#  ('CASTIRAGA VIDARDO', 'LOMBARDIA'),
#  ('TAVAZZANO CON VILLAVESCO', 'LOMBARDIA'),
#  ('SAN GIULIANO MILANESE', 'LOMBARDIA'),
#  ('ABBADIA CERRETO', 'LOMBARDIA'),
#  ('CODOGNO', 'LOMBARDIA'),
#  ("CASIRATE D'ADDA", 'LOMBARDIA'),
#  ("CORTE DE' CORTESI CON CIGNONE", 'LOMBARDIA'),
#  ('CREMONA', 'LOMBARDIA'),
#  ('CREMA', 'LOMBARDIA'),
#  ('SORESINA', 'LOMBARDIA'),
#  ('PAVIA', 'LOMBARDIA'),
#  ('PAVIA', 'LOMBARDIA'),
#  ('BRESCIA', 'LOMBARDIA'),
#  ('BRESCIA', 'LOMBARDIA'),
#  ('SAREZZO', 'LOMBARDIA'),
#  ('DARFO BOARIO TERME', 'LOMBARDIA'),
#  ('GAMBARA', 'LOMBARDIA'),
#  ('LONATO DEL GARDA', 'LOMBARDIA'),
#  ('ODOLO', 'LOMBARDIA'),
#  ('REZZATO', 'LOMBARDIA'),
#  ('MANTOVA', 'LOMBARDIA'),
#  ('MANTOVA', 'LOMBARDIA'),
#  ('VIADANA', 'LOMBARDIA'),
#  ('BRESCIA', 'LOMBARDIA'),
#  ('MANTOVA', 'LOMBARDIA'),
#  ('CORNALE E BASTIDA', 'LOMBARDIA'),
#  ('VOGHERA', 'LOMBARDIA'),
#  ('MONZA', 'LOMBARDIA'),
#  ('CREMONA', 'LOMBARDIA'),
#  ('VALMADRERA', 'LOMBARDIA'),
#  ('MOGGIO', 'LOMBARDIA'),
#  ('FERRERA ERBOGNONE', 'LOMBARDIA'),
#  ("CASSANO D'ADDA", 'LOMBARDIA'),
#  ("CALUSCO D'ADDA", 'LOMBARDIA'),
#  ('FERNO', 'LOMBARDIA'),
#  ('MEZZANA BIGLI', 'LOMBARDIA'),
#  ("SANNAZZARO DE' BURGONDI", 'LOMBARDIA'),
#  ('VALEGGIO SUL MINCIO', 'VENETO'),
#  ('PONTI SUL MINCIO', 'LOMBARDIA'),
#  ('MONZAMBANO', 'LOMBARDIA'),
#  ('BORGOCARBONARA', 'LOMBARDIA'),
#  ('CENESELLI', 'VENETO'),
#  ('MAGNACAVALLO', 'LOMBARDIA'),
#  ('MELARA', 'VENETO'),
#  ('BORGO MANTOVANO', 'LOMBARDIA'),
#  ('SCHIVENOGLIA', 'LOMBARDIA'),
#  ('MILANO', 'LOMBARDIA'),
#  ('LECCO', 'LOMBARDIA'),
#  ('MORTARA', 'LOMBARDIA'),
#  ('PARONA', 'LOMBARDIA'),
#  ('VIGEVANO', 'LOMBARDIA'),
#  ('LUGANO', 'TICINO'),
#  ('CADENAZZO', 'TICINO'),
#  ('LUGANO', 'TICINO'),
#  ('FERRARA', 'EMILIA-ROMAGNA'),
#  ('LATSCH - LACES', 'TRENTINO-ALTO ADIGE/SÜDTIROL'),
#  ("LUGAGNANO VAL D'ARDA", 'EMILIA-ROMAGNA'),
#  ('ROVERETO', 'TRENTINO-ALTO ADIGE/SÜDTIROL'),
#  ('ALESSANDRIA', 'PIEMONTE'),
#  ('MODENA', 'EMILIA-ROMAGNA'),
#  ('RIVA DEL GARDA', 'TRENTINO-ALTO ADIGE/SÜDTIROL'),
#  ('PARMA', 'EMILIA-ROMAGNA'),
#  ('REGGIO EMILIA', 'EMILIA-ROMAGNA'),
#  ('BUSALLA', 'LIGURIA'),
#  ('PARMA', 'EMILIA-ROMAGNA'),
#  ('CARPI', 'EMILIA-ROMAGNA'),
#  ('VALLELAGHI', 'TRENTINO-ALTO ADIGE/SÜDTIROL'),
#  ('VERONA', 'VENETO'),
#  ('CERANO', 'PIEMONTE'),
#  ('VERBANIA', 'PIEMONTE'),
#  ('NOVARA', 'PIEMONTE'),
#  ('BORGOSESIA', 'PIEMONTE'),
#  ('VERCELLI', 'PIEMONTE'),
#  ('LEGNAGO', 'VENETO'),
#  ('CASALE MONFERRATO', 'PIEMONTE'),
#  ('NOVARA', 'PIEMONTE'),
#  ('BUSALLA', 'LIGURIA'),
#  ('MODENA', 'EMILIA-ROMAGNA'),
#  ('ALESSANDRIA', 'PIEMONTE'),
#  ('VERCELLI', 'PIEMONTE'),
#  ('DERNICE', 'PIEMONTE'),
#  ('COLORNO', 'EMILIA-ROMAGNA'),
#  ('GUASTALLA', 'EMILIA-ROMAGNA'),
#  ('CENTO', 'EMILIA-ROMAGNA'),
#  ('FERRARA', 'EMILIA-ROMAGNA'),
#  ('MIRANDOLA', 'EMILIA-ROMAGNA'),
#  ('PIACENZA', 'EMILIA-ROMAGNA'),
#  ('BESENZONE', 'EMILIA-ROMAGNA'),
#  ('PIACENZA', 'EMILIA-ROMAGNA'),
#  ('BADIA POLESINE', 'VENETO'),
#  ('CORTE BRUGNATELLA', 'EMILIA-ROMAGNA'),
#  ('OMEGNA', 'PIEMONTE'),
#  ('BORGOMANERO', 'PIEMONTE'),
#  ('REGGIO EMILIA', 'EMILIA-ROMAGNA'),
#  ('VERONA', 'VENETO'),
#  ('NOVARA', 'PIEMONTE')]

# lombard_stations = [munic for munic, reg in stations_locations if reg =='LOMBARDIA']
# lombard_stations = list(set(lombard_stations))

# # Filter the centroids file
# munic_centroids = pd.read_csv(".\MunicipalitiesCentroids.csv")
# munic_centroids = munic_centroids.loc[:, ~munic_centroids.columns.str.contains('^Unnamed')]
# filter = munic_centroids['MUNICIPALITY_NAME'].isin(lombard_stations)
# munic_match = munic_centroids[filter]

# assert len(munic_match) == len(lombard_stations) # we do a lil' check
# print(munic_match.head())

# munic_match.to_csv(".\StationMunicipalitiesCentroids.csv")



# Load municipalities polygons
# munic_poly = gpd.read_file(".\REGIONE_LOMBARDIA\Comuni_correnti_poligonali.shp")
# munic_poly = munic_poly.to_crs('epsg:4326')
# munic_poly = munic_poly[['geometry']]

# munics = gpd.read_file(".\StationMunicipalitiesCentroids.csv", crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO')

# cannot select by intersection, but i kept the indeces

# munics_indeces = munics['field_1'].to_numpy()
# names = munics['MUNICIPALITY_NAME'].to_numpy()

# munics_with_stations_poly = munic_poly.iloc[munics_indeces]
# munics_with_stations_poly.insert(0, 'MUNICIPALITY_NAME', names, True)
# munics_with_stations_poly.to_csv(".\MunicipalitiesWithStationsPolygon.csv", index=False)


# FINALLY PUT EVERYTHING TOGETHER

# munics_with_stations_poly = gpd.read_file(".\MunicipalitiesWithStationsPolygon.csv", crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO')

# gridded_lombardy = gpd.read_file(".\Gridded_Lombardy.csv", crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO')

# grid_with_holes = gpd.overlay(gridded_lombardy, munics_with_stations_poly, how="difference")

# merged = gpd.overlay(grid_with_holes, munics_with_stations_poly, how='union')

# squares_names = merged['MUNICIPALITY_NAME_1'].to_numpy()
# munic_names = merged['MUNICIPALITY_NAME_2'].to_numpy()

# merged_names = []
# for s, m in zip(squares_names, munic_names):
#     if(type(s) == str):
#         merged_names.append(s)
#     else:
#         merged_names.append(m)

# assert len(merged_names) == len(munic_names) == len(squares_names)

# merged = merged[["geometry"]]
# merged.insert(0, 'MUNICIPALITY_NAME', merged_names)

# centroids = merged["geometry"].centroid
# centroids_gdf = gpd.GeoDataFrame({'geometry':centroids})

# centroids_gdf.insert(0, 'MUNICIPALITY_NAME', merged_names)

# centroids_gdf.to_csv(".\AreasCentroids.csv")


# FINAL TEST

# map = gpd.read_file(".\AreasGeometriesWithNames.csv", crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO')
# map.crs = 'epsg:4326'
# centroids = gpd.read_file(".\AreasCentroids.csv", crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO')

# centroids.crs = map.crs
# ax = map.plot(color="white", edgecolor='black')
# centroids.plot(ax=ax, color="red", markersize=2)
# plt.show()