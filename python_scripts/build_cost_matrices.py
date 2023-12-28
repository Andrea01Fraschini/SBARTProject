import geopandas as gpd
import pandas as pd
import numpy as np

albaredo_arnaboldi_index = 1154

def build(centroids, functions):
    N = len(centroids)
    
    # progress bar variables
    progress = 0.0
    todo_total = float(N*(N-1)/2)
    progress_bar_length = 40 # in chars
    
    number_of_functions = len(functions)
    cost_matrices = []
    for _ in functions:
        cost_matrices.append(np.zeros((N, N)))

    # it does only the N(N-1)/2 necessary checks
    for i in range(N-1):
        for j in range(i + 1, N):
            
            if albaredo_arnaboldi_index in (i, j):
                continue

            point1 = centroids.iloc[i]['geometry']
            point2 = centroids.iloc[j]['geometry']

            d = point1.distance(point2)

            for f_i, f in enumerate(functions):
                cost_matrices[f_i][i][j] = f(d)
                cost_matrices[f_i][j][i] = f(d)
            
            # progress bar code
            progress += 1
            print('[', end='')
            _ = int(progress/todo_total * progress_bar_length)
            print('#'*_ , end='')
            print('-'*(progress_bar_length-_) , end='')
            print(f'] {int(progress)}/{int(todo_total)}', end='\r')
    
    print("") # due to progress bar code

    return cost_matrices




centroids_pdf = pd.read_csv("./MunicipalitiesCentroids.csv") # read CSV as pandas dataframe

# convert dataframe into GeoDataFrame
centroids = gpd.GeoDataFrame(
    # select all columns apart from the "geometry" column
    centroids_pdf.loc[:, [c for c in centroids_pdf.columns if c != "geometry"]],
    # use column "geometry" for geometries
    geometry=gpd.GeoSeries.from_wkt(centroids_pdf["geometry"]),
    crs="epsg:4326" # set Coordinate Reference System
)

functions = [
    lambda d: 1/(d**0.5),
    lambda d: 1/d,
    lambda d: 1/(d**1.5),
    lambda d: 1/(d**2),
    lambda d: np.exp(-d)
]

matrices = build(centroids, functions)

for i, m in enumerate(matrices):
    pd.DataFrame(m).to_csv(f"./python_scripts/cost_matrices/cost_matrix_{i}.csv", index=False)