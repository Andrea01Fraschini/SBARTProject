import geopandas as gpd
import pandas as pd
import numpy as np

def build(v):
    N = len(v)
    
    # progress bar variables
    progress = 0.0
    todo_total = float(N*(N-1)/2)
    progress_bar_length = 40 # in chars
    
    adjacency_matrix = np.eye(N = N, M = N)
    adjacency_tuples = [(i, i) for i in range(N)]

    # it does only the N(N-1)/2 necessary checks
    for i in range(N-1):
        for j in range(i + 1, N):
            are_adjacent = not v[i].disjoint(v[j])

            # update adjacency matrix
            adjacency_matrix[i][j] = adjacency_matrix[j][i] = are_adjacent
            
            # update adjacency list
            if are_adjacent:
                adjacency_tuples.append((i, j))
                adjacency_tuples.append((j, i))
            
            
            # progress bar code
            progress += 1
            print('[', end='')
            _ = int(progress/todo_total * progress_bar_length)
            print('#'*_ , end='')
            print('-'*(progress_bar_length-_) , end='')
            print(f'] {int(progress)}/{int(todo_total)}', end='\r')
    
    print("") # due to progress bar code

    return adjacency_tuples, adjacency_matrix




map = gpd.read_file('./REGIONE_LOMBARDIA/Comuni_correnti_poligonali.shp') # load file as a geodataframe
map = map.to_crs({'init': 'epsg:4326'}) # set a Coordinate Reference System
map = map[["geometry"]] # select geometry column
array = map.to_numpy().flatten() # convert to numpy array

adjacency_tuples, adjacency_m = build(array)

pd.DataFrame(adjacency_tuples).to_csv("./adjacency_files/adjacency_tuples.csv")
pd.DataFrame(adjacency_m).to_csv("./adjacency_files/adjacency_matrix.csv")