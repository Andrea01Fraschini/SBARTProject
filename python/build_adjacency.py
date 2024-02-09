import geopandas as gpd
import pandas as pd
import numpy as np
from shapely import affinity

def build(v, grid=False):
    N = len(v)
    
    # progress bar variables
    progress = 0.0
    todo_total = float(N*(N-1)/2)
    progress_bar_length = 40 # in chars
    
    adjacency_matrix = np.eye(N = N, M = N)
    adjacency_tuples = [(i, i) for i in range(N)]

    # it does only the N(N-1)/2 necessary checks
    for i in range(N-1):

        # if the areas are in a grid, scale the polygons 'a bit' to avoid
        # the possibility of having adjacent areas that do not intersect each other
        if(grid):
            v[i] = v[i].buffer(0.00001, join_style=2)
        
        for j in range(i + 1, N):

            if(grid):
                v[j] = v[j].buffer(0.00001, join_style=2)

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

map = gpd.read_file('./AreasGeometriesWithNames.csv', crs = 'epsg:4326', GEOM_POSSIBLE_NAMES='geometry', KEEP_GEOM_COLUMNS='NO') # load file as a geodataframe
print(map.head())
map = map[["geometry"]] # select geometry column
array = map.to_numpy().flatten() # convert to numpy array

adjacency_tuples, adjacency_m = build(array, grid=True)

pd.DataFrame(adjacency_tuples).to_csv("./adjacency_files/adjacency_tuples.csv", index=False)
pd.DataFrame(adjacency_m).to_csv("./adjacency_files/adjacency_matrix.csv", index=False)