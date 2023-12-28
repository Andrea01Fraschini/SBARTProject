import numpy as np
from scipy import interpolate

def build_wind_dataframe(wind_data, altitude):
    if altitude not in {10, 100}:
        raise Exception('altitude must be 10 or 100')
    
    direction_column = f"prevalent_direction_{altitude}m"
    speed_column = f"average_speed_{altitude}m"

    wind_data = wind_data[['Longitude', 'Latitude', direction_column, speed_column]]
    
    known_wind={'x':[], 'y':[], 'W_x':[], 'W_y':[]}
    for i, data in wind_data.iterrows():
        known_wind['x'].append(data['Longitude'])
        known_wind['y'].append(data['Latitude'])
        
        #from polar to cartesian
        arg = data[direction_column]
        radius = data[speed_column]

        known_wind['W_x'].append(np.cos(arg)*radius)
        known_wind['W_y'].append(np.sin(arg)*radius)
    
    return known_wind
    
def build_wind_adjacency_matrix(centroids, wind_data, angle):
    if not (0 < angle < 90):
        raise Exception('angle must be between 0° and 90° both excluded')

    # build function for interpolating wind vectors    
    grid_points = list(zip(wind_data['x'], wind_data['y']))

    interpX = interpolate.LinearNDInterpolator(grid_points, wind_data['W_x'])
    interpY = interpolate.LinearNDInterpolator(grid_points, wind_data['W_y'])
    
    interpolate_wind = lambda x, y: (interpX(x, y), interpY(x, y))

    theta = angle*np.pi/180
    sin = np.sin(theta)
    cos = np.cos(theta)
    sin2 = np.sin(2*theta)
    
    N = len(centroids)

    # progress bar variables
    progress = 0.0
    todo_total = N**2
    progress_bar_length = 40 # in chars
    
    adjacency_matrix = np.eye(N = N, M = N)
    #adjacency_tuples = [(i, i) for i in range(N)]

    # using the same notation of the documentation
    for c in range(N):
        
        # reference point
        c_x = centroids[c].x
        c_y = centroids[c].y

        # interpolate wind vector
        W_x, W_y = interpolate_wind(c_x, c_y)

        # build the inverse matrix
        xc, yc, xs, ys = W_x*cos, W_y*cos, W_x*sin, W_y*sin

        M = np.matrix([
            [xs-yc, xc+ys],
            [xs+yc, ys-xc]
        ])

        determinant = -(W_x**2 + W_y**2)*sin2
        
        M = M/determinant 

        for d in [i for i in range(N) if i != c]:
            # progress bar code
            progress += 1
            print('[', end='')
            _ = int(progress/todo_total * progress_bar_length)
            print('#'*_ , end='')
            print('-'*(progress_bar_length-_) , end='')
            print(f'] {int(progress)}/{int(todo_total)}', end='\r')

            if adjacency_matrix[c][d] == 1: #skip if already adjacent
                continue
            
            # point to check if it's adjacent
            d_x = centroids[d].x
            d_y = centroids[d].y

            # translate to reference point
            x = d_x - c_x
            y = d_y - c_y

            d_p = np.array([x, y]) # d_p stands for "d prime" (d' in the documentation) 

            # solve linear system by left-multiplying the inverse matrix
            solution = M @ d_p

            # points are adjacent to one another if all components of the solution are non-negative
            if np.all(solution >= 0):
                # update adjacency matrix
                adjacency_matrix[c][d] = adjacency_matrix[d][c] = 1
                
                # adjacency_tuples.append((c, d))
                # adjacency_tuples.append((d, c))
            
    print("") # due to progress bar code

    #return adjacency_tuples, adjacency_matrix
    return adjacency_matrix
