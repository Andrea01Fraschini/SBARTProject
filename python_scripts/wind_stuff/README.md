# Wind data scripts

These python scripts allow to extract and do stuff with wind data from the AGC dataset. Since the original dataset is *way* too big, the already extracted *AGC_wind_data.csv* dataset is uploaded to the repo for quick execution.

The scripts and their functions are:

- **extract_wind_data.py**: extracts from *AGC_Dataset_v_3_0_0.csv* the columns used for wind analysis and creates the file *AGC_Wind_Data.csv*. This file should already be available in the repository so this python script should not be necessary.
- **convert_wind_data.py**: converts cathegorical data in *AGC_Wind_Data.csv* to numerical data for processing and creates the file *Converted_AGC_Wind_Data.csv*
- **aggregate_wind_data.py**: aggregates time series data from *Converted_AGC_Wind_Data.csv* into point data for each point in the grid and creates the file *Aggregated_wind_data.csv*

# Wind adjacency algorithm

It's an implementation of what Kim did in [the paper](https://doi.org/10.1080/00949655.2022.2102633). It builds an adjacency matrix in which adjacency between a given point and any other is considered present if the second point is inside an area determined by the prevailing wind speed at the first point and by an arbitrary angle parameter.

<img src="images/image.png" alt="algorithm diagram" style="
    width: 60%;
    max-width: 600px;
    display: block;
    margin-left: auto;
    margin-right: auto;
"/>

In the above image, if we consider 60° as the angle parameter, we'll have that _L_ is adjacent only to _B_ and _C_ (note that adjacency is a **symmetric** relationship). If we instead consider 75°, then _A_ and _D_ will **also** be considered adjacent to _L_. Note that in this case the distance between points is not considered. The distance matrix will be automatically selected by the SBART algorithm and used as a weight for this wind adjacency matrix.

To compute whether a point is in the cone of influence