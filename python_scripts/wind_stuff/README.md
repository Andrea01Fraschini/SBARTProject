# Wind data scripts

These python scripts allow to extract and do stuff with wind data from the AGC dataset. Since the original dataset is *way* too big, the already extracted *AGC_wind_data.csv* dataset is uploaded to the repo for quick execution.

The scripts and their functions are:

- **extract_wind_data.py**: extracts from *AGC_Dataset_v_3_0_0.csv* the columns used for wind analysis and creates the file *AGC_Wind_Data.csv*. This file should already be available in the repository so this python script should not be necessary.
- **convert_wind_data.py**: converts cathegorical data in *AGC_Wind_Data.csv* to numerical data for processing and creates the file *Converted_AGC_Wind_Data.csv*
- **aggregate_wind_data.py**: aggregates time series data from *Converted_AGC_Wind_Data.csv* into point data for each point in the grid and creates the file *Aggregated_wind_data.csv*

The other scripts are for other stuff idk we'll see.
