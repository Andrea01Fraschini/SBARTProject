import pandas as pd

agc_df = pd.read_csv("./AGC_Dataset_v_3_0_0.csv")

wind_data = pd.DataFrame(
    agc_df.loc[:, [c for c in agc_df.columns if c in {'Latitude', 'Longitude', 'Time'} or "wind" in c]],
)

wind_data = wind_data.dropna(how='any')

wind_data.to_csv("./AGC_Wind_Data.csv", index=False)

wind_data = pd.read_csv("./AGC_Wind_Data.csv")
print(wind_data.head())
