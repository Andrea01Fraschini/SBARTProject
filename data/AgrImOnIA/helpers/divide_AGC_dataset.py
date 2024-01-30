import pandas as pd
import os
def split_csv_file(input_file, output_prefix, rows_per_file):
    df = pd.read_csv(input_file)
    total_rows = len(df)
    num_files = total_rows // rows_per_file + 1

    for i in range(num_files):
        start_row = i * rows_per_file
        end_row = min((i + 1) * rows_per_file, total_rows)
        output_file = f"{output_prefix}_{i}.csv"
        df[start_row:end_row].to_csv(output_file, index=False)

# Usage example
input_file = "data/AgrImOnIA/raw/AGC_Dataset_v_3_0_0.csv"
output_prefix = "data/AgrImOnIA/raw/split_file"
rows_per_file = 100000

split_csv_file(input_file, output_prefix, rows_per_file)
