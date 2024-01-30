import pandas as pd
import glob

def join_csv_files(input_prefix, output_file):
    # Get a list of all the part files
    all_filenames = [i for i in glob.glob(f'{input_prefix}*.csv')]

    # Combine all files in the list
    combined_df = pd.concat([pd.read_csv(f) for f in all_filenames])

    # Export to csv
    combined_df.to_csv(output_file, index=False, encoding='utf-8-sig')

input_prefix = "data/AgrImOnIA/raw/split_file"
output_file = "data/AgrImOnIA/raw/AGC_Dataset_v_3_0_0.csv"

join_csv_files(input_prefix, output_file)