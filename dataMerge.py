import pandas as pd

def main():
    # Load the original dataset
    df_data = pd.read_csv("Data.csv")

    # Load the publisher location dataset
    df_loc = pd.read_csv("publisher_locations.csv")

    # Rename 'final_country' to 'location'
    df_loc.rename(columns={"final_country": "location"}, inplace=True)

    # Deduplicate publisher entries to ensure clean join
    df_loc = df_loc.drop_duplicates(subset="source", keep="first")

    # Merge on 'source'
    merged_df = df_data.merge(df_loc[["source", "location"]], on="source", how="left")

    # Drop rows where location is missing or 'Unknown'
    filtered_df = merged_df[merged_df["location"].notna() & (merged_df["location"] != "Unknown")]

    # Optional: Drop duplicate rows in the final result, if you want unique headlines only
    filtered_df = filtered_df.drop_duplicates()

    # Save the cleaned and filtered dataset
    filtered_df.to_csv("Data_with_location.csv", index=False)
    print("Done! Cleaned and saved to Data_with_location.csv")

if __name__ == "__main__":
    main()
