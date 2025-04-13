import pandas as pd
import anthropic
import time

# ============================================================================
# 1) Configuration
# ============================================================================
client = anthropic.Anthropic(
    api_key="your-anthropic-api-key-here"
)

MAX_RETRIES = 3

# ============================================================================
# 2) Claude call with retry logic
# ============================================================================
def get_country_one_word(publisher_name):
    """
    Calls Claude to return exactly one word (no spaces) for the publisher's country.
    If unknown, returns 'Unknown'.
    """
    system_prompt = (
        "You are a location extraction assistant. "
        "Return exactly one word (no spaces) indicating the country where the given publisher is based. "
        "Use ISO short forms if applicable (e.g., 'USA', 'UK'). "
        "If the location is unknown, return 'Unknown'. Do not include any explanation."
    )

    user_prompt = (
        f"Publisher: {publisher_name}\n\n"
        "Return the country of this publisher in one word. If unknown, return 'Unknown'."
    )

    for attempt in range(1, MAX_RETRIES + 1):
        try:
            response = client.messages.create(
                model="claude-3-sonnet-20240229",
                max_tokens=10,
                temperature=0.0,
                system=system_prompt,
                messages=[
                    {"role": "user", "content": user_prompt}
                ]
            )

            answer = response.content[0].text.strip()

            if " " in answer or not answer:
                return "Unknown"
            return answer

        except Exception as e:
            print(f"[Attempt {attempt}] Error: {e}")
            if attempt < MAX_RETRIES:
                time.sleep(3)
            else:
                print("Max retries reached. Returning 'Unknown'.")
                return "Unknown"

    return "Unknown"

# ============================================================================
# 3) Main Script
# ============================================================================
def main():
    df = pd.read_csv("Data.csv")

    if "source" not in df.columns:
        raise ValueError("Data.csv must contain a 'source' column for publisher names.")

    publishers = df["source"].dropna().unique().tolist()
    publisher_country_map = {}

    for pub_name in publishers:
        country = get_country_one_word(pub_name)
        publisher_country_map[pub_name] = country

    results = []
    for val in df["source"]:
        if pd.isna(val):
            results.append("Unknown")
        else:
            results.append(publisher_country_map[val])

    out_df = pd.DataFrame({
        "source": df["source"],
        "final_country": results
    })

    out_df.to_csv("publisher_locations.csv", index=False)
    print("Done! Wrote publisher_locations.csv")

if __name__ == "__main__":
    main()
