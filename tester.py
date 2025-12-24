import asyncio
import httpx
import time
import json
import numpy as np
import polars as pl

# --- CONFIGURATION ---
BASE_URL = "http://localhost:8002"
# Sample Rhode Island coordinates (Providence/Newport area)
LOCS = [
    {"lat": 41.8193, "lon": -71.4221}, # I-95 North
    {"lat": 41.8288, "lon": -71.4025}  # Near Brown University
]

async def get_matrix(client: httpx.AsyncClient, sources, targets, radius=100):
    payload = {
        "sources": [{**s, "search_radius": radius} for s in sources],
        "targets": [{**t, "search_radius": radius} for t in targets],
        "costing": "auto",
        "costing_options": {
            "auto": {
                "search_cutoff": 5000,
                "service_penalty": 0,
                "use_living_streets": 1
            }
        },
        "units": "miles"
    }

    # Use separators to ensure the most compact JSON for the C++ parser
    json_body = json.dumps(payload, separators=(',', ':'))

    try:
        resp = await client.post(
            "/sources_to_targets",
            content=json_body,
            headers={"Content-Type": "application/json"},
            timeout=60.0
        )

        if resp.status_code == 400:
            err_data = resp.json()
            if err_data.get("error_code") == 171 and radius < 500:
                print(f"⚠️  Snapping failed at {radius}m. Retrying with 500m radius...")
                return await get_matrix(client, sources, targets, radius=500)

            print(f"❌ Valhalla Error {err_data.get('error_code')}: {err_data.get('error')}")
            return None

        resp.raise_for_status()
        return resp.json()

    except Exception as e:
        print(f"❌ HTTP/Network Request failed: {e}")
        return None

def transform_to_matrix(json_data):
    raw_matrix = json_data.get("sources_to_targets", [])
    if not raw_matrix:
        return None

    df = pl.DataFrame([item for row in raw_matrix for item in row])

    dist_matrix_pl = (
        df.pivot(values="distance", index="from_index", on="to_index")
        .sort("from_index")
        .drop("from_index")
    )

    return dist_matrix_pl.to_numpy()


async def main():
    print(f"Starting Valhalla Testing Suite...")
    print(f"Testing with {len(LOCS)} sources and {len(LOCS)} targets (Rhode Island)")

    async with httpx.AsyncClient(base_url=BASE_URL) as client:
        try:
            health = await client.get("/status")
            if health.status_code == 200:
                print("Service is UP")
        except:  # lord forgive me for I have sinned
            print("Service is DOWN. Check your Docker container.")
            return

        start_time = time.perf_counter()

        result_json = await get_matrix(client, LOCS, LOCS)

        if result_json:
            matrix = transform_to_matrix(result_json)

            elapsed = time.perf_counter() - start_time
            print(f"\nSUCCESS: Matrix calculated in {elapsed:.4f}s")
            print(f"Matrix Shape: {matrix.shape}")
            print(f"Avg Distance: {np.mean(matrix):.2f} miles")
            print("-" * 30)
            print("First 3x3 subset of the matrix:")
            print(matrix[:3, :3])
        else:
            print("Test failed. See logs above for details.")

if __name__ == "__main__":
    asyncio.run(main())