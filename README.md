# valhalla-timedist-hs

A Servant-based Haskell microservice that wraps the Valhalla routing engine to calculate time and distance from a subject location to multiple target points.

## API

### `GET /health`
Health check endpoint.

### `POST /time_distance`
Calculate time/distance from a subject location to target points.

**Request:**
```json
{
  "subject": {"lon": -122.4, "lat": 37.8},
  "points": [
    {"lon": -122.5, "lat": 37.9},
    {"lon": -122.6, "lat": 38.0}
  ],
  "costing": ["auto", "bicycle"]
}
```

**Response:**
```json
[
  {
    "costing": "auto",
    "results": [
      {"time": 300.5, "distance": 1000.0},
      {"time": 450.2, "distance": 1500.0}
    ]
  },
  {
    "costing": "bicycle",
    "results": [
      {"time": 900.0, "distance": 1000.0},
      {"time": 1200.0, "distance": 1500.0}
    ]
  }
]
```

**Costing models:** `auto`, `bicycle`, `pedestrian`, `truck`

**Limits:** Max 10,000 points, 30 second timeout per costing model.

## Configuration

Environment variables:
- `VALHALLA_URL` - Valhalla service URL (default: `http://localhost:8002`)
- `PORT` - Server port (default: `9000`)

## Running

```bash
# With nix
nix develop
cabal run exe:valhalla-timedist-hs-exe

# With docker-compose (includes Valhalla)
docker-compose up
```
