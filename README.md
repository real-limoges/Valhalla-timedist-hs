# valhalla-timedist-hs

A Servant-based Haskell microservice that wraps the [Valhalla](https://github.com/valhalla/valhalla) routing engine to calculate time and distance from a subject location to multiple target points using one or more costing models.

Given a single origin ("subject") and a list of destinations ("points"), the service calls Valhalla's [sources-to-targets matrix API](https://valhalla.github.io/valhalla/api/matrix/api-reference/) for each requested costing model concurrently, and returns the travel time (in seconds) and distance (in meters) for every origin-destination pair.

## API

All endpoints are served under the `/v1/` prefix.

### `GET /v1/health`

Health check endpoint. Pings Valhalla's `/status` endpoint and returns `"OK"` if reachable, or `"OK (Valhalla unreachable)"` if not. This endpoint always returns 200 -- it reports Valhalla availability but does not gate on it, so orchestrators can distinguish between "service is up" and "routing engine is reachable".

### `POST /v1/time_distance`

Calculate time and distance from a single subject location to a list of target points, evaluated concurrently across one or more costing models.

**Request:**

```json
{
  "subject": { "lon": -122.4194, "lat": 37.7749 },
  "points": [
    { "lon": -122.4089, "lat": 37.7836 },
    { "lon": -122.4528, "lat": 37.7694 }
  ],
  "costing": ["auto", "bicycle"]
}
```

| Field     | Type                | Description                                       |
|-----------|---------------------|---------------------------------------------------|
| `subject` | `Location`          | Origin point. `lon` must be in [-180, 180], `lat` must be in [-90, 90]. Coordinates outside these ranges are rejected during JSON parsing. |
| `points`  | `[Location]`        | Target points. Must contain at least 1 and at most `MAX_POINTS` (default 10,000) locations. Each location is validated with the same coordinate bounds as `subject`. |
| `costing` | `[CostingModel]`    | One or more costing models to evaluate. Must be a non-empty array containing any of: `auto`, `bicycle`, `pedestrian`, `truck`. Each model produces an independent set of results. |

**Costing models:**

| Model        | Description                                                        |
|--------------|--------------------------------------------------------------------|
| `auto`       | Standard automobile routing, accounting for turn costs and road hierarchy |
| `bicycle`    | Bicycle routing, preferring bike lanes and low-traffic roads       |
| `pedestrian` | Walking routes, including paths and footways not available to vehicles |
| `truck`      | Commercial truck routing with height/weight restrictions           |

**Response:**

```json
[
  {
    "costing": "auto",
    "results": [
      { "time": 245.3, "distance": 1832.0 },
      { "time": 412.7, "distance": 3150.5 }
    ]
  },
  {
    "costing": "bicycle",
    "results": [
      { "time": 580.0, "distance": 1832.0 },
      null
    ]
  }
]
```

The response is a JSON array with one entry per requested costing model, in the same order as the request's `costing` array.

Each entry in `results` corresponds positionally to an entry in the request `points` array. A result is `null` if Valhalla could not find a route for that target (e.g., the point is on a disconnected island or in water).

| Field      | Type                     | Description                              |
|------------|--------------------------|------------------------------------------|
| `costing`  | `string`                 | The costing model used                   |
| `results`  | `[TargetResult \| null]` | One entry per target point               |

Each `TargetResult` contains:

| Field      | Type     | Description                                   |
|------------|----------|-----------------------------------------------|
| `time`     | `number` | Travel time from subject to target, in seconds |
| `distance` | `number` | Travel distance from subject to target, in meters |

**Error responses** are JSON objects with an `error` field and an appropriate HTTP status code:

```json
{ "error": "Too many points: 15000 exceeds limit of 10000" }
```

| Status | Condition                                                          |
|--------|--------------------------------------------------------------------|
| 400    | Empty `points` array                                               |
| 400    | Number of points exceeds `MAX_POINTS`                              |
| 400    | Invalid JSON, missing required fields, or out-of-range coordinates |
| 500    | Valhalla routing engine returned an error for a costing model      |

**Example with `curl`:**

```bash
curl -X POST http://localhost:9000/v1/time_distance \
  -H "Content-Type: application/json" \
  -d '{
    "subject": { "lon": -73.9857, "lat": 40.7484 },
    "points": [
      { "lon": -73.9680, "lat": 40.7614 },
      { "lon": -74.0060, "lat": 40.7128 }
    ],
    "costing": ["auto", "pedestrian"]
  }'
```

## Configuration

All configuration is done via environment variables. The server logs its resolved configuration at startup.

| Variable       | Default                   | Description                                              |
|----------------|---------------------------|----------------------------------------------------------|
| `VALHALLA_URL` | `http://localhost:8002`   | Base URL of the Valhalla routing engine                  |
| `PORT`         | `9000`                    | Port the microservice listens on                         |
| `MAX_POINTS`   | `10000`                   | Maximum number of target points allowed per request      |

If `PORT` is set to an invalid (non-numeric) value, the server logs a warning and falls back to the default.

## Getting Started

### Prerequisites

- [GHC 9.12.2](https://www.haskell.org/ghc/) and [Cabal 3.12](https://www.haskell.org/cabal/)
- [Docker](https://www.docker.com/) and [Docker Compose](https://docs.docker.com/compose/) for running with Valhalla
- A running Valhalla instance with routing tiles built

### Building

```bash
cabal build all
```

### Running Tests

```bash
cabal test all --test-show-details=streaming
```

The test suite includes:

- **Unit tests** -- JSON serialization round-trips, coordinate validation, costing model parsing, and edge cases (empty arrays, out-of-range values, malformed JSON)
- **Property-based tests** -- QuickCheck generators verify invariants like coordinate bound validation and JSON round-trip consistency across random inputs
- **Integration tests** -- Full HTTP request/response tests against a mock Valhalla server using `hspec-wai`, exercising the real Servant routing and handler logic without a live Valhalla instance

To run tests matching a specific pattern:

```bash
cabal test all --test-show-details=streaming --test-option=--match --test-option="time_distance"
```

### Running Locally

**Option 1: With Cabal (standalone)**

```bash
cabal run exe:valhalla-timedist-hs-exe
```

This requires a Valhalla instance already running at `VALHALLA_URL` (defaults to `http://localhost:8002`). The server will start even if Valhalla is unreachable, logging a warning -- this allows the service to come up before Valhalla finishes loading tiles.

**Option 2: With Docker Compose (full stack)**

This starts both the Valhalla routing engine and the microservice:

```bash
docker-compose up
```

The microservice will be available at `http://localhost:9000`. The compose setup includes:

- **Health checks** -- Valhalla has a health check with a 60-second startup grace period (tile loading takes time). The microservice waits for Valhalla to be healthy before starting.
- **Resource limits** -- Valhalla is capped at 8 GB memory and 4 CPUs; the microservice at 1 GB and 2 CPUs.
- **Security hardening** -- Both containers run with read-only filesystems and `no-new-privileges`.
- **Log rotation** -- JSON file logging with 50 MB max size and 5 file rotation.

To use a specific image tag instead of `latest`:

```bash
IMAGE_TAG=abc1234 docker-compose up
```

### Bootstrapping Valhalla Tile Data

Before running with Docker Compose, you need to build the Valhalla routing tiles. The `runme.sh` script automates this for the US region:

```bash
./runme.sh
```

This will:
1. Download the US OSM extract from Geofabrik (~10 GB) with MD5 verification
2. Generate a Valhalla configuration file via Docker (with 30-second timeout and 10,000 max locations)
3. Build the admin database (`data/admin.sqlite`)
4. Build the routing tiles (`data/valhalla_tiles/`)

The resulting data is stored in `data/` and `config/` and is mounted read-only into the Docker containers. This process only needs to be run once (or when you want to update the map data).

## Architecture

```
                        ┌─────────────────────┐
                        │       Client        │
                        └─────────┬───────────┘
                                  │
                          POST /v1/time_distance
                                  │
                                  v
                      ┌───────────────────────┐
                      │    API.Handlers       │
                      │  - validate request   │
                      │  - check point count  │
                      └───────────┬───────────┘
                                  │
                         mapConcurrently
                    ┌─────────┼─────────┐
                    v         v         v
              ┌──────────┬──────────┬──────────┐
              │  auto    │ bicycle  │  truck   │
              └────┬─────┴────┬─────┴────┬─────┘
                   v          v          v
              ┌───────────────────────────────┐
              │       Valhalla.Client         │
              │  POST /sources_to_targets     │
              └───────────────┬───────────────┘
                              v
              ┌───────────────────────────────┐
              │   Valhalla routing engine     │
              │      (external service)       │
              └───────────────────────────────┘
```

### Module Overview

| Module             | Responsibility                                                       |
|--------------------|----------------------------------------------------------------------|
| `Server`           | Entry point. Reads environment variables, creates the HTTP manager, validates Valhalla connectivity, and starts Warp with graceful shutdown. |
| `API.Api`          | Servant API type definitions. Defines the `/v1/health` and `/v1/time_distance` routes and wires them to handlers. |
| `API.Handlers`     | Request validation and orchestration. Validates point counts, fans out concurrent Valhalla queries per costing model, and aggregates results. |
| `Valhalla.Client`  | HTTP client for the Valhalla routing engine. Provides `checkStatus` (health ping) and `queryMatrix` (sources-to-targets matrix query). |
| `Types.Domain`     | Core domain types with validation. `Location`, `CostingModel`, and newtypes for `Longitude`, `Latitude`, `Seconds`, `Meters`, `ValhallaUrl`. |
| `Types.API`        | User-facing request/response types: `TimeDistanceRequest` and `CostingResult`. |
| `Types.Valhalla`   | Wire-format types for the Valhalla matrix API: `MatrixRequest`, `MatrixResponse`, `TargetResult`. |
| `Types`            | Re-export hub that consolidates all type modules for convenient imports. |
| `Logging`          | Structured JSON logging (`logInfo`, `logWarn`, `logError`) and a WAI middleware that logs every request with method, path, status, and duration in milliseconds. |

### Key Design Decisions

- **Concurrent costing model evaluation** -- Each costing model is queried in parallel via `mapConcurrently`. A request with `["auto", "bicycle", "truck"]` makes 3 concurrent calls to Valhalla, bounded only by Valhalla's capacity.
- **Type-safe domain modeling** -- Newtypes (`Longitude`, `Latitude`, `Seconds`, `Meters`) prevent unit and coordinate mixups at compile time. `Location`'s `FromJSON` instance validates coordinate bounds, so invalid data is rejected before it reaches any handler logic.
- **Structured JSON logging** -- All log output is structured JSON with timestamps, log levels, and contextual key-value fields. The WAI request logging middleware captures method, path, HTTP status, and response duration for every request.
- **Graceful shutdown** -- Warp is configured with a 30-second graceful shutdown timeout, allowing in-flight requests to complete before the process exits.
- **Non-blocking startup** -- The server checks Valhalla reachability at startup and logs a warning if unreachable, but does not block startup. This allows the service to come up in orchestration environments where Valhalla may still be loading tiles.
- **Opaque error responses** -- Internal errors (Valhalla exceptions) are logged with full detail server-side but returned to clients as generic JSON error messages, avoiding information leakage.

## Deployment

CI runs on GitHub Actions (`.github/workflows/ci.yml`):

1. **On every push and PR to `main`:** builds with GHC 9.12.2, runs the full test suite, caches the cabal store and `dist-newstyle` for faster subsequent builds.
2. **On push to `main` only:** builds a Docker image and pushes to GHCR with both a short-SHA tag and `latest`:

```
ghcr.io/real-limoges/valhalla-timedist-hs:<short-sha>
ghcr.io/real-limoges/valhalla-timedist-hs:latest
```

The Docker build (`Dockerfile.servant`) uses a multi-stage approach:

- **Builder stage** -- `haskell:9.12.2` base image with Docker BuildKit cache mounts for the cabal package database, store, and `dist-newstyle`. Dependencies are built in a separate layer for caching.
- **Runtime stage** -- `debian:bookworm-slim` with only the minimal runtime dependencies (`libgmp10`, `ca-certificates`, `libffi8`, `netbase`). The binary runs as a non-root `appuser` on a read-only filesystem, exposing port 9000.

## License

BSD-3-Clause