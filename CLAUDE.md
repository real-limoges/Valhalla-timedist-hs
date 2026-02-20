# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Development Commands

```bash
# Build
cabal build all

# Test (with streaming output)
cabal test all --test-show-details=streaming

# Run locally (requires Valhalla running at VALHALLA_URL)
cabal run exe:valhalla-timedist-hs-exe

# Full stack with Docker (includes Valhalla routing engine)
docker-compose up

# Bootstrap Valhalla tile data (US region)
./runme.sh
```

**Environment variables:** `VALHALLA_URL` (default: `http://localhost:8002`), `PORT` (default: `9000`), `MAX_POINTS` (default: `10000`)

## Architecture

Servant-based Haskell microservice that wraps the [Valhalla](https://github.com/valhalla/valhalla) routing engine to calculate time and distance from a subject location to multiple target points.

**Request flow:**
1. `POST /v1/time_distance` receives subject location, target points, and costing models
2. Handler validates points (non-empty, within `maxPoints` limit)
3. For each costing model, `queryMatrix` is called concurrently via `mapConcurrently`
4. Each call hits Valhalla's `/sources_to_targets` endpoint
5. Results aggregated and returned as `[CostingResult]`

**Module layout:**

| Module | Role |
|--------|------|
| `Server` | Reads env vars, bootstraps Warp server with graceful shutdown |
| `API.Api` | Servant API type definitions (routes under `/v1/`) |
| `API.Handlers` | Request validation and orchestration |
| `Valhalla.Client` | HTTP client for Valhalla (`checkStatus`, `queryMatrix`) |
| `Types.Domain` | Core types: `Location`, `CostingModel`, newtypes (`Longitude`, `Latitude`, `Seconds`, `Meters`) |
| `Types.Config` | `AppConfig` (valhallaUrl, httpManager, maxPoints) |
| `Types.API` | User-facing types: `TimeDistanceRequest`, `CostingResult` |
| `Types.Valhalla` | Wire format types for Valhalla API |
| `Logging` | Structured JSON logging with request middleware |

**Key type safety patterns:**
- `Longitude`/`Latitude` newtypes prevent coordinate swapping
- `Seconds`/`Meters` newtypes prevent unit confusion
- `Location` JSON parsing validates lat ∈ [-90, 90], lon ∈ [-180, 180]
- `NonEmpty CostingModel` enforces at least one costing model per request

## Testing

27 tests across unit, integration (mock WAI server), and property-based (QuickCheck) tests. Test files are in `test/`. Run a specific test module by filtering:

```bash
cabal test all --test-show-details=streaming --test-option=--match --test-option="pattern"
```

## Project Configuration

- **GHC 9.12.2**, **Cabal 3.12**, language **GHC2024**
- CI via GitHub Actions (`.github/workflows/ci.yml`): builds, tests, and pushes Docker image to GHCR on main
- Docker multi-stage build in `Dockerfile.servant` (builder + debian-slim runtime)
- Detailed architecture and code review notes in `docs/`