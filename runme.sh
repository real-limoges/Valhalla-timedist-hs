#!/bin/zsh

set -euo pipefail

# Always resolve paths relative to this script's directory, regardless of where
# it is invoked from.
SCRIPT_DIR="${0:a:h}"

# ---------------------------------------------------------------------------
# verify_md5 <file> <md5file>
# Portable MD5 check: uses md5sum (Linux) or md5 (macOS).
# ---------------------------------------------------------------------------
verify_md5() {
    local file=$1 md5file=$2
    local expected actual
    expected=$(awk '{print $1}' "$md5file")
    if command -v md5sum > /dev/null 2>&1; then
        actual=$(md5sum "$file" | awk '{print $1}')
    else
        actual=$(md5 -q "$file")
    fi
    if [ "$expected" != "$actual" ]; then
        echo "Error: checksum mismatch for $file"
        echo "  Expected: $expected"
        echo "  Got:      $actual"
        return 1
    fi
    echo "Checksum verified: $file"
}

# ---------------------------------------------------------------------------
# Download OSM data
# ---------------------------------------------------------------------------
if [ ! -f "$SCRIPT_DIR/data/us-latest.osm.pbf" ]; then
    echo "Downloading US-wide OSM data from Geofabrik..."
    curl -L -o "$SCRIPT_DIR/data/us-latest.osm.pbf" https://download.geofabrik.de/north-america/us-latest.osm.pbf
    curl -L -o "$SCRIPT_DIR/data/us-latest.osm.pbf.md5" https://download.geofabrik.de/north-america/us-latest.osm.pbf.md5
    verify_md5 "$SCRIPT_DIR/data/us-latest.osm.pbf" "$SCRIPT_DIR/data/us-latest.osm.pbf.md5"
fi

# ---------------------------------------------------------------------------
# Generate Valhalla config
# ---------------------------------------------------------------------------
docker run --rm -v "$SCRIPT_DIR/data:/data" ghcr.io/valhalla/valhalla:3.6.1 \
  valhalla_build_config \
  --mjolnir-tile-dir /valhalla_tiles \
  --mjolnir-tile-extract /valhalla_tiles.tar \
  --mjolnir-admin /data/admin.sqlite | \
  jq '.httpd.service.listen = "tcp://*:8002" |
      .httpd.service.interrupt = "ipc:///tmp/interrupt" |
      .httpd.service.loopback = "ipc:///tmp/loopback" |
      .httpd.service.timeout_seconds = 30 |
      .mjolnir.max_locations = 10000' \
  > "$SCRIPT_DIR/config/valhalla.json"

# ---------------------------------------------------------------------------
# Build tile data
# ---------------------------------------------------------------------------
echo "Building Admin Database..."
docker run --rm \
  -v "$SCRIPT_DIR/data:/data" \
  -v "$SCRIPT_DIR/config:/config" \
  ghcr.io/valhalla/valhalla:3.6.1 \
  valhalla_build_admins --config /config/valhalla.json /data/us-latest.osm.pbf

echo "🗺️  Building US Tiles..."
docker run --rm \
  -v "$SCRIPT_DIR/data:/data" \
  -v "$SCRIPT_DIR/config:/config" \
  -v "$SCRIPT_DIR/data/valhalla_tiles:/valhalla_tiles" \
  ghcr.io/valhalla/valhalla:3.6.1 \
  valhalla_build_tiles --config /config/valhalla.json /data/us-latest.osm.pbf
