#!/bin/zsh

# 1. Clean up old failed attempts
rm -rf data/valhalla_tiles/* 2>/dev/null

# 2. Ensure we have the data
if [ ! -f data/rhode-island.osm.pbf ]; then
    echo "Downloading Rhode Island OSM data..."
    curl -L -o data/rhode-island.osm.pbf https://download.geofabrik.de/north-america/us/rhode-island-latest.osm.pbf
fi

# 3. Build Config (Using quotes to avoid Zsh globbing error)
docker run --rm -v $(pwd)/data:/data ghcr.io/valhalla/valhalla:3.6.1 \
  valhalla_build_config \
  --mjolnir-tile-dir /valhalla_tiles \
  --mjolnir-tile-extract /valhalla_tiles.tar \
  --mjolnir-admin /data/admin.sqlite | \
  jq '.httpd.service.listen = "tcp://*:8002" |
      .httpd.service.interrupt = "ipc:///tmp/interrupt" |
      .httpd.service.loopback = "ipc:///tmp/loopback"' \
  > config/valhalla.json

# 4. Build Tiles (This will fail immediately if PBF is corrupt)
docker run --rm \
  -v $(pwd)/data:/data \
  -v $(pwd)/config:/config \
  -v $(pwd)/data/valhalla_tiles:/valhalla_tiles \
  ghcr.io/valhalla/valhalla:3.6.1 \
  valhalla_build_tiles --config /config/valhalla.json /data/rhode-island.osm.pbf