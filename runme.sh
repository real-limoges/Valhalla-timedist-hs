#!/bin/zsh


if [ ! -f data/us-latest.osm.pbf ]; then
    echo "Downloading US-wide OSM data from Geofabrik..."
    curl -L -o data/us-latest.osm.pbf https://download.geofabrik.de/north-america/us-latest.osm.pbf
fi


docker run --rm -v $(pwd)/data:/data ghcr.io/valhalla/valhalla:3.6.1 \
  valhalla_build_config \
  --mjolnir-tile-dir /valhalla_tiles \
  --mjolnir-tile-extract /valhalla_tiles.tar \
  --mjolnir-admin /data/admin.sqlite | \
  jq '.httpd.service.listen = "tcp://*:8002" |
      .httpd.service.interrupt = "ipc:///tmp/interrupt" |
      .httpd.service.loopback = "ipc:///tmp/loopback" |
      .mjolnir.max_locations = 10000' \
  > config/valhalla.json


echo "Building Admin Database..."
docker run --rm -v $(pwd)/data:/data -v $(pwd)/config:/config \
  ghcr.io/valhalla/valhalla:3.6.1 \
  valhalla_build_admins --config /config/valhalla.json /data/us-latest.osm.pbf


echo "🗺️  Building US Tiles ..."
docker run --rm \
  -v $(pwd)/data:/data \
  -v $(pwd)/config:/config \
  -v $(pwd)/data/valhalla_tiles:/valhalla_tiles \
  ghcr.io/valhalla/valhalla:3.6.1 \
  valhalla_build_tiles --config /config/valhalla.json /data/us-latest.osm.pbf