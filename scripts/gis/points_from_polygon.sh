#!/bin/bash

# Check for correct number of arguments
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <input_gpkg> <input_layer> <output_layer>"
    exit 1
fi

# Input variables
GPKG_IN="$1"     # Input GPKG
LAYER_IN="$2"    # Input polygon layer
LAYER_OUT="$3"   # Output point layer

# Temporary files
RASTER_FILE="/tmp/density.tif"
XYZ_FILE="/tmp/density_points.xyz"

# Verify the input layer exists
if ! ogrinfo "$GPKG_IN" "$LAYER_IN" &>/dev/null; then
    echo "ERROR: Layer '$LAYER_IN' not found in '$GPKG_IN'."
    exit 1
fi

# 1. Rasterize polygon density to a temporary GeoTIFF file
# Ensure density_points is treated as a numeric value
gdal_rasterize -a density_points -tr 1.0 1.0 -ot Float32 -a_nodata 0 \
    -l "$LAYER_IN" "$GPKG_IN" "$RASTER_FILE"

# 2. Convert raster to XYZ and write to a real temp file
gdal_translate -of XYZ "$RASTER_FILE" /vsistdout/ | awk '$3 > 0 {print $1, $2}' > "$XYZ_FILE"

# Verify if the XYZ file was created successfully
if [ ! -s "$XYZ_FILE" ]; then
    echo "ERROR: XYZ file generation failed."
    exit 1
fi

# 3. Convert XYZ to a point layer in the same GPKG
ogr2ogr -f "GPKG" "$GPKG_IN" "$XYZ_FILE" \
    -nln "$LAYER_OUT" -overwrite -dialect sqlite \
    -sql "SELECT MakePoint(CAST(f1 AS REAL), CAST(f2 AS REAL), 4326) AS geometry FROM density_points"

# 4. Cleanup
rm "$RASTER_FILE" "$XYZ_FILE"

