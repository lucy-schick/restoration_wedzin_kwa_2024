# # This OGR command gives us a one-sided buffer based on a column in the attribute table of a line representing the high watermark (layer name `hwm`).
#
# ogr2ogr -update -overwrite \
# /Users/airvine/Projects/gis/restoration_wedzin_kwa/prescriptions_2025.gpkg \
# /Users/airvine/Projects/gis/restoration_wedzin_kwa/prescriptions_2025.gpkg \
# -dialect sqlite \
# -sql "SELECT ST_SingleSidedBuffer(geometry, -buffer_m, 0) AS geometry, * FROM hwm" \
# -nln hwm_buffered \
# -f "GPKG" \
# -nlt POLYGON


# For each row in table representing the hwm (line segment - layer name `hwm`in this case) - this OGR command gives
# one-sided buffer based on columns of the table representing the buffer size in metres and the direction of which to
# generate the buffer (not sure if this is based on a curve in the line segment or not - need to test).
ogr2ogr -update -overwrite \
/Users/airvine/Projects/gis/restoration_wedzin_kwa/prescriptions_2025.gpkg \
/Users/airvine/Projects/gis/restoration_wedzin_kwa/prescriptions_2025.gpkg \
-dialect sqlite \
-sql "SELECT ST_SingleSidedBuffer(geometry,
       CASE WHEN buffer_direction = 1 THEN buffer_m ELSE -buffer_m END, 0) AS geometry, *
       FROM hwm" \
-nln prescription_poly_raw \
-f "GPKG" \
-nlt POLYGON
