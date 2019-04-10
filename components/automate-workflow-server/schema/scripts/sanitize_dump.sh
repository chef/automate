#!/bin/sh
#
# Use this to slim down and sanitize an SHD database dump for use in
# pgTAP tests.
set -x

IN_DUMP=delivery_db.dump
OUT_DUMP=sanitized_shd.dump
DBNAME=delivery_test

# Clear out anything there
psql --dbname ${DBNAME} --command 'delete from enterprises cascade; delete from stage_ordering;'

# Slurp in the entire dump
pg_restore --create --dbname=${DBNAME} --data-only --disable-triggers --schema=public ${IN_DUMP}

# Clean up some stuff
psql --dbname=${DBNAME} --file sanitize.sql

# Export it again
pg_dump --format=custom ${DBNAME} --file=${OUT_DUMP}
