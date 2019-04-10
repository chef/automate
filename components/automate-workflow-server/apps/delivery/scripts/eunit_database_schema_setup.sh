#!/bin/bash

# Use the schema's existing makefile to create an Eunit-specific
# testing database. We'll just lay down the schema; no test data is
# populated at this point.
#
# Set the EUNIT_DROP_DATABASE environment variable if you wish to drop
# the database before deploying schema.

set -x
EUNIT_DATABASE=delivery_eunit

function is_postgres_running() {
    ps auxww | grep [p]ostgres
}

if [ -z "$PGHOST" ]; then
  if [ -n "$(is_postgres_running)" ]; then
      echo "postgres is running"
      # Use Git to tell us the concrete directory we need
      cd $(git rev-parse --show-toplevel)/server/schema

      if [ -n "$EUNIT_DROP_DATABASE" ];
      then
          TEST_DB=${EUNIT_DATABASE} make recreate_database
      fi

      TEST_DB=${EUNIT_DATABASE} make core_extensions setup_schema
  else
      echo "Postgres is NOT running!"
      exit 1
  fi
fi
