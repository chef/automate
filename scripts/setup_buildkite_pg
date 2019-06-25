#!/bin/bash
set -euo pipefail

DB_NAME=$1

sed -i 's|^local.*|local   all   all   trust|' /etc/postgresql/9.6/main/pg_hba.conf
sed -i 's|^host.*127.0.0.1.*|host   all   all   127.0.0.1/32   trust|' /etc/postgresql/9.6/main/pg_hba.conf

service postgresql start

psql -c "create database $DB_NAME;" -U postgres
psql -c 'create extension "uuid-ossp";' -d "postgresql://postgres@127.0.0.1:5432/$DB_NAME?sslmode=disable"
